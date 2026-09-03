## =====================================================================
##  data-raw/make_no_pooled_ltbl.R   (one-time; committed for audit)
##  Build the newest Norwegian POOLED life table from the frozen SSB JSON
##  and write an .rds that mirrors tb_pool_ons_2022_2024 exactly, so the
##  app container ingests it with no special-casing.
##
##  Pooling is written out EXPLICITLY (survivor-weighted one-year q,
##  male_share weights) rather than hidden in a call, and is CROSS-CHECKED
##  against the stored ONS pooled object so both jurisdictions are pooled
##  by the identical convention. Run once; re-run when the freeze updates.
## =====================================================================
# needs: jsonlite ; library(ltbl.mb) for as.ltbl(), male_share(), mrqs()
# assumes the renamed reader is available (source ons_lifetable.R first).

library(ltbl.mb)
source("C:/Users/mathb/repos/ltbl.mb/data-raw/json_lifetable.R")          # close_ltbl + read_lifetable_json + json_periods

## ---- parameters -----------------------------------------------------
FREEZE       <- "C:/Users/mathb/repos/ltbl.mb/data-raw/ssb07902_lifetables.json"
BIRTH_RATIO  <- 1.06        # NO males:females at birth (SSB ~1.06). Used for BOTH
# this table AND the later norm pooling -- keep consistent.
OUT_RDS      <- NULL        # NULL -> auto-name tb_pool_ssb_<period>.rds in data-raw/
ONS_JSON     <- "C:/Users/mathb/repos/ltbl.mb/data-raw/nlte198020223.json"   # for the convention cross-check
ONS_POOLED   <- "../PMD_app/pds_data/tb_pool_ons_2022_2024.rds"      # path to your stored tb_pool_ons_2022_2024.rds, or NULL to skip

## ---- explicit survivor-weighted pooler ------------------------------
## Pooled one-year death probability at age x is the share-weighted mean
## of the sex-specific q_x, weighting by P(sex | alive at x):
##   q_pooled(x) = s(x) q_male(x) + (1 - s(x)) q_female(x),  s = male_share.
## This is the demographically exact pooled q for a survivor mix; the
## "survivor-weighted hazard (male_share weights)" label names precisely this.
pool_ltbl_explicit <- function(tb_m, tb_f, birth_ratio) {
  stopifnot(identical(tb_m$a, tb_f$a))               # both closed to 0:120 by the reader
  s  <- male_share(tb_m, tb_f, birth_ratio = birth_ratio)   # function(age) -> P(male|alive)
  a  <- tb_m$a
  wm <- s(a)
  mu <- wm * tb_m$mu + (1 - wm) * tb_f$mu
  as.ltbl(data.frame(a = a, mu = mu))
}

## ---- pick newest period, read both sexes, pool ----------------------
periods <- json_periods(FREEZE)
period  <- as.character(max(suppressWarnings(as.integer(periods)), na.rm = TRUE))
message("newest SSB period: ", period)

tb_m <- read_lifetable_json(FREEZE, period, "male")
tb_f <- read_lifetable_json(FREEZE, period, "female")

tb_no <- pool_ltbl_explicit(tb_m, tb_f, BIRTH_RATIO)

## ---- stamp to match the ONS pooled object's attribute set -----------
frozen_date <- attr(tb_m, "read_date")
attr(tb_no, "region")     <- "Norway"
attr(tb_no, "year")       <- period
attr(tb_no, "sex")        <- "pooled"
attr(tb_no, "birth_ratio")<- BIRTH_RATIO
attr(tb_no, "pooling")    <- "survivor-weighted hazard (male_share weights)"
attr(tb_no, "source")     <- sprintf(
  "SSB 07902 (Dodssannsynlighet) via %s, frozen %s (%s, pooled qx)",
  basename(FREEZE), frozen_date %||% "NA", period)

## ---- sanity checks (fail loudly before writing) ---------------------
`%||%` <- function(x, y) if (is.null(x)) y else x
stopifnot(
  inherits(tb_no, "ltbl"),
  identical(tb_no$a, 0:120),
  all(tb_no$mu >= 0 & tb_no$mu <= 1),
  isTRUE(tb_no$mu[tb_no$a == 120] == 1),
  # pooled mu must sit between the sexes at every age (share is in [0,1])
  all(tb_no$mu >= pmin(tb_m$mu, tb_f$mu) - 1e-12 &
        tb_no$mu <= pmax(tb_m$mu, tb_f$mu) + 1e-12)
)
# pooled life expectancy is finite and between the sexes at birth
e0_m <- mrqs(0, tb_m, hrqol = 1, r = 0)
e0_f <- mrqs(0, tb_f, hrqol = 1, r = 0)
e0_p <- mrqs(0, tb_no, hrqol = 1, r = 0)
message(sprintf("e0  male %.3f  female %.3f  pooled %.3f", e0_m, e0_f, e0_p))
stopifnot(e0_p > min(e0_m, e0_f) - 1e-6, e0_p < max(e0_m, e0_f) + 1e-6)

## ---- CONVENTION CROSS-CHECK against the stored ONS pooled object -----
## Re-pool the ONS sexes with the SAME explicit method and confirm it
## reproduces your committed tb_pool_ons_2022_2024$mu. If TRUE, Norway is
## pooled by the identical convention to England. If FALSE, the ONS object
## was built on a different scale -> switch both to that method.
if (!is.null(ONS_POOLED) && file.exists(ONS_POOLED) && file.exists(ONS_JSON)) {
  ons_m <- read_lifetable_json(ONS_JSON, "2022-2024", "male")
  ons_f <- read_lifetable_json(ONS_JSON, "2022-2024", "female")
  ons_repool <- pool_ltbl_explicit(ons_m, ons_f, birth_ratio = 1.051)
  ons_stored <- readRDS(ONS_POOLED)
  cc <- all.equal(ons_repool$mu, ons_stored$mu, tolerance = 1e-8)
  if (isTRUE(cc)) {
    message("CONVENTION CHECK: PASS -- explicit pooling reproduces ONS. ",
            "Norway pooled by the identical convention.")
  } else {
    warning("CONVENTION CHECK: MISMATCH -- explicit q-mix != stored ONS pooled mu.\n",
            "  ", paste(cc, collapse = "\n  "), "\n",
            "  The ONS object was built on a different scale (likely continuous\n",
            "  hazard). Do NOT freeze Norway until both use the same pooler.")
  }
} else {
  message("CONVENTION CHECK: skipped (set ONS_POOLED to your tb_pool_ons_2022_2024.rds ",
          "to verify identical pooling before freezing).")
}

## ---- write ----------------------------------------------------------
out <- OUT_RDS %||% file.path("pds_data", sprintf("tb_pool_ssb_%s.rds", period))
saveRDS(tb_no, out)
message("wrote ", out)
print(utils::str(attributes(tb_no)))

## Suggested container key for pds_data (shared by table AND norm, as with
## the ONS pair's 'ons_pool_2022_2024'):  no_pool_<period>  e.g. no_pool_2024
message("\nsuggested pds_data key: no_pool_", period,
        "\nhand me this file's attributes() and I will add_ltbl() it and set ",
        "norway_2020's reference_hint.")


## =====================================================================
##  make_no_pooled_norm.R   (one-time; committed for audit)
##  Twin of make_no_pooled_ltbl.R, for the HRQoL norm. Builds the pooled
##  Norwegian norm from the Garratt MALE/FEMALE primitives -- NEVER the
##  'total' series (author-pooled, inert per the norms doctrine) -- using
##  the SAME male_share and BIRTH_RATIO as the life table, so the norm and
##  the mortality are pooled on one consistent survivor sex-mix.
##
##  Output mirrors qn_pool_ons_2022_2024: columns (age, hrqol), attributes
##  region/year/source/valueset/sex/birth_ratio/pooling.
## =====================================================================
# needs: library(ltbl.mb) for read_norm(), male_share(); life table built already.

"C:/Users/mathb/repos/ltbl.mb/data-raw/ssb07902_lifetables.json"
## ---- parameters (keep BIRTH_RATIO identical to the life table run) ---
BIRTH_RATIO <- 1.06
# FREEZE      <- "pds_data/ssb07902_lifetables.json"   # as above
# PERIOD      <- "2025"                                 # match the pooled life table's period
TB_POOLED   <- "pds_data/tb_pool_ssb_2025.rds"        # the table built above
OUT_RDS     <- NULL                                   # NULL -> qn_pool_no_<period>.rds in pds_data/
source("C:/Users/mathb/repos/ltbl.mb/data-raw/json_lifetable.R")                    # read_lifetable_json (adjust path if needed)

## ---- read the SEX-SPECIFIC Garratt norms (long: age, hrqol) ----------
## read_norm resolves by id; these are the primitives, NOT 'total'/'wide'.
gm <- read_norm(id = "C:/Users/mathb/repos/ltbl.mb/inst/extdata/norms/hrqol_norway_2019_garratt_male.rds")
gf <- read_norm(id = "C:/Users/mathb/repos/ltbl.mb/inst/extdata/norms/hrqol_norway_2019_garratt_female.rds")
stopifnot(identical(gm$age, gf$age))                 # common 0..105 grid

## ---- male_share from the SAME sex-specific SSB tables ----------------
## The norm must be pooled on the survivor sex-mix, so male_share is built
## from the Norwegian male/female life tables (read from the freeze), with
## the SAME birth_ratio as the life table pooling.
tb_m <- read_lifetable_json(FREEZE, period, "male")
tb_f <- read_lifetable_json(FREEZE, period, "female")
s    <- male_share(tb_m, tb_f, birth_ratio = BIRTH_RATIO)   # function(age) -> P(male|alive)

## ---- pool the norm survivor-weighted, per age -----------------------
##   hrqol_pooled(a) = s(a) hrqol_male(a) + (1 - s(a)) hrqol_female(a)
## Norm grid tops at 105; male_share is defined on the life-table grid
## (0:120). Evaluate s on the norm's ages.
age <- gm$age
wm  <- s(age)
hrqol <- wm * gm$hrqol + (1 - wm) * gf$hrqol
nm_no <- data.frame(age = age, hrqol = hrqol)

## ---- stamp to match qn_pool_ons_2022_2024 ---------------------------
vs <- attr(gm, "valueset") %||% attr(gf, "valueset")
src <- attr(gm, "source")  %||% attr(gf, "source")
attr(nm_no, "region")      <- "Norway"
attr(nm_no, "year")        <- attr(gm, "year") %||% 2019L      # Garratt vintage, not the table's
attr(nm_no, "source")      <- src
attr(nm_no, "valueset")    <- vs
attr(nm_no, "sex")         <- "pooled"
attr(nm_no, "birth_ratio") <- BIRTH_RATIO
attr(nm_no, "pooling")     <- "survivor sex-mix (external share weights)"

## ---- sanity checks --------------------------------------------------
stopifnot(
  all(is.finite(nm_no$hrqol)),
  # pooled hrqol sits between the sexes at every age (share in [0,1])
  all(nm_no$hrqol >= pmin(gm$hrqol, gf$hrqol) - 1e-12 &
        nm_no$hrqol <= pmax(gm$hrqol, gf$hrqol) + 1e-12),
  # native Norwegian 5L tariff floor is -0.453; nothing below it
  all(nm_no$hrqol >= -0.453 - 1e-9)
)
message(sprintf("hrqol at age 40: male %.3f  female %.3f  pooled %.3f",
                gm$hrqol[gm$age == 40], gf$hrqol[gf$age == 40],
                nm_no$hrqol[nm_no$age == 40]))

## ---- write ----------------------------------------------------------
out <- OUT_RDS %||% file.path("pds_data", sprintf("qn_pool_no_%s.rds", period))
saveRDS(nm_no, out)
message("wrote ", out)
print(utils::str(attributes(nm_no)))

message("\nsuggested pds_data key (shared with the table): no_pool_", period,
        "\nhand me this file's attributes() and I will add_norm() it and set ",
        "norway_2020's reference_hint = list(table='no_pool_", period,
        "', norm='no_pool_", period, "').")


