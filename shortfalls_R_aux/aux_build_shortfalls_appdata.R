## =====================================================================
## aux_build_shortfalls_appdata.R  -- assemble shortfalls_data/shortfalls_data.rds
## ---------------------------------------------------------------------
## Standalone, offline, run MANUALLY when a jurisdiction's numbers change.
## Committed for audit: the auditable chain from published figures to the
## container. The app never runs this; it only READS the result.
##
## Layout assumed (run from the app home, i.e. the folder holding these):
##   ./shortfalls_R_aux/aux_sevset.R        core machinery (constructor, validator)
##   ./shortfalls_R_aux/aux_sevset_ext.R    container + lifecycle + setters
##   ./shortfalls_data_aux/*.rds            pooled intermediates (ONS + Norway)
##   ./shortfalls_data/shortfalls_data.rds         OUTPUT (the one shipped artefact)
##
## Assembles:
##   sf_thresholds : nice_2022 (real), norway_2020 (real), nl_2015 (PROVISIONAL)
##   hrqol_norms   : ons_pool_2022_2024, no_pool_2025
##   life_tables   : ons_pool_2022_2024, no_pool_2025
##
## Run:  Rscript shortfalls_R_aux/aux_build_thresholds.R   (from the app home)
##   or  source("shortfalls_R_aux/aux_build_thresholds.R") with getwd() = app home.
## All strings are ASCII (no em-dash / section sign): the deploy locale is
## not ours to assume, and a C locale mangles non-ASCII.
## =====================================================================

## --- locate machinery + data relative to the app home ----------------
## Prefer an explicit APP_HOME; else derive from --file=; else getwd().
APP_HOME <- Sys.getenv("SHORTFALLS_APP_HOME", "")
if (!nzchar(APP_HOME)) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  APP_HOME <- if (length(f) && nzchar(f) && !is.na(f)) dirname(dirname(f)) else "."
}
AUX  <- file.path(APP_HOME, "shortfalls_R_aux")     # machinery
DIN  <- file.path(APP_HOME, "shortfalls_data_aux")  # pooled intermediates (inputs)
DOUT <- file.path(APP_HOME, "shortfalls_data")      # container (output)

SEVSET_NO_SELFTEST <- TRUE                    # silence the core self-test
source(file.path(AUX, "aux_sevset.R"))
source(file.path(AUX, "aux_sevset_ext.R"))

git_sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                    error = function(e) NA_character_)

## helper: read a pooled intermediate and stamp a display label ---------
read_stamped <- function(file, label) {
  obj <- readRDS(file.path(DIN, file))
  attr(obj, "label") <- label
  obj
}

## =====================================================================
## 1. reference objects (pooled intermediates), labelled at read time
## =====================================================================
tb    <- read_stamped("tb_pool_ons_2022_2024.rds", "England - ONS 2022-2024 (pooled)")
nm    <- read_stamped("qn_pool_ons_2022_2024.rds", "England - McNamara 2018 (pooled, 3L)")
tb_no <- read_stamped("tb_pool_ssb_2025.rds",      "Norway - SSB 2025 (pooled)")
nm_no <- read_stamped("qn_pool_no_2025.rds",       "Norway - Garratt 2019 (pooled, 5L)")

## sex-specific tables. Read straight from the JSON freezes -- no pooling,
## so no survivor weighting to record: these ARE the published series.
tb_ons_m <- read_stamped("tb_male_ons_2022_2024.rds",   "England - ONS 2022-2024 (male)")
tb_ons_f <- read_stamped("tb_female_ons_2022_2024.rds", "England - ONS 2022-2024 (female)")
tb_no_m  <- read_stamped("tb_male_ssb_2025.rds",        "Norway - SSB 2025 (male)")
tb_no_f  <- read_stamped("tb_female_ssb_2025.rds",      "Norway - SSB 2025 (female)")

## Netherlands: the reference ZiN's iDBC uses (HMD 2019).
tb_nl_m  <- read_stamped("tb_male_nld_2019.rds",        "Netherlands - HMD 2019 (male)")
tb_nl_f  <- read_stamped("tb_female_nld_2019.rds",      "Netherlands - HMD 2019 (female)")
tb_nl_p  <- read_stamped("tb_pool_nld_2019.rds",        "Netherlands - HMD 2019 (pooled)")

## England, ONS 2017-2019: the vintage the DSU shortfall calculator's
## reference case DELIBERATELY uses (commit 286c56e reverted it from
## 2022-24), pre-pandemic like ZiN's HMD 2019. Harvested 2026-09-04 from
## ltbl.mb's nlte198020223.json freeze; pooled survivor-weighted,
## birth_ratio 1.051, same convention as every pooled table here.
tb_on17_m <- read_stamped("tb_male_ons_2017_2019.rds",   "England - ONS 2017-2019 (male)")
tb_on17_f <- read_stamped("tb_female_ons_2017_2019.rds", "England - ONS 2017-2019 (female)")
tb_on17_p <- read_stamped("tb_pool_ons_2017_2019.rds",   "England - ONS 2017-2019 (pooled)")


## Sex-specific norms for England and Norway, copied verbatim from
## ltbl.mb (hence the source-style filenames). With these, every
## jurisdiction can offer a matched-sex pairing rather than only pooled.
nm_ons_m <- read_stamped("hrqol_england_2018_mcnamara_male.rds",
                         "England - McNamara 2018 (male, 3L)")
nm_ons_f <- read_stamped("hrqol_england_2018_mcnamara_female.rds",
                         "England - McNamara 2018 (female, 3L)")
nm_no_m  <- read_stamped("hrqol_norway_2019_garratt_male.rds",
                         "Norway - Garratt 2019 (male, 5L)")
nm_no_f  <- read_stamped("hrqol_norway_2019_garratt_female.rds",
                         "Norway - Garratt 2019 (female, 5L)")

## Netherlands norms. Two Heijink surfaces, one per tariff column of the
## appendix (Heijink 2011 predicted values, via Tromm/Versteegh email
## 2026-08-31). The NLD tariff is the one the iDBC applies -- verified
## against the live tool, see SESSION_2026-09-03_idbc_tariff_check.md --
## and takes the blessed hint; the UK tariff is the published Table 2 fit,
## kept for the England comparison (same instrument and states, different
## preferences). Versteegh is 5L on the Dutch tariff, pooled by its own
## sample composition and inert for shortfall.
nm_nl_m_nld <- read_stamped("qn_male_nl_heijinknld_hmd2019.rds",
                            "Netherlands - Heijink 2011 (male, 3L Dutch tariff)")
nm_nl_f_nld <- read_stamped("qn_female_nl_heijinknld_hmd2019.rds",
                            "Netherlands - Heijink 2011 (female, 3L Dutch tariff)")
nm_nl_p_nld <- read_stamped("qn_pool_nl_heijinknld_hmd2019.rds",
                            "Netherlands - Heijink 2011 (pooled, 3L Dutch tariff)")
nm_nl_m_uk  <- read_stamped("qn_male_nl_heijinkuk_hmd2019.rds",
                            "Netherlands - Heijink 2011 (male, 3L UK tariff)")
nm_nl_f_uk  <- read_stamped("qn_female_nl_heijinkuk_hmd2019.rds",
                            "Netherlands - Heijink 2011 (female, 3L UK tariff)")
nm_nl_p_uk  <- read_stamped("qn_pool_nl_heijinkuk_hmd2019.rds",
                            "Netherlands - Heijink 2011 (pooled, 3L UK tariff)")

## the freezes carry source = "appendix", too thin for the audit chain;
## enrich here until the upstream builder stamps full provenance itself.
HEIJINK_SRC <- paste(
  "Heijink R, van Baal P, Oppe M, Koolman X, Westert G (2011). Decomposing",
  "cross-country differences in quality adjusted life expectancy. Popul",
  "Health Metr 9:17. Predicted values by age group/sex/tariff from the",
  "author's appendix, received via M. Tromm / M. Versteegh (2026-08-31);",
  "interpolated to the HMD 2019 single-year grid.")
for (v in c("nm_nl_m_nld", "nm_nl_f_nld", "nm_nl_p_nld",
            "nm_nl_m_uk",  "nm_nl_f_uk",  "nm_nl_p_uk")) {
  o <- get(v); attr(o, "source") <- HEIJINK_SRC; assign(v, o)
}
nm_nl_v <- read_stamped("qn_pool_nl_versteegh2016_samplepooled.rds",
                        "Netherlands - Versteegh 2016 (5L, sample-pooled)")

## England 5L norms: HSE 2017-2018 profiles scored with the Rowen 2026
## UK value set -- the NICE reference case from 2026-08-27 (PMG51; the
## 12/18 and .85/.95 cut-offs are explicitly unchanged, FAQ 21).
## Harvested from the DSU shortfall calculator v2 data (provenance
## sidecar in DIN); the calculator reproduces to the digit under
## year-wise discounting (25.29 @ age 0, 50/50, 3.5%; 2026-09-04).
## McNamara 2018 (3L) remains the correct record for pre-PMG51 topics.
nm_en5_m <- read_stamped("qn_male_en_rowen2026_hse20172018.rds",
                         "England - HSE/Rowen 2026 (male, 5L)")
nm_en5_f <- read_stamped("qn_female_en_rowen2026_hse20172018.rds",
                         "England - HSE/Rowen 2026 (female, 5L)")
nm_en5_p <- read_stamped("qn_pool_en_rowen2026_hse20172018.rds",
                         "England - HSE/Rowen 2026 (pooled, 5L)")


stopifnot(inherits(tb, "ltbl"), identical(attr(tb, "region"), "England"),
          !is.null(attr(tb, "source")))
stopifnot(!is.null(attr(nm, "region")), !is.null(attr(nm, "valueset")),
          !is.null(attr(nm, "source")))
stopifnot(inherits(tb_no, "ltbl"), identical(attr(tb_no, "region"), "Norway"))
stopifnot(!is.null(attr(nm_no, "valueset")), identical(attr(nm_no, "region"), "Norway"))


nm_full_healt <- structure(
  data.frame(age = 0:105, hrqol = rep(1,106)),
  region = "Any",
  valueset = "NA (unadjusted life-years: HRQoL = 1)",
  source = "NA",
  sex = "pooled",
  label = "Full Health")

## =====================================================================
## 2. NICE 2022 -- real.
## =====================================================================
nice_2022 <- new_sevset2(
  id = "nice_2022", label = "NICE (England & Wales) - 2022 manual",
  region = "England & Wales", year = 2022L,
  measures = c("AS","PS"), rule = "max",
  bands = list(
    AS = data.frame(lower = c(0, 12, 18),   upper = c(12, 18, Inf), weight = c(1, 1.2, 1.7)),
    PS = data.frame(lower = c(0, .85, .95), upper = c(.85, .95, 1),  weight = c(1, 1.2, 1.7))),
  weight_native = "multiplier",
  ce_threshold = 20000, currency = "GBP", threshold_year = 2022L,
  discount_shortfall = TRUE, discount_rate = 0.035,
  ## superseded by nice_2026 (PMG51, 2026-08-27) for topics starting
  ## after that date; remains the correct record for ongoing/older topics.
  status = "superseded",
  reference_hint = list(table = "ons_pool_2022_2024", norm = "ons_pool_2022_2024"),
  reference_spec = list(
    match = "age_sex",
    value_set = "EQ-5D-3L (Dolan 1997) via van Hout crosswalk",
    notes = "5L modular update changes inputs, not the 12/18 or .85/.95 thresholds"),
  provenance = list(
    source = "NICE health technology evaluation manual (PMG36), Table 6.1; sec 6.2.17",
    url = "https://www.nice.org.uk/process/pmg36",
    accessed = as.Date("2026-07-27"),
    citation = "NICE. NICE Health Technology Evaluations: The Manual. 2022.",
    notes = "Disjunctive max-rule; shortfall discounted at reference-case 3.5%. Scotland (SMC) is a separate regime."))

## =====================================================================
## 2b. NICE 2026 -- the 5L modular update (REAL). Same bands, new inputs:
##     PMG51 (2026-08-27) makes the Rowen 2026 UK EQ-5D-5L value set the
##     reference case for utilities AND for the shortfall's population
##     norms (FAQ 21), while the 12/18 and .85/.95 cut-offs "remain
##     unchanged". Prospective: topics starting after 2026-08-27.
## =====================================================================
nice_2026 <- new_sevset2(
  id = "nice_2026", label = "NICE (England & Wales) - 2026 5L update",
  region = "England & Wales", year = 2026L,
  measures = c("AS","PS"), rule = "max",
  bands = list(
    AS = data.frame(lower = c(0, 12, 18),   upper = c(12, 18, Inf), weight = c(1, 1.2, 1.7)),
    PS = data.frame(lower = c(0, .85, .95), upper = c(.85, .95, 1),  weight = c(1, 1.2, 1.7))),
  weight_native = "multiplier",
  ce_threshold = 20000, currency = "GBP", threshold_year = 2022L,
  discount_shortfall = TRUE, discount_rate = 0.035,
  status = "current", supersedes = "nice_2022",
  reference_hint = list(table = "ons_pool_2017_2019", norm = "en_rowen_pool"),
  reference_spec = list(
    match = "age_sex",
    value_set = "EQ-5D-5L, UK value set (Rowen et al. 2026)",
    notes = paste("PMG51 FAQ 21: shortfall from 5L utilities and 5L population",
                  "norms; cut-offs unchanged. The DSU calculator's reference",
                  "case pairs the norms with ONS 2017-2019 life tables",
                  "(deliberately PRE-pandemic, like ZiN's HMD 2019) and",
                  "discounts YEAR-WISE: our freezes reproduce its QALE(0)",
                  "of 25.29 (50/50, 3.5%) exactly under (1+r)^-t",
                  "(verified 2026-09-04).")),
  provenance = list(
    source = "NICE interim methods statement: implementing the EQ-5D-5L value set (PMG51)",
    url = "https://www.nice.org.uk/process/pmg51",
    accessed = as.Date("2026-09-04"),
    citation = paste("NICE (2026). Interim methods statement: implementing the",
                     "EQ-5D-5L value set. Published 2026-08-27. Norms: DSU QALY",
                     "Shortfall Calculator v2; value set: Rowen D, Mukuria C,",
                     "Bray N, et al. A United Kingdom value set for the",
                     "EQ-5D-5L. Value Health 2026;29(5):858-869."),
    notes = paste("Bands identical to nice_2022; the update changes the value",
                  "set and reference norms only. Applies to topics with an",
                  "invitation to participate issued after 2026-08-27; older",
                  "topics keep the 3L reference (5L data mapped to 3L).")))

## =====================================================================
## 3. Norway -- Magnussen six severity classes (REAL).
##    Absolute shortfall (QALYs), every 4 QALYs; multiplier on a base
##    ce_threshold of NOK 275,000 (class 1 = 1.0x) rising in equal 0.4
##    steps to class 6 = 3.0x (AS > 20 -> NOK 825,000). Multiplier-native
##    keeps class 1 a clean 1.0. AS-native, gender-neutral, UNDISCOUNTED.
## =====================================================================
norway_2020 <- new_sevset2(
  id = "norway_2020", label = "Norway - Magnussen severity classes",
  region = "Norway", year = 2020L,
  measures = "AS", rule = "AS",
  bands = list(
    AS = data.frame(
      lower  = c(0,   4,   8,   12,  16,  20),
      upper  = c(4,   8,   12,  16,  20,  Inf),
      weight = c(1.0, 1.4, 1.8, 2.2, 2.6, 3.0),
      label  = paste("class", 1:6))),
  weight_native = "multiplier",
  ce_threshold = 275000, currency = "NOK", threshold_year = 2020L,
  discount_shortfall = FALSE, discount_rate = NA_real_,
  status = "current",
  reference_hint = list(table = "no_pool_2025", norm = "no_pool_2025"),
  reference_spec = list(match = "age", sex = "gender-neutral",
                        value_set = "Norwegian EQ-5D-5L (Garratt 2022/2025)"),
  provenance = list(
    source = "Magnussen committee (NOU 2015); six absolute-shortfall severity classes",
    url = "https://www.ssb.no/statbank/table/07902",
    accessed = as.Date("2026-07-27"),
    citation = paste("Barra M, Feiring E, Magelssen M, Aasen HS.",
                     "Prioritering i helsetjenesten - Fra teori til praksis.",
                     "1. utg. Gyldendal Norsk Forlag AS, 2026."),
    notes = paste("Absolute shortfall vs age-matched, gender-neutral healthy reference;",
                  "shortfall UNDISCOUNTED. Native: multiplier on base ce_threshold",
                  "NOK 275,000 (class 1) rising in 0.4 steps to 3.0 (class 6, AS>20 =",
                  "NOK 825,000). Thresholds administratively set (sources use 'WTP'",
                  "language; stored as ce_threshold per project doctrine).")))

## =====================================================================
## 4. Netherlands -- PROVISIONAL STUB.  DO NOT CITE.
##    PS-native. Real Zorginstituut PS bands + reference thresholds pending.
## =====================================================================
## 4. Netherlands -- Zorginstituut Nederland (REAL).
##    Four PS severity classes. The lowest is EXCLUSIONARY, not neutral:
##    technologies targeting diseases with PS < 0.10 are in principle not
##    reimbursed, so its threshold is 0 rather than a base rate. This is a
##    structural difference from NICE, whose bottom band means "no uplift,
##    normal threshold". PS-native; shortfall UNDISCOUNTED.
nl_2018 <- new_sevset2(
  id = "nl_2018", label = "Netherlands - Zorginstituut severity classes",
  region = "Netherlands", year = 2018L,
  measures = "PS", rule = "PS",
  bands = list(
    PS = data.frame(
      lower  = c(0,    0.10,  0.41,  0.71),
      upper  = c(0.10, 0.41,  0.71,  1),
      weight = c(0,    20000, 50000, 80000),
      label  = c("not reimbursed in principle",
                 "PS 0.10-0.40", "PS 0.41-0.70", "PS 0.71-1.00"))),
  weight_native = "ce_threshold",
  ce_threshold = 20000, currency = "EUR", threshold_year = 2018L,
  discount_shortfall = FALSE, discount_rate = NA_real_,
  status = "current",
  reference_hint = list(table = "nl_pool_2019", norm = "nl_heijink_pool_nld"),
  reference_spec = list(
    match = "age",
    value_set = "EQ-5D-3L, country-specific (Heijink et al. 2011)",
    notes = paste("ZiN computes PS with the iDBC tool",
                  "(imtamodels.shinyapps.io/iDBCv2_1/), which uses Heijink",
                  "2011 EQ-5D-3L value sets and Human Mortality Database 2019",
                  "life expectancy. HMD 2019 is PRE-pandemic, unlike the ONS",
                  "2022-2024 and SSB 2025 tables held here. Tariff verified",
                  "against the live iDBC v2.1 (2026-09-03): the tool's",
                  "reference QALE matches the NETHERLANDS-tariff column of",
                  "the Heijink appendix, not the published UK fit.")),
  provenance = list(
    source = "Zorginstituut Nederland, Ziektelast in de praktijk (2018)",
    url = "https://doi.org/10.1016/j.jval.2019.07.012",
    accessed = as.Date("2026-08-11"),
    citation = paste("Reckers-Droog VT, van Exel J, Brouwer W.",
                     "Equity Weights for Priority Setting in Healthcare:",
                     "Severity, Age, or Both? Value in Health.",
                     "2019;22(12):1441-1449, Box 1."),
    notes = paste("weight1-exception: the lowest class carries a threshold of",
                  "ZERO, not the base rate. PS < 0.10 is in principle NOT",
                  "REIMBURSED, so no ICER clears it. The Dutch scheme has no",
                  "neutral band; its bottom class is a refusal. Since 2018 ZiN",
                  "supplements PS with absolute shortfall and prospective",
                  "health (PH = remaining QALE from onset without treatment),",
                  "explicitly to be transparent about the age consequences of",
                  "applying PS. Method reference: Versteegh et al.,",
                  "PharmacoEconomics 2019;37(9):1155-1163.")))

## =====================================================================
## 5. assemble via the guarded setters (validate at the door)
##    labels are already on the objects, so they ride into the store.
## =====================================================================
store <- new_shortfalls(
  ## the regulator-current reference case (PMG51): nice_2026 paired as
  ## the DSU calculator pairs it. nice_2022's triplet remains available.
  defaults = list(sevset = "nice_2026",
                  norm   = "en_rowen_pool",
                  table  = "ons_pool_2017_2019"),
  builder  = paste0("aux_build_shortfalls_appdata.R @ ", git_sha %||% "nogit"))

store <- add_ltbl(store, "ons_pool_2022_2024",   tb)
store <- add_norm(store, "ons_pool_2022_2024",   nm)
store <- add_ltbl(store, "ons_male_2022_2024",   tb_ons_m)
store <- add_norm(store, "ons_male_2022_2024",   nm_ons_m)
store <- add_ltbl(store, "ons_female_2022_2024", tb_ons_f)
store <- add_norm(store, "ons_female_2022_2024", nm_ons_f)

## England 5L strand (PMG51): tables keyed by vintage, norms carry the
## instrument (en_rowen_*), per the Dutch precedent.
store <- add_ltbl(store, "ons_pool_2017_2019",   tb_on17_p)
store <- add_ltbl(store, "ons_male_2017_2019",   tb_on17_m)
store <- add_ltbl(store, "ons_female_2017_2019", tb_on17_f)
store <- add_norm(store, "en_rowen_pool",        nm_en5_p)
store <- add_norm(store, "en_rowen_male",        nm_en5_m)
store <- add_norm(store, "en_rowen_female",      nm_en5_f)

store <- add_ltbl(store, "no_pool_2025",         tb_no)
store <- add_norm(store, "no_pool_2025",         nm_no)
store <- add_ltbl(store, "no_male_2025",         tb_no_m)
store <- add_norm(store, "no_male_2025",         nm_no_m)
store <- add_ltbl(store, "no_female_2025",       tb_no_f)
store <- add_norm(store, "no_female_2025",       nm_no_f)

store <- add_ltbl(store, "nl_pool_2019",         tb_nl_p)
store <- add_ltbl(store, "nl_male_2019",         tb_nl_m)
store <- add_ltbl(store, "nl_female_2019",       tb_nl_f)

## Dutch norms carry the tariff in the key; tables keep theirs. _nld is
## what the iDBC applies (verified 2026-09-03), _uk the published fit.
store <- add_norm(store, "nl_heijink_pool_nld",   nm_nl_p_nld)
store <- add_norm(store, "nl_heijink_male_nld",   nm_nl_m_nld)
store <- add_norm(store, "nl_heijink_female_nld", nm_nl_f_nld)
store <- add_norm(store, "nl_heijink_pool_uk",    nm_nl_p_uk)
store <- add_norm(store, "nl_heijink_male_uk",    nm_nl_m_uk)
store <- add_norm(store, "nl_heijink_female_uk",  nm_nl_f_uk)

store <- add_norm(store, "nl_versteegh",         nm_nl_v)

store <- add_norm(store, "full_health", nm_full_healt)

store <- add_sevset(store, "nice_2022",   nice_2022)
store <- add_sevset(store, "nice_2026",   nice_2026)
store <- add_sevset(store, "norway_2020", norway_2020)
store <- add_sevset(store, "nl_2018",     nl_2018)

## whole-container validation before freezing
validate_shortfalls(store)

## 5b. conditions -- harvest the knot-authored .rds from DIN ####
## They sit in DIN beside the pooled intermediates, so discriminate on
## SHAPE: a condition is a data.frame(age, hrqol, mu) carrying menu_label.
## Their stored reference is free text from the pre-container app; remap
## it to container ids (all were authored against the ONS pooled pair),
## keeping the original strings and fingerprint for the record.
COND_TABLE <- "ons_pool_2022_2024"
COND_NORM  <- "ons_pool_2022_2024"

slug <- function(path) {
  s <- tolower(sub("\\.rds$", "", basename(path)))
  gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", s))
}

for (f in list.files(DIN, pattern = "\\.rds$", full.names = TRUE)) {
  obj <- tryCatch(readRDS(f), error = function(e) NULL)
  is_cond <- is.data.frame(obj) &&
    all(c("age", "hrqol", "mu") %in% names(obj)) &&
    !is.null(attr(obj, "menu_label"))
  if (!is_cond) next
  old <- attr(obj, "reference") %||% list()
  attr(obj, "reference") <- list(
    table = COND_TABLE, norm = COND_NORM,
    fingerprint   = old$fingerprint %||% NA_character_,
    remapped_from = list(table = old$table %||% NA_character_,
                         norm  = old$norm  %||% NA_character_))
  attr(obj, "canonical") <- TRUE
  store <- add_condition(store, slug(f), obj)
}


## =====================================================================
## 6. write + report
## =====================================================================
if (!dir.exists(DOUT)) dir.create(DOUT, recursive = TRUE)
out <- file.path(DOUT, "shortfalls_data.rds")
saveRDS(store, out)

cat("wrote ", out, "\n", sep = "")
cat("spec_version ", attr(store,"spec_version"),
    "   built ", format(attr(store,"built")),
    "   builder ", attr(store,"builder"), "\n", sep = "")
cat("\nsevsets (current + provisional):\n");  print(list_sevsets(store))
cat("\nall sevsets incl. historical:\n");     print(list_sevsets(store, include_historical = TRUE))
cat("\nnorms : ", paste(names(store$hrqol_norms),  collapse = ", "), "\n", sep = "")
cat("tables: ", paste(names(store$life_tables), collapse = ", "), "\n", sep = "")
cat("\nconditions:\n"); print(list_conditions(store))

## -- pairing sanity: each real regime against its blessed reference ---
green_check <- function(sid, tkey, nkey = tkey) {
  s  <- read_sevset(store, sid)
  nk <- store$hrqol_norms[[nkey]]
  tk <- store$life_tables[[tkey]]
  cmp <- compatible(s, nk, tk, nkey, tkey)
  cat(sprintf("%-12s x %-16s / %-16s : %s%s\n", sid, tkey, nkey, cmp$status,
              if (length(cmp$reasons)) paste0(" (", paste(cmp$reasons, collapse="; "), ")") else ""))
}
cat("\npairing checks:\n")
green_check("nice_2022",   "ons_pool_2022_2024")
green_check("nice_2026",   "ons_pool_2017_2019", "en_rowen_pool")
green_check("norway_2020", "no_pool_2025")
green_check("nl_2018", "nl_pool_2019", "nl_heijink_pool_nld")