## =====================================================================
## sevset.R  (v0.1) -- severity threshold-set machinery
## ---------------------------------------------------------------------
## Standalone. Source it and a self-test runs, checking applied_weight()
## against the worked numbers in Skedgel & Mott (2026). The app sources
## this file (set SEVSET_NO_SELFTEST <- TRUE first to silence the test).
##
## A "sevset" records how a jurisdiction turns AS/PS shortfall into a
## decision weight: the tier bands, the combination rule, whether the
## body discounts the shortfall, and provenance. It does NOT compute Q/q
## -- life tables and norms live elsewhere.
##
## This is unit (1) of the data layer. Not yet included: the container
## (shortfalls_data.rds), the add_* setters, the overlay loader, compatible().
## =====================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

## project palette (kept here so this file is standalone; the app may
## source this and drop its own copy -- they must stay identical)
if (!exists("pal")) pal <- c(
  SPBlue='#002768', SPGreen='#64A620', SPRed='#EF2B2D', SPYellow='#FECA00',
  SPPurple='#756FB9', SPBlueLight='#80A8D9', SPGreenLight='#64D292')

## ---------------------------------------------------------------------
## constructor
## ---------------------------------------------------------------------
## bands: named list of data.frames (names == measures), each with
##   columns lower, upper, weight (and optional label). Half-open
##   [lower, upper), ascending, contiguous. See validate_sevset for the
##   full invariant set.
new_sevset <- function(id, label, region, year,
                       measures, rule, bands,
                       weight_native = c("multiplier", "ce_threshold"),
                       ce_threshold = NA_real_, currency = NA_character_,
                       threshold_year = NA_integer_,
                       discount_shortfall = FALSE, discount_rate = NA_real_,
                       reference_spec = list(),
                       reference_hint = list(),      # list(table=, norm=)
                       provenance = list(),
                       canonical = TRUE,
                       validate = TRUE) {
  weight_native <- match.arg(weight_native)
  set <- structure(
    list(id = id, label = label, region = region, year = as.integer(year),
         measures = measures, rule = rule, bands = bands,
         weight_native = weight_native, ce_threshold = ce_threshold,
         currency = currency, threshold_year = threshold_year,
         discount_shortfall = discount_shortfall, discount_rate = discount_rate,
         reference_spec = reference_spec, reference_hint = reference_hint,
         provenance = provenance, canonical = canonical),
    class = "sevset")
  if (validate) validate_sevset(set)
  set
}

## ---------------------------------------------------------------------
## validator -- guard-clause; errors on any breach, invisible(TRUE) else
## ---------------------------------------------------------------------
validate_sevset <- function(set) {
  if (!inherits(set, "sevset")) stop("not a sevset", call. = FALSE)
  err <- function(...) stop(sprintf("sevset '%s': %s",
                                    set$id %||% "?", paste0(...)), call. = FALSE)

  ## -- per set ------------------------------------------------------
  if (!length(set$measures) || !all(set$measures %in% c("AS","PS")) ||
      anyDuplicated(set$measures))
    err("measures must be a non-empty subset of {AS,PS}, no duplicates")
  if (!setequal(names(set$bands), set$measures))
    err("names(bands) must set-equal measures")
  if (!set$rule %in% c("max","AS","PS")) err("rule must be max/AS/PS")
  if (set$rule == "max" && length(set$measures) != 2)
    err("rule 'max' requires exactly two measures")
  if (set$rule %in% c("AS","PS") && !identical(set$measures, set$rule))
    err("rule '", set$rule, "' requires measures == '", set$rule, "'")
  if (!set$weight_native %in% c("multiplier","ce_threshold"))
    err("weight_native must be multiplier/ce_threshold")
  if (identical(set$weight_native, "ce_threshold") && !is.finite(set$ce_threshold))
    err("weight_native 'ce_threshold' requires finite ce_threshold")
  if (isTRUE(set$discount_shortfall) && !is.finite(set$discount_rate))
    err("discount_shortfall == TRUE requires finite discount_rate")

  neutral <- if (identical(set$weight_native, "multiplier")) 1 else set$ce_threshold
  ok_exc  <- grepl("weight1-exception", set$provenance$notes %||% "", fixed = TRUE)

  ## -- per band -----------------------------------------------------
  for (m in set$measures) {
    b <- set$bands[[m]]
    if (!all(c("lower","upper","weight") %in% names(b)))
      err(m, ": band needs columns lower/upper/weight")
    if (!is.numeric(b$lower) || !is.numeric(b$upper) || !is.numeric(b$weight))
      err(m, ": lower/upper/weight must be numeric")
    if (nrow(b) < 1)                       err(m, ": band is empty")
    if (is.unsorted(b$lower, strictly = TRUE)) err(m, ": lower must strictly ascend")
    if (any(b$upper[-nrow(b)] != b$lower[-1]))  err(m, ": bands not contiguous")
    if (b$lower[1] != 0)                   err(m, ": first lower must be 0")
    if (m == "AS" && !is.infinite(b$upper[nrow(b)])) err("AS: last upper must be Inf")
    if (m == "PS" && b$upper[nrow(b)] != 1)          err("PS: last upper must be 1")
    if (is.unsorted(b$weight))             err(m, ": weight must be non-decreasing")
    if (m == "AS" && any(b$lower < 0))     err("AS: bands must be non-negative")
    if (m == "PS" && (b$lower[1] < 0 || b$upper[nrow(b)] > 1))
      err("PS: bands must lie in [0,1]")
    if (!isTRUE(all.equal(b$weight[1], neutral)) && !ok_exc)
      err(m, ": first-tier weight must equal neutral (", neutral,
          ") unless provenance$notes documents 'weight1-exception'")
  }
  invisible(TRUE)
}

## ---------------------------------------------------------------------
## band lookup + applied weight (vectorised over AS, PS)
## ---------------------------------------------------------------------
## findInterval(x, lower) gives the half-open [lower_i, lower_{i+1}) row.
## x < 0 -> index 0 -> NA (undefined; should not occur for valid input).
band_weight <- function(x, band) {
  i <- findInterval(x, band$lower)
  i[i < 1] <- NA_integer_
  band$weight[i]
}

## the applied weight under the set's own rule.
applied_weight <- function(AS = NULL, PS = NULL, set) {
  w <- list()
  if ("AS" %in% set$measures) w$AS <- band_weight(AS, set$bands$AS)
  if ("PS" %in% set$measures) w$PS <- band_weight(PS, set$bands$PS)
  switch(set$rule,
         max = do.call(pmax, c(w, list(na.rm = TRUE))),
         AS  = w$AS,
         PS  = w$PS)
}

## backward-compatible wrapper (retires the app's hardcoded nice_weight)
nice_weight <- function(AS, PS) applied_weight(AS, PS, nice_sevset)

## ---------------------------------------------------------------------
## guide lines for a measure: interior boundaries + a yellow->red ramp
## (neutral tier undrawn). Blue stays reserved for survival horizons.
## ---------------------------------------------------------------------
guide_lines <- function(set, measure) {
  b <- set$bands[[measure]]
  n <- nrow(b)
  if (n < 2) return(data.frame(at = numeric(0), tier = integer(0),
                               weight = numeric(0), colour = character(0),
                               stringsAsFactors = FALSE))
  ramp <- grDevices::colorRampPalette(c(pal[["SPYellow"]], pal[["SPRed"]]))(n - 1)
  data.frame(at = b$lower[-1], tier = 2:n, weight = b$weight[-1],
             colour = ramp, stringsAsFactors = FALSE)
}

## ---------------------------------------------------------------------
## normalise a ce_threshold-native set to multiplier representation
## (tier thresholds / base). Idempotent on multiplier-native sets.
## ---------------------------------------------------------------------
as_multiplier <- function(set) {
  if (identical(set$weight_native, "multiplier")) return(set)
  base <- set$ce_threshold
  for (m in set$measures) set$bands[[m]]$weight <- set$bands[[m]]$weight / base
  set$weight_native <- "multiplier"
  set$provenance$notes <- paste0(set$provenance$notes %||% "",
                                 " [normalised to multiplier from ce_threshold]")
  set
}

## =====================================================================
## NICE 2022 -- the one fully-specified canonical set (PMG36 Table 6.1)
## =====================================================================
nice_sevset <- new_sevset(
  id = "nice_2022", label = "NICE (England & Wales) \u2014 2022 manual",
  region = "England & Wales", year = 2022L,
  measures = c("AS","PS"), rule = "max",
  bands = list(
    AS = data.frame(lower = c(0, 12, 18),   upper = c(12, 18, Inf),
                    weight = c(1, 1.2, 1.7)),
    PS = data.frame(lower = c(0, .85, .95), upper = c(.85, .95, 1),
                    weight = c(1, 1.2, 1.7))),
  weight_native = "multiplier",
  ce_threshold = 20000, currency = "GBP", threshold_year = 2022L,
  discount_shortfall = TRUE, discount_rate = 0.035,
  reference_spec = list(
    match = "age_sex", value_set = "EQ-5D-3L (Dolan 1997) via van Hout crosswalk",
    notes = "5L modular update changes inputs, not the 12/18 or .85/.95 thresholds"),
  reference_hint = list(table = "ons_pool_2022_2024", norm = "ons_pool_2022_2024"),
  provenance = list(
    source = "NICE health technology evaluation manual (PMG36), Table 6.1; sec 6.2.17",
    url = "https://www.nice.org.uk/process/pmg36",
    accessed = as.Date("2026-07-27"),
    citation = "NICE. NICE Health Technology Evaluations: The Manual. 2022.",
    notes = "Disjunctive max-rule; shortfall discounted at reference-case 3.5%."))

## =====================================================================
## self-test -- runs on source unless SEVSET_NO_SELFTEST is set.
## Reproduces the worked figures in Skedgel & Mott (2026): the newborn
## who loses half their QALE qualifies at tier 3 undiscounted, but drops
## OUT of the modifier once the shortfall is discounted.
## =====================================================================
if (!exists("SEVSET_NO_SELFTEST") || !isTRUE(SEVSET_NO_SELFTEST)) local({
  n_pass <- 0L; n_fail <- 0L ## WARNING: do not rename without updating <<- in chk() below
  chk <- function(label, cond) {
    ok <- isTRUE(cond)
    cat(if (ok) "  [ok] " else "  [XX] ", label, "\n", sep = "")
    if (ok) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L ## WARNING
  }
  cat("sevset.R self-test\n------------------\n")

  chk("NICE set validates", tryCatch({validate_sevset(nice_sevset); TRUE},
                                      error = function(e) FALSE))

  ## paper's newborn (undiscounted): AS 35.43, PS 0.50 -> AS route, tier 3
  chk("newborn undiscounted -> 1.7",
      applied_weight(AS = 35.43, PS = 0.50, nice_sevset) == 1.7)
  ## paper's newborn (discounted 3.5%): AS 4.07, PS 0.1635 -> out (1.0)
  chk("newborn discounted   -> 1.0",
      applied_weight(AS = 4.07, PS = 0.1635, nice_sevset) == 1.0)

  ## tier edges (half-open): AS exactly 12 -> tier 2; 18 -> tier 3
  chk("AS = 12 -> 1.2 (half-open lower closed)",
      applied_weight(AS = 12, PS = 0, nice_sevset) == 1.2)
  chk("AS = 18 -> 1.7",
      applied_weight(AS = 18, PS = 0, nice_sevset) == 1.7)
  chk("AS = 11.99 -> 1.0",
      applied_weight(AS = 11.99, PS = 0, nice_sevset) == 1.0)

  ## PS route independent of AS (max-rule): PS .90 -> 1.2, .96 -> 1.7, 1 -> 1.7
  chk("PS = .90 -> 1.2 via PS route",
      applied_weight(AS = 0, PS = .90, nice_sevset) == 1.2)
  chk("PS = .96 -> 1.7 via PS route",
      applied_weight(AS = 0, PS = .96, nice_sevset) == 1.7)
  chk("PS = 1.00 -> 1.7 (closed top)",
      applied_weight(AS = 0, PS = 1, nice_sevset) == 1.7)

  ## max-rule takes the more generous: AS tier 1, PS tier 3 -> 1.7
  chk("max-rule: (AS 5, PS .96) -> 1.7",
      applied_weight(AS = 5, PS = .96, nice_sevset) == 1.7)

  ## vectorised
  chk("vectorised applied_weight",
      identical(applied_weight(c(4.07, 13, 40), c(.16, .10, .50), nice_sevset),
                c(1.0, 1.2, 1.7)))

  ## guide lines land on the interior boundaries
  gAS <- guide_lines(nice_sevset, "AS")
  chk("guide_lines AS at 12 & 18", identical(gAS$at, c(12, 18)))
  gPS <- guide_lines(nice_sevset, "PS")
  chk("guide_lines PS at .85 & .95", identical(gPS$at, c(.85, .95)))

  ## as_multiplier idempotent on multiplier-native
  chk("as_multiplier idempotent", identical(as_multiplier(nice_sevset)$bands,
                                             nice_sevset$bands))

  ## a broken set (non-contiguous AS bands) must fail validation
  bad <- tryCatch({
    new_sevset("bad","bad","x",2020L, c("AS","PS"), "max",
      bands = list(
        AS = data.frame(lower=c(0,12,20), upper=c(12,18,Inf), weight=c(1,1.2,1.7)),
        PS = data.frame(lower=c(0,.85,.95), upper=c(.85,.95,1), weight=c(1,1.2,1.7))))
    FALSE
  }, error = function(e) TRUE)
  chk("non-contiguous bands rejected", bad)

  cat(sprintf("------------------\n%d passed, %d failed\n", n_pass, n_fail))
  if (n_fail > 0L) warning("sevset.R self-test: ", n_fail, " failure(s)")
})
