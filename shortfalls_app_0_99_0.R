## shortfalls_app.R  (0.99.0 RC -- release candidate for 1.0.0) ####
## ---------------------------------------------------------------------#
## Interactive AS/PS severity visualiser for the premature-death scenario.
##
## MODEL. Vantage = onset = age a. Patient is guaranteed W years' survival
## (q = 0 on [a, a+W), q = 1 after), with HRQoL from the chosen norm.
## x-axis = age at onset. Twin axes: absolute shortfall AS (left),
## proportional shortfall PS (right). One remaining-survival W (slider),
## one severity regime (single-select). Guide lines, tier colours, and the
## severity ribbon are all generated FROM the chosen regime's bands, so
## NICE (3 tiers), Norway (6 AS classes), and user-authored regimes all
## render without special-casing.
##
## TRIPLET. Three independent single-selects -- severity regime (sevset),
## HRQoL norm, life table -- form a triplet. Any combination renders:
## this is a conditional visualiser, not an oracle ("IF this is it, THEN
## this is the picture"). The compatibility badge CHARACTERISES the triplet
## (green = reference pairing; amber = plausible/unverified; red = off-label
## or user-edited) but NEVER blocks the plot.
##
## DISCOUNTING. Continuous e^(-rho t) by default; sub-year survival
## discounted at its midpoint. Year-wise (1+rho)^(-t) is selectable but
## understates sub-year shortfall. Applies to the with-condition stream;
## the reference QALE is cached and recomputed only on norm/table/rho/mode.
##
## SEVERITY RIBBON. A strip below the x-axis showing, per age, the applied
## weight and the decisive criterion (AS / PS / AS.PS) under the regime's
## rule (max / min / mean / single). Colour maps to the applied weight;
## text gives the number, so multi-tier regimes stay legible.
##
## THRESHOLD AUTHOR (tab). Fork an existing regime, edit its bands, choose
## measures and (for two measures) a combining rule, and save. LOCAL mode
## persists to shortfalls_data/shortfalls_data_user.rds and can
## delete authored regimes; SERVER mode is session-only + download and
## never writes to disk. Built-in regimes are immutable.
##
## DEPLOY CONTRACT. Runs given ONLY this file + shortfalls_data/shortfalls_data.rds.
## Run-time accessors (read/validate/compatible) are inlined below; build-
## time machinery (constructors, add_*, the container build script) lives
## in shortfalls_R_aux/ and never ships. Server safety comes from read-only deploy
## permissions, not the APP_MODE flag.
##
## PARKED (see shortfalls_vault/ notes): "compare with" two-line mode; warn-vs-
## block validator split in the author tab; caption/reason-string polish.
## =====================================================================#
#options(shiny.fullstacktrace = TRUE)

library(shiny)
library(plotly)
library(shinyWidgets)


pal <- c(SPBlue='#002768', SPGreen='#64A620', SPRed='#EF2B2D', SPYellow='#FECA00',
         SPPurple='#756FB9', SPBlueLight='#80A8D9', SPGreenLight='#64D292',
         SPBlueLightest='#D6E4F5')

FONT_BODY <- "Perpetua, 'Computer Modern Serif', Cambria, Georgia, serif"
FONT_HEAD <- "'Gill Sans', 'Gill Sans MT', Calibri, 'Segoe UI', sans-serif"

`%||%` <- function(x, y) if (is.null(x)) y else x
TOP <- 120                      # maximal age in life tables
SF <- "shortfalls_data/shortfalls_data.rds"  # the one shipped artefact
PLOT_HEIGHT <- "680px"          # The height of the plot-part of the app
LOG_BASE <- 2      # slider is log_OR_BASE(OR); 0 = no excess (OR = 1)

## READ THIS BEFORE SERVER DEPLOYMENT!!!!####
## Deployment mode. LOCAL: authored regimes persist to shortfalls_data_user.rds
## SERVER: session-only + download; never writes
## to server disk. NOTE: the flag is INTENT, not security -- real server
## safety comes from running the process with the app dir READ-ONLY to the
## shiny user, so save cannot write regardless. See save handler.
APP_MODE <- "local"   # "local" | "server"
## DEPLOY (server): set APP_MODE <- "server" AND run the process with the
## app directory + shortfalls_data/ read-only to the shiny user. The flag stops
## the app OFFERING to save; the permissions stop it BEING ABLE to. The
## flag alone is not a security boundary.

## =====================================================================#
## INLINED RUN-TIME ACCESSORS  (read-side only; build-side stays in aux)
## =====================================================================#

## -- band lookup + applied weight (vectorised) ------------------------
.band_weight <- function(x, band) {
  i <- findInterval(x, band$lower); i[i < 1] <- NA_integer_; band$weight[i]
}
applied_weight <- function(AS = NULL, PS = NULL, set) {
  w <- list()
  if ("AS" %in% set$measures) w$AS <- .band_weight(AS, set$bands$AS)
  if ("PS" %in% set$measures) w$PS <- .band_weight(PS, set$bands$PS)
  switch(set$rule,
         max  = do.call(pmax, c(w, list(na.rm = TRUE))),
         min  = do.call(pmin, c(w, list(na.rm = TRUE))),
         mean = Reduce(`+`, w) / length(w),
         AS   = w$AS, PS = w$PS)
}

## -- interior guide lines for a measure, with a yellow->red tier ramp --
## returns data.frame(at, tier, weight, colour); empty if <2 bands.
guide_lines <- function(set, measure) {
  b <- set$bands[[measure]]; n <- nrow(b)
  if (n < 2) return(data.frame(at=numeric(0), tier=integer(0),
                               weight=numeric(0), colour=character(0)))
  ramp <- grDevices::colorRampPalette(c(pal[["SPYellow"]], pal[["SPRed"]]))(n - 1)
  data.frame(at = b$lower[-1], tier = 2:n, weight = b$weight[-1],
             colour = ramp, stringsAsFactors = FALSE)
}

## -- listing / lookup over the loaded store ---------------------------
list_sevsets <- function(store, include_historical = FALSE) {
  lst <- store$sf_thresholds
  keep <- vapply(lst, function(s)
    include_historical || (s$status %||% "current") %in% c("current","provisional"),
    logical(1))
  names(lst)[keep]
}
sevset_label  <- function(store, id) store$sf_thresholds[[id]]$label %||% id
read_sevset   <- function(store, id) store$sf_thresholds[[id]]
norm_ids      <- function(store) names(store$hrqol_norms)
table_ids     <- function(store) names(store$life_tables)
read_condition <- function(store, id) (store$sf_conditions %||% list())[[id]]


## display label for a container object: label -> menu_label -> id.
## (norms and tables carry "label"; conditions inherit "menu_label" from
## the pre-container app -- both are honoured so one helper serves all.)
.labels_for <- function(objs) {
  ids  <- names(objs)
  labs <- vapply(ids, function(i) {
    o <- objs[[i]]
    attr(o, "label") %||% attr(o, "menu_label") %||% i
  }, "")
  stats::setNames(ids, labs)
}

## -- light integrity guard (NOT the full build-time validator) --------
## The object was validated when built; here we only fail clearly on a
## corrupt/stale/foreign file rather than deep downstream.
load_shortfalls <- function(path = SF) {
  if (!file.exists(path)) stop("shortfalls_data.rds not found at: ", path, call. = FALSE)
  store <- readRDS(path)
  ok <- inherits(store, "shortfalls_data") &&
    all(c("sf_thresholds","hrqol_norms","life_tables") %in% names(store)) &&
    length(store$sf_thresholds) >= 1 &&
    length(store$hrqol_norms)   >= 1 &&
    length(store$life_tables)   >= 1
  if (!ok) stop("file is not a well-formed shortfalls_data container: ", path, call. = FALSE)
  store
}


## display label for a container object (falls back to the id)
.obj_label <- function(lst, id) {
  o <- lst[[id %||% ""]]
  if (is.null(o)) return(id %||% "?")
  attr(o, "label") %||% attr(o, "menu_label") %||% id
}

## -- compatibility: CHARACTERISE a triplet, never block ---------------
## green  : sevset's reference_hint names exactly these norm/table ids,
##          regions agree, override consistent with native discounting.
## amber  : no hint, but regions/value coherent; plausible-unverified.
## red    : region mismatch (tariff-vintage confound), user-forced object,
##          off-diagonal pooled norm (mixed against a different table),
##          or discount override contradicting the set's native practice.
## Returns list(status, reasons).  norm_id/table_id are the chosen keys.
compatible <- function(set, norm, ltbl, norm_id, table_id, override = NULL) {
  ### NEVER RENAME <var> "status" or "reasons" without correcting bump's <var> <<- lines!!
  status <- "green"; reasons <- character(0)
  bump <- function(to, why) {
    rank <- c(green=1L, amber=2L, red=3L)
    if (rank[[to]] > rank[[status]]) status <<- to ## WARNING must be updated on renaming local variable status
    reasons <<- c(reasons, why) ## WARNING must be updated on renaming local variable reasons
  }
  ### NEVER RENAME <var> "status" or "reasons" without correcting bump's <var> <<- lines!!
  hint <- set$reference_hint %||% list()
  hint_named <- length(hint) && !is.na(hint$norm %||% NA) && !is.na(hint$table %||% NA)
  hint_met <- hint_named &&
    identical(norm_id,  hint$norm) &&
    identical(table_id, hint$table)
  
  nr <- attr(norm, "region"); tr <- attr(ltbl, "region"); sr <- set$region
  
  if (hint_met) {
    ## consistent: skip region check (set region "England & Wales" is a
    ## deliberate superset of the England-coverage reference data).
  } else if (hint_named) {
    bump("amber", sprintf("Not fully consistent: %s expects norm='%s', table='%s'",
                          set$id, hint$norm, hint$table))
  } else {
    regs <- unique(stats::na.omit(c(sr, nr, tr)))
    if (length(regs) > 1)
      bump("red", sprintf("region mismatch (set=%s, norm=%s, table=%s); tariff vintage differs, measured AS not comparable",
                          sr %||% "?", nr %||% "?", tr %||% "?"))
    else bump("amber", "no reference_hint; pairing plausible but unverified")
  }
  
  ## jurisdiction-agnostic references (e.g. full health, HRQoL = 1)
  ANY_REGION <- c("any")
  
  ## cross-jurisdiction norm vs table (independent of the hint), UNLESS
  ## either side is jurisdiction-agnostic ("any") -- then it's not a
  ## confound, just an omnibus reference (amber, handled below).
  if (!is.null(nr) && !is.null(tr) && !is.na(nr) && !is.na(tr) &&
      !(nr %in% ANY_REGION) && !(tr %in% ANY_REGION) && !identical(nr, tr))
    bump("red", sprintf("norm (%s) and life table (%s) are from different jurisdictions;
                        a pooled norm was survivor-weighted against <em>its own</em> table's sex-mix, so this off-diagonal pairing is doubly conditional \u2013 re-pool upstream for a coherent analysis", nr, tr))
  ## an "any" reference is usable but not a jurisdiction-matched pairing
  if ((!is.null(nr) && nr %in% ANY_REGION) || (!is.null(tr) && tr %in% ANY_REGION))
    bump("amber", "jurisdiction-agnostic reference in use (e.g. full health); omnibus, not a jurisdiction-matched pairing")
  ## forced / non-canonical object
  if (isFALSE(attr(norm, "canonical")) || isFALSE(attr(ltbl, "canonical")))
    bump("red", "a forced (non-container) reference is in use; provenance not guaranteed")
  
  ## discount override contradicting the set's native practice
  if (!is.null(override)) {
    native <- isTRUE(set$discount_shortfall)
    if (!identical(isTRUE(override), native))
      bump("red", sprintf("discount-shortfall override (%s) contradicts %s's native practice (%s)",
                          isTRUE(override), set$id, native))
  }
  list(status = status, reasons = reasons)
}

## Inlined BUILD-side machinery for the Threshold author tab. ###
## KEEP IN SYNC with shortfalls_R_aux/aux_sevset.R and aux_sevset_ext.R.
## The app is self-contained (deploy = this file + shortfalls_data.rds), so the
## editor's construct/validate/write logic is duplicated here rather than
## sourced. Read-side accessors are above; these are the write-side twins.

STATUS_LEVELS <- c("current","provisional","superseded","deprecated")

validate_sevset <- function(set) {
  err <- function(...) stop(sprintf("Severity regime '%s': %s", set$id %||% "?", paste0(...)), call. = FALSE)
  ## measures must be non-empty and match the bands present
  if (!length(set$measures) || !all(set$measures %in% c("AS","PS")) || anyDuplicated(set$measures))
    err("choose at least one measure (AS and/or PS)")
  if (!setequal(names(set$bands), set$measures))
    err("every chosen measure needs bands, and vice versa")
  
  ## rule must match the measure count:
  ##   two measures  -> a combining rule (max/min/mean)
  ##   one measure   -> that measure's own rule (AS or PS)
  if (!set$rule %in% c("max","min","mean")) err("Rule must be max/min/mean")
  if (length(set$measures) == 2) {
    if (!set$rule %in% c("max","min","mean"))
      err("With both AS and PS, a rule must be set for combining implied weights/multipliers (max/min/mean)")
  }
  # } else {  # exactly one measure
  #   if (!identical(set$rule, set$measures))
  #     err(sprintf("With one measure (%s), rule must be '%s'", set$measures, set$measures))
  #}
  if (set$rule %in% c("max","min","mean") && length(set$measures) < 1)
    err("rule '", set$rule, "' requires at least one measure")
  if (set$rule %in% c("AS","PS") && !identical(set$measures, set$rule))
    err("rule '", set$rule, "' requires measures == '", set$rule, "'")
  if (!set$weight_native %in% c("multiplier","ce_threshold")) err("weight_native invalid")
  if (identical(set$weight_native,"ce_threshold") && !is.finite(set$ce_threshold))
    err("ce_threshold must be finite when native")
  if (isTRUE(set$discount_shortfall) && !is.finite(set$discount_rate))
    err("discount_shortfall implies finite discount_rate")
  neutral <- if (identical(set$weight_native,"multiplier")) 1 else set$ce_threshold
  ok_exc  <- grepl("weight1-exception", set$provenance$notes %||% "", fixed = TRUE)
  for (m in set$measures) {
    b <- set$bands[[m]]
    if (!all(c("lower","upper","weight") %in% names(b))) err(m, ": band needs lower/upper/weight")
    if (!is.numeric(b$lower)||!is.numeric(b$upper)||!is.numeric(b$weight)) err(m, ": columns must be numeric")
    if (nrow(b) < 1) err(m, ": band empty")
    if (is.unsorted(b$lower, strictly = TRUE)) err(m, ": lower must strictly ascend")
    if (any(b$upper[-nrow(b)] != b$lower[-1])) err(m, ": bands not contiguous")
    if (b$lower[1] != 0) err(m, ": first lower must be 0")
    if (m == "AS" && !is.infinite(b$upper[nrow(b)])) err("AS: last upper must be Inf")
    if (m == "PS" && b$upper[nrow(b)] != 1) err("PS: last upper must be 1")
    if (is.unsorted(b$weight)) err(m, ": weight must be non-decreasing")
    if (m == "AS" && any(b$lower < 0)) err("AS: bands non-negative")
    if (m == "PS" && (b$lower[1] < 0 || b$upper[nrow(b)] > 1)) err("PS: bands in [0,1]")
    if (!isTRUE(all.equal(b$weight[1], neutral)) && !ok_exc)
      err(m, ": first-tier weight must equal neutral (", neutral, ") unless documented")
  }
  if (!(set$status %||% "current") %in% STATUS_LEVELS) err("status invalid")
  invisible(TRUE)
}

new_sevset_ui <- function(id, label, region, year, measures, rule, bands,
                          weight_native, ce_threshold = NA_real_, currency = NA_character_,
                          threshold_year = NA_integer_, discount_shortfall = FALSE,
                          discount_rate = NA_real_, reference_hint = list(),
                          derived_from = NA_character_, provenance = list()) {
  set <- structure(list(
    id = id, label = label, region = region, year = as.integer(year),
    measures = measures, rule = rule, bands = bands,
    weight_native = weight_native, ce_threshold = ce_threshold, currency = currency,
    threshold_year = threshold_year, discount_shortfall = discount_shortfall,
    discount_rate = discount_rate, status = "current", supersedes = NA_character_,
    reference_hint = reference_hint, reference_spec = list(),
    canonical = FALSE, derived_from = derived_from, provenance = provenance),
    class = "sevset")
  validate_sevset(set); set
}

## overlay path + non-overwriting write (never touches canonical shortfalls_data.rds)
SF_USER <- "shortfalls_data/shortfalls_data_user.rds"
fingerprint <- function(bands) paste(vapply(bands, function(b)
  paste(b$lower, b$upper, b$weight, collapse=";"), ""), collapse="||")

load_overlay <- function(path = SF_USER)
  if (file.exists(path)) readRDS(path) else list(sf_thresholds = list())

save_user_sevset <- function(set, path = SF_USER) {
  ov <- load_overlay(path)
  ov$sf_thresholds[[set$id]] <- set
  saveRDS(ov, path)
  invisible(set$id)
}




## =====================================================================#
## LOAD THE CONTAINER ####
## =====================================================================#
store <- load_shortfalls(SF)
## local mode: fold previously-authored regimes from the overlay into the
## store so they appear in the Severity-thresholds menu. Canonical ids are
## never overwritten (a user set that collided was refused at save time).
## Server mode starts from a clean stack -- no overlay read.
## Local mode: fold previously-authored objects from the overlay into the
## store. All four sublists, not just regimes -- conditions authored in the
## Author tab live here too. Canonical ids always win a collision, and an
## overlay object is marked non-canonical on the way in (a sevset carries
## the flag as a field, a condition as an attribute).


CANON <- list(sf_thresholds = store$sf_thresholds,
              hrqol_norms   = store$hrqol_norms,
              life_tables   = store$life_tables,
              sf_conditions = store$sf_conditions %||% list())


if (identical(APP_MODE, "local") && file.exists(SF_USER)) {
  ov <- tryCatch(readRDS(SF_USER), error = function(e) NULL)
  if (!is.null(ov))
    for (sub in c("sf_thresholds", "hrqol_norms", "life_tables", "sf_conditions"))
      for (id in names(ov[[sub]])) {
        if (id %in% names(store[[sub]])) next
        obj <- ov[[sub]][[id]]
        if (sub == "sf_thresholds") obj$canonical <- FALSE
        if (sub == "sf_conditions") attr(obj, "canonical") <- FALSE
        store[[sub]][[id]] <- obj
      }
}
DEF   <- attr(store, "defaults") %||%
  list(sevset = list_sevsets(store)[1],
       norm   = norm_ids(store)[1],
       table  = table_ids(store)[1])

## expand a stored (age,value) frame onto 0:TOP with flat tail extrapolation
gv <- function(df, ac, vc) {
  df <- as.data.frame(df); v <- numeric(TOP + 1); v[df[[ac]] + 1] <- df[[vc]]
  last <- max(df[[ac]]); if (last < TOP) v[(last + 2):(TOP + 1)] <- df[[vc]][which.max(df[[ac]])]
  v
}

## =====================================================================#
## COMPUTE  (norm is now EXPLICIT, so a triplet is genuinely swappable) ####
## ---------------------------------------------------------------------#
## discounted quality-adjusted expectancy from age a, given a mortality
## vector qmod (survival) and a norm vector qv (HRQoL), both on 0:TOP.
## COMPUTE (cached reference; discrete or continuous discounting) ####
## discount weight for time t (years from vantage), mode "cont"|"disc":
##   cont: exp(-r t)   (continuous, DEFAULT)
##   disc: (1+r)^(-t)  (geometric, year-wise -- legacy, "at your own peril")
.disc_w <- function(t, r, mode) if (mode == "cont") exp(-r * t) else (1 + r)^(-t)

## discounted quality-adjusted expectancy from age a, given mortality qmod
## (survival) and HRQoL qv, both on 0:TOP. t = 0,1,2,... years from a.
eq_from <- function(a, qmod, qv, r, mode) {
  ages <- a:TOP
  S <- c(1, cumprod(1 - qmod[a:(TOP)][-length(ages)]))
  sum(S * qv[ages + 1] * .disc_w(0:(length(ages) - 1), r, mode))
}

## reference QALE Q(a) for every plotted age, in ONE pass. Depends only on
## (qx, qv, r, mode) -- NOT on W or the sevset. This is the cached reference.
ref_vec <- function(ages_plot, qx, qv, r, mode)
  vapply(ages_plot, eq_from, 0, qmod = qx, qv = qv, r = r, mode = mode)

## shortfall at age a, given the ALREADY-COMPUTED reference Q. Only the
## with-condition arm s depends on W. Sub-year survived fraction is now
## discounted at its midpoint (continuous) / (1+r)^(-W/2) (geometric),
## instead of undiscounted -- removing the old conservative bias.
sf_Q <- function(a, W, r, qx, qv, mode, Q) {
  wholeW <- floor(W); frac <- W - wholeW
  if (wholeW >= 1) {
    qmod <- qx; idx <- 0:TOP
    qmod[idx >= a & idx < a + wholeW] <- 0
    qmod[idx >= a + wholeW] <- 1
    s <- eq_from(a, qmod, qv, r, mode)
    if (frac > 0 && a + wholeW <= TOP)
      s <- s + frac * qv[a + wholeW + 1] * .disc_w(wholeW, r, mode)
  } else {
    ## W < 1: survive fraction W of the first year, discounted at midpoint
    s <- frac * qv[a + 1] * .disc_w(W / 2, r, mode)
  }
  c(AS = Q - s, PS = (Q - s) / Q)
}

## first age where decreasing series y crosses thr (linear interp)
cross_at <- function(ages, y, thr) {
  below <- which(y < thr)
  if (length(below) == 0 || below[1] == 1) return(NA_real_)
  i <- below[1]; x0 <- ages[i-1]; x1 <- ages[i]; y0 <- y[i-1]; y1 <- y[i]
  x0 + (thr - y0) * (x1 - x0) / (y1 - y0)
}

AGES  <- 1:90 ## age span plotted on x-axis
LINE  <- pal[['SPBlue']]        # the single line (dark blue). Light blue reserved
# for the future "compare with" second line.

## CONDITION MACHINERY (ported from the condition-shortfall app) ####
## ---------------------------------------------------------------------#
## Ported verbatim except that build_stack now takes the reference
## vectors as ARGUMENTS (qxv, qvv) instead of closing over module-level
## globals -- the reference is reactive here, driven by the shared
## triplet, so it cannot be a constant. compare_table takes the sevset
## and reports ITS weight via applied_weight(), replacing the hardcoded
## NICE thresholds of the standalone app.

## stack fill / stroke palettes for the QALE stack views
BAND <- list(
  norm = pal[["SPGreenLight"]],   # reference potential ceiling
  mort = pal[["SPRed"]],          # mortality shortfall
  morb = pal[["SPYellow"]],       # morbidity shortfall
  disc = pal[["SPBlueLightest"]], # discounting loss (cool, neutral)
  qale = pal[["SPGreen"]])        # delivered discounted QALE (floor)
STROKE <- list(
  surv = pal[["SPBlue"]],  ref = pal[["SPGreen"]], cond = pal[["SPRed"]],
  disc = pal[["SPBlue"]],  onset = pal[["SPPurple"]], active = pal[["SPPurple"]])

## knots -> a full 0:TOP vector (flat outside the knot span, rule = 2)
interp_knots <- function(knots, base_value, method = "linear") {
  k <- knots[order(knots$age), , drop = FALSE]
  k <- k[!duplicated(k$age), , drop = FALSE]
  if (nrow(k) == 0) return(rep(base_value, TOP + 1))
  stats::approx(k$age, k$value, xout = 0:TOP, method = method, rule = 2, f = 0)$y
}

## apply an odds ratio to a one-year death probability: [0,1] -> [0,1]
or_hit <- function(q, OR) OR * q / (1 - q + OR * q)

## the QALE stack from OR / HRQoL-scale age-vectors, at vantage a.
## qxv = reference mortality on 0:TOP, qvv = reference HRQoL on 0:TOP.
build_stack <- function(a, or_v, m_v, r, qxv, qvv, mode = "cont", top_plot = 105) {
  K   <- max(1, min(top_plot, TOP) - a)
  j   <- 0:K; age <- a + j
  qref <- qvv[age + 1]; qx <- qxv[age + 1]
  qxc  <- or_hit(qx, or_v[age + 1]); qc <- m_v[age + 1] * qref
  surv <- function(qv) c(1, cumprod(1 - qv[-length(qv)]))
  Sref <- surv(qx); Sc <- surv(qxc); disc <- .disc_w(j, r, mode)
  Csq <- Sref * qref; Scm <- Sc * qref; Scc <- Sc * qc; S <- sum
  list(j = j, K = K, a = a, r = r,
       Sref = Sref, Csq = Csq, Scm = Scm, Scc = Scc, Scd = Scc * disc,
       Sref_d = Sref * disc, Csq_d = Csq * disc, Scm_d = Scm * disc,
       QALEref = S(Csq), QALEc = S(Scc),
       AS = S(Csq) - S(Scc), PS = (S(Csq) - S(Scc)) / S(Csq),
       mort = S(Csq - Scm), morb = S(Scm - Scc),
       AS_d = S(Csq * disc) - S(Scc * disc),
       PS_d = (S(Csq * disc) - S(Scc * disc)) / S(Csq * disc),
       QALEc_d = S(Scc * disc), QALEref_d = S(Csq * disc))
}

## draw one stack. disc_view = TRUE puts every curve (reference included)
## on the discounted footing.
draw_stack <- function(x, bands = TRUE, main = "", disc_view = FALSE) {
  j <- x$j; K <- x$K
  Sref <- if (disc_view) x$Sref_d else x$Sref
  Csq  <- if (disc_view) x$Csq_d  else x$Csq
  Scm  <- if (disc_view) x$Scm_d  else x$Scm
  Scc  <- if (disc_view) x$Scd    else x$Scc
  par(mar = c(3.4, 1, if (nzchar(main)) 1.8 else 0.5, 1))
  ## the ceiling is 1 under non-negative discounting (every curve is a
  ## survival-weighted quantity), but rho < 0 inflates the discounted
  ## curves above 1, so let the top follow the data.
  ytop <- max(1, Sref, Csq, Scm, Scc, if (!disc_view) x$Scd else 0, na.rm = TRUE)
  plot(NULL, xlim = c(0, K), ylim = c(0, ytop), xlab = "", ylab = "",
       axes = FALSE, main = main, family = "serif")
  poly <- function(y, col) polygon(c(0, j, K), c(0, y, 0), border = NA, col = col)
  if (bands) {
    poly(Sref, adjustcolor(BAND$norm, 0.30))
    poly(Csq,  adjustcolor(BAND$mort, 0.60))     # [Scm,Csq] mortality shortfall
    poly(Scm,  adjustcolor(BAND$morb, 0.60))     # [Scc,Scm] morbidity shortfall
    if (disc_view) {
      poly(Scc, adjustcolor(BAND$qale, 0.55))    # arm already discounted
    } else {
      poly(Scc,   adjustcolor(BAND$disc, 0.75))  # [Scd,Scc] discounting loss
      poly(x$Scd, adjustcolor(BAND$qale, 0.55))  # delivered discounted QALE
    }
    lines(j, Csq, col = STROKE$ref,  lwd = 2)
    lines(j, Scc, col = STROKE$cond, lwd = 2)
    ## Overlaid view: BOTH arms get their discounted twin, so the gap
    ## between the dashed pair is AS_d exactly as the gap between the
    ## solid pair is AS. Convention: colour = which arm, dashed = discounted.
    if (!disc_view) {
      lines(j, x$Csq_d, col = STROKE$ref,  lwd = 1.3, lty = 2)   # reference, discounted
      lines(j, x$Scd,   col = STROKE$cond, lwd = 1.3, lty = 2)   # condition, discounted
    }
  } else {
    poly(Sref, adjustcolor(BAND$norm, 0.30))
    poly(Csq,  adjustcolor(BAND$qale, 0.45))
    lines(j, Csq, col = STROKE$ref, lwd = 2)
  }
  lines(j, Sref, col = STROKE$surv, lwd = 1.5)
  at <- seq(0, K, by = 10); axis(1, at = at, labels = x$a + at)
  mtext("age  (vantage a + t)", side = 1, line = 2.3, col = "#666", cex = 0.9)
}


## VALUE-POTENTIAL MACHINERY (ported from the value-potential app) ####
## ---------------------------------------------------------------------#
## A flat unit stream of years over a horizon t, discounted at force f.
## No HRQoL, no severity regime: the discounting kernel on its own.
##
## The kernel is written in CONTINUOUS form. Year-wise discounting at rate
## r is the same kernel at force log(1 + r), so the shared disc radio is
## handled by choosing the force, not by branching here.

## PV share held by the FIRST tau years of a horizon t
near_share <- function(tau, t, f) {
  if (abs(f) < 1e-9) return(tau / t)          # undiscounted: value share = time share
  (1 - exp(-f * tau)) / (1 - exp(-f * t))
}

## inverse: how many near years hold a target PV share phi
near_years <- function(phi, t, f) {
  if (abs(f) < 1e-9) return(phi * t)
  -log(1 - phi * (1 - exp(-f * t))) / f
}

## balance point: the head time-fraction p at which the tail's VALUE share
## equals the head's TIME share. Returns the head time-fraction; r' (the
## head's value share) is 1 - p.
head_frac <- function(u) {                    # u = f * t
  if (abs(u) < 1e-6) return(0.5)
  Phi <- function(p) (1 - exp(-u * p)) / (1 - exp(-u))
  uniroot(function(p) Phi(p) - (1 - p), c(1e-9, 1 - 1e-9), tol = 1e-12)$root
}

## remaining life expectancy at age a from a mortality vector on 0:TOP.
## Half-year correction on the survivor sum, as in the source app.
e_at <- function(a, qxv) {
  a <- floor(a)
  if (!is.finite(a) || a < 0 || a >= TOP) return(NA_real_)
  S <- cumprod(c(1, 1 - qxv[(a + 1):TOP]))
  sum(S) - 0.5
}

## shared comparison table. The weight row is REGIME-SENSITIVE: it uses
## applied_weight() on the selected sevset, and names the regime, rather
## than hardcoding NICE's 12/18 + .85/.95.
compare_table <- function(x, header, set, regime_label = NULL) {
  cell   <- function(v) tags$td(style = "text-align:right;padding:5px 16px;font-variant-numeric:tabular-nums;", v)
  hd     <- function(v) tags$th(style = "text-align:right;padding:5px 16px;font-weight:600;", v)
  rowlab <- function(v) tags$td(style = "text-align:left;padding:5px 16px;color:#555;", v)
  w0 <- applied_weight(x$AS,   x$PS,   set)
  wd <- applied_weight(x$AS_d, x$PS_d, set)
  wlab <- paste0(regime_label %||% (set$label %||% set$id), " weight")
  tags$table(style = "border-collapse:collapse;margin:10px 0 16px;font-size:16px;",
    tags$thead(tags$tr(hd(header), hd("undiscounted"),
                       hd(sprintf("discounted (\u03c1 = %.1f%%)", 100 * x$r)))),
    tags$tbody(
      tags$tr(rowlab("reference QALE"), cell(sprintf("%.1f", x$QALEref)), cell(sprintf("%.1f", x$QALEref_d))),
      tags$tr(rowlab("condition QALE"), cell(sprintf("%.1f", x$QALEc)),   cell(sprintf("%.1f", x$QALEc_d))),
      tags$tr(style = "border-top:1px solid #ddd;",
              rowlab(tags$b("absolute shortfall (AS)")),
              cell(tags$b(sprintf("%.1f", x$AS))), cell(tags$b(sprintf("%.1f", x$AS_d)))),
      tags$tr(rowlab(tags$b("proportional shortfall (PS)")),
              cell(tags$b(sprintf("%.0f%%", 100 * x$PS))), cell(tags$b(sprintf("%.0f%%", 100 * x$PS_d)))),
      tags$tr(style = "border-top:1px solid #ddd;",
              rowlab(wlab), cell(sprintf("%.1f", w0)), cell(sprintf("%.1f", wd)))))
}

## ===================================================================== ##
## UI User Interface ####
ui <- fluidPage(
  tags$head(tags$style(HTML(sprintf(
    ".irs-bar,.irs-single{background:%s!important;border-color:%s!important}
     .badge-green{color:%s} .badge-amber{color:%s} .badge-red{color:%s}
     body{font-family:%s}
     h1,h2,h3,h4,h5,h6,.title{font-family:%s}
     .control-label{font-family:%s}",
    pal[['SPBlue']], pal[['SPBlue']],
    pal[['SPGreen']], pal[['SPYellow']], pal[['SPRed']],
    FONT_BODY, FONT_HEAD, FONT_HEAD)))),
  titlePanel("Shortfall and severity explorer"),

  ## SHARED HEADER: the triplet and its badge govern EVERY tab. Choose a
  ## regime / norm / table once; the visualiser, the condition explorer
  ## and both authoring tabs all read the same selection.
  fluidRow(
    column(3, selectInput("sevset", "Severity Regime",
                          choices = stats::setNames(list_sevsets(store),
                                                    vapply(list_sevsets(store),
                                                           function(id) sevset_label(store, id), "")),
                          selected = DEF$sevset)),
    column(3, selectInput("norm", "HRQoL norm",
                          choices = .labels_for(store$hrqol_norms), selected = DEF$norm)),
    column(3, selectInput("table", "Life table",
                          choices = .labels_for(store$life_tables), selected = DEF$table)),
    column(1,
           tags$label("Condition as", tags$br(), "reference", class = "control-label"),
           div(title = "Enable the use of life tables and HRQoL from conditions as shortfall reference.
               Can be used to explore excess shortfall from co-morbidities.",
               materialSwitch("use_cond_ref", label = NULL, value = FALSE,
                              status = "danger"))
    ),
    column(1, div(style = "margin-top:25px;",
                  actionButton("reload", "Refresh", title =
                                 "Re-read authored regimes and conditions from disk")))
  ),
  
  fluidRow(
    column(6, sliderInput("r", "Discount rate \u03c1", min = 0, max = 0.10,
                          value = 0.035, step = 0.001, width = "100%")),
    column(2,
           radioGroupButtons("disc", "Discounting",
                             c("Continuous" = "cont", "Year-wise" = "disc"),
                             selected = "cont", size = "sm")),
    column(2, numericInput("r_max", "\u03c1 slider max", value = 0.10,
                           min = 0.01, max = 1, step = 0.01)),
    column(1,
           tags$label("Allow \u03c1 < 0", class = "control-label"),
           div(materialSwitch("r_neg", label = NULL, value = FALSE,
                              status = "warning")))
  ),
  fluidRow(
    column(10, uiOutput("rho_warn"))
  ),
  
  fluidRow(
    column(9, div(style="font-size:18px;margin-top:-6px;margin-bottom:8px;",
                  uiOutput("badge")))
  ),

  tabsetPanel(
    tabPanel("Severity regime explorer",
             br(),
             fluidRow(column(3,h3('Severity regime explorer'))),
             fluidRow(
               column(6,
                      sliderInput("rs", "RS (remaining survival)", min = 0, max = 40,
                                  value = 5, step = 1, width = "100%"),
                      div(style=sprintf("font-family:%s;font-size:11px;color:#999;margin-top:-6px;", FONT_BODY),
                          textOutput("rs_hint", inline = TRUE))),
               column(1,
                      tags$label("Crossing guides", class = "control-label"),
                      div(materialSwitch("show_cross", label = NULL, value = FALSE,
                                         status = "success")))),
             plotlyOutput("plot", height = "680px"),
             div(style="color:#666;font-size:12px;margin-top:8px;",
                 "Vantage = onset. Guaranteed W survival, then certain death; HRQoL = norm. ",
                 "Solid = AS (left), dotted = PS (right). Guides drawn from the chosen regime's bands. ",
                 "Continuous discounting e^(-\u03c1 t) by default; sub-year survival discounted at midpoint. ",
                 "Year-wise (1+\u03c1)^(-t) available but understates sub-year shortfall. ",
                 "Age axis from 1; y-axis rescales with \u03c1."),
             div(style="color:#999;font-size:11px;margin-top:2px;", textOutput("src"))
    ),
    tabPanel("Severity regime editor",
             br(),
             fluidRow(column(6,h3('Severity regime editor'))),
             fluidRow(
               column(3,
                      selectInput("ed_fork", "Start from",
                                  choices = stats::setNames(list_sevsets(store, TRUE),
                                                            vapply(list_sevsets(store, TRUE),
                                                                   function(id) sevset_label(store, id), ""))),
                      actionButton("ed_load", "Load into editor"),
                      hr(),
                      textInput("ed_id",    "New id (unique)", ""),
                      textInput("ed_label", "Display label", ""),
                      checkboxGroupInput("ed_meas", "Measures",
                                         c("AS","PS"), selected = c("AS","PS"), inline = TRUE),
                      uiOutput("ed_rule_ui"),
                  
                      selectInput("ed_wn", "Weight native", c("multiplier","ce_threshold")),
                      hr(),
                      uiOutput("ed_saveui"),         # Save (local) or Download (server)
                      hr(),
                      uiOutput("ed_delui")           # Delete severity regime (user-authored only)
               ),
               column(9,
                      h4("AS bands"), uiOutput("ed_AS_grid"),
                      actionButton("ed_AS_add", "+ AS band"), actionButton("ed_AS_del", "- AS band"),
                      hr(),
                      h4("PS bands"), uiOutput("ed_PS_grid"),
                      actionButton("ed_PS_add", "+ PS band"), actionButton("ed_PS_del", "- PS band"),
                      hr(),
                      div(style="font-family:monospace;font-size:12px;", uiOutput("ed_valid"))
               )
             )
    ),

    ## ---- Condition explorer -----------------------------------------
    tabPanel("Condition explorer",
             br(),
             fluidRow(column(6,h3('Condition explorer'))),
             fluidRow(
               column(4, selectInput("cond", "Condition",
                                     choices = c("manual (sliders)" = "__manual__"),
                                     selected = "__manual__")),
               column(2,
                      radioGroupButtons("view", "Layout",
                                        c("Overlaid" = "ov", "Side-by--side" = "sbs"),
                                        selected = "sbs", size = "sm")),
               column(4, sliderInput("age", "Vantage age a",
                                     min = 0, max = 90, value = 35, step = 1))
             ),
             conditionalPanel("input.cond == '__manual__'",
               fluidRow(
                 column(4, sliderInput("onset", "Onset age (a')", min = 0, max = 100,
                                       value = 45, step = 1)),
                 column(4, sliderInput("m", "HRQoL scale from onset (1 = none)",
                                       min = 0.1, max = 1, value = 0.6, step = 0.05)),
                 column(4, sliderInput("or_log", "log\u2082-mortality OR from onset (0 = no excess)",
                                       min = 0, max = 10, value = 1, step = 0.025)),
               ),
               div(style = "color:#666;font-size:12px;margin-top:-6px;",
                   textOutput("or_readout"))),
             div(style = sprintf("font-family:%s;font-size:12px;color:#666;margin:4px 0;", FONT_BODY),
                 uiOutput("ex_ref_note")),
             plotOutput("explore_plot", height = "330px"),
             uiOutput("legend"),
             uiOutput("compare"),
             uiOutput("cond_info")
    ),

    ## ---- Condition Editor-------------------------------------------
    tabPanel("Condition editor",
             br(),
             fluidRow(column(1,h3('Condition editor')),
                      column(1,actionButton("cond_reset", "Reset condition",class = "btn-danger",
                                            style = "margin-top:25px;width:100%;"))),
             fluidRow(
               column(6,
                      h4("Mortality OR profile"),
                      helpText("Age 0 pinned at OR 1; age", TOP, "inherits the last knot. ",
                               "Click a marker to activate. Double-click the plot to add a ",
                               "knot, or double-click a marker to remove it."),
                     
                        
                      fluidRow(
                        column(2, selectInput("or_rule", "Between knots",
                                              c("shock (step)" = "constant",
                                                "ramp (linear)" = "linear"),
                                              selected = "constant")),
                        column(5, sliderInput("or_age", "Active knot age",
                                              min = 1, max = TOP - 1, value = 30,
                                              step = 1, width = "100%")),
                        column(5, sliderInput("or_slider", "Active knot OR (log\u2082)",
                                              min = 0, max = 10, value = 1,
                                              step = 0.05, width = "100%"))),
                      uiOutput("or_age_ui"),
                      plotOutput("or_preview", height = "170px",
                                 click = "or_click", dblclick = "or_dbl")),
               column(6,
                      h4("HRQoL scale profile"),
                      helpText("Age 0 pinned at scale 1; age", TOP, "inherits the last knot. ",
                               "Click a marker to activate. Double-click the plot to add a ",
                               "knot, or double-click a marker to remove it."),
                      fluidRow(
                        column(2, selectInput("m_rule", "Between knots",
                                              c("shock (step)" = "constant",
                                                "ramp (linear)" = "linear"),
                                              selected = "constant")),
                        column(5, sliderInput("m_age", "Active knot age",
                                              min = 1, max = TOP - 1, value = 30,
                                              step = 1, width = "100%")),
                        column(5, sliderInput("m_slider", "Active knot scale",
                                              min = 0.05, max = 1, value = 0.6,
                                              step = 0.01, width = "100%"))),
                      plotOutput("m_preview", height = "170px",
                                 click = "m_click", dblclick = "m_dbl"))
             ),
             hr(),
             h5("Preview against the selected reference life table and HRQoL-norm:"),
             fluidRow(
               column(4, sliderInput("a_auth", "Vantage age", min = 0, max = 90,
                                     value = 0, step = 1)),
               column(2,
                      radioGroupButtons("auth_view", "Layout",
                                        c("Overlaid" = "ov", "Side-by--side" = "sbs"),
                                        selected = "sbs", size = "sm"))
             ),
             plotOutput("auth_plot", height = "300px"),
             uiOutput("auth_compare"),
             hr(),
             h5("Save condition"),
             fluidRow(
               column(4, textInput("cond_label", "Menu label", value = "New condition")),
               column(4, textInput("cond_id", "Container id", value = "new_condition")),
               column(4, div(style = "margin-top:25px;", uiOutput("cond_saveui")))
             ),
             fluidRow(
               column(12, textInput("cond_summary", "Info summary", value = "",
                                    width = "100%"))),
             fluidRow(
               column(6, textInput("cond_cite", "Source citation", value = "")),
               column(6, textInput("cond_url", "Source url", value = ""))),
             div(style = "color:#666;font-size:12px;", uiOutput("export_msg")),
             fluidRow(
               column(2, actionButton("edit_load_go", "Load", class = "btn-primary",
                                      style = "margin-top:25px;width:100%;")),
               column(5, selectInput("edit_load", "Load condition into editor",
                                     choices = c("(none)" = ""), width = "100%")),
               column(5, div(style = "margin-top:28px;font-size:12px;",
                             uiOutput("edit_load_msg")))
             ),
             fluidRow(
               column(2, uiOutput("cond_delbtn")),
               column(5, uiOutput("cond_delsel")),
               column(5, div(style = "margin-top:28px;font-size:12px;",
                             uiOutput("cond_delmsg")))
             )
             
    ),
    ## ---- Value potential --------------------------------------------
    tabPanel("Value potential",
             br(),
             fluidRow(column(6,h3('Value potential visualiser'))),
             tags$p(style = "color:#666;",
                    "A flat unit stream of years over a horizon, discounted at the shared ",
                    "\u03c1. No HRQoL and no severity regime \u2013 the discounting kernel on ",
                    "its own, showing where the present value of a future is located."),
             fluidRow(
               column(2,radioGroupButtons(
                 "vp_hmode", "Horizon set by",
                 c("years" = "t", "age \u2192 e(age)" = "a"),
                 selected = "a", size = "sm")),
               column(2, conditionalPanel(
                 "input.vp_hmode == 't'",
                 numericInput("vp_T", "Horizon t (years, any t > 0)",
                              value = 53, min = 0.5, step = 1))),
               column(4, conditionalPanel(
                 "input.vp_hmode == 'a'",
                 sliderInput("vp_age", "Age (years)", min = 0, max = 90,
                             value = 32, step = 1)))
             ),
             sliderInput("vp_N", "Near window: first N years", min = 0, max = 60,
                         value = 10, step = 0.5, width = "100%"),
             plotOutput("vp_plot", height = "300px"),
             uiOutput("vp_sentence"),
             fluidRow(
               column(4, wellPanel(tags$small("balance point r\u2032"),
                                   tags$h3(textOutput("vp_rp", inline = TRUE)))),
               column(4, wellPanel(uiOutput("vp_nsh_label"),
                                   tags$h3(textOutput("vp_nsh", inline = TRUE)))),
               column(4, wellPanel(tags$small("product f \u00d7 t"),
                                   tags$h3(textOutput("vp_u", inline = TRUE))))
             ),
             div(style = "color:#888;font-size:12px;", textOutput("vp_note"))
    )
    
  )
)

## SERVER ####
server <- function(input, output, session) {
  
  ## -- the chosen triplet -------------------------------------------##
  set  <- reactive(sevsets_r()[[input$sevset]])
  ## Split a condition into its two halves on demand. NOT stored in the
  ## container: the halves must track the condition, and a duplicate would
  ## drift the moment the condition were re-authored.
  ##
  ## canonical = FALSE is deliberate -- compatible() already reds on a
  ## forced non-container reference, so the experimental status is announced
  ## by machinery that exists rather than by a new rule.
  .cond_half <- function(id, what) {
    o <- conds_r()[[sub("^cond:", "", id)]]
    if (is.null(o)) return(NULL)
    lab <- attr(o, "menu_label") %||% id
    src <- paste0("DERIVED from condition '", lab,
                  "' \u2013 not a published reference")
    if (identical(what, "norm"))
      structure(data.frame(age = o$age, hrqol = o$hrqol),
                region = "derived", sex = "any", valueset = "derived",
                source = src, label = paste0("[condition] ", lab),
                canonical = FALSE)
    else
      structure(data.frame(a = o$age, mu = o$mu),
                region = "derived", sex = "any",
                source = src, label = paste0("[condition] ", lab),
                canonical = FALSE)
  }
  
  nmO <- reactive({
    id <- input$norm %||% ""
    if (startsWith(id, "cond:")) .cond_half(id, "norm") else norms_r()[[id]]
  })
  tbO <- reactive({
    id <- input$table %||% ""
    if (startsWith(id, "cond:")) .cond_half(id, "table") else tbls_r()[[id]]
  })
  qx   <- reactive(gv(tbO(), "a",  "mu"))
  qv   <- reactive(gv(nmO(), "age","hrqol"))
  
  sevsets_r <- reactiveVal(store$sf_thresholds)
  norms_r   <- reactiveVal(store$hrqol_norms)
  tbls_r    <- reactiveVal(store$life_tables)
  conds_r   <- reactiveVal(store$sf_conditions %||% list())
  
  refresh_store <- function() {
    out <- CANON
    if (identical(APP_MODE, "local") && file.exists(SF_USER)) {
      ov <- tryCatch(readRDS(SF_USER), error = function(e) NULL)
      if (!is.null(ov))
        for (sub in names(CANON))
          for (id in names(ov[[sub]])) if (!id %in% names(out[[sub]])) {
            o <- ov[[sub]][[id]]
            if (sub == "sf_thresholds") o$canonical <- FALSE
            else attr(o, "canonical") <- FALSE
            out[[sub]][[id]] <- o
          }
    }
    sevsets_r(out$sf_thresholds); norms_r(out$hrqol_norms)
    tbls_r(out$life_tables);      conds_r(out$sf_conditions)
  }
  
  ## turning the switch off must not strand a cond: selection
  observeEvent(input$use_cond_ref, {
    if (isTRUE(input$use_cond_ref)) return()
    if (startsWith(input$norm  %||% "", "cond:"))
      updateSelectInput(session, "norm",  selected = DEF$norm)
    if (startsWith(input$table %||% "", "cond:"))
      updateSelectInput(session, "table", selected = DEF$table)
  }, ignoreInit = TRUE)
  
  ## every selector that lists a container sublist, refreshed together
  observe({
    sv <- sevsets_r(); cd <- conds_r()
    lab  <- function(l) vapply(names(l), function(i) l[[i]]$label %||% i, "")
    keep <- vapply(sv, function(s) (s$status %||% "current") %in%
                     c("current", "provisional"), logical(1))
    updateSelectInput(session, "sevset",
                      choices  = stats::setNames(names(sv)[keep], lab(sv)[keep]),
                      selected = isolate(input$sevset) %||% DEF$sevset)
    updateSelectInput(session, "ed_fork",
                      choices  = stats::setNames(names(sv), lab(sv)),
                      selected = isolate(input$ed_fork))
    ## A stored condition is (age, hrqol, mu): its mu column IS a life table
    ## and its hrqol column IS a norm. When the switch is on, both halves are
    ## offered alongside the published references, prefixed "cond:" so that
    ## nmO/tbO can tell them apart. They are independently selectable, so a
    ## chimera is possible -- one condition's mortality with a population
    ## norm. That computes, and the badge says so; forbidding it would be the
    ## app taking a stance it does not take elsewhere.
    extra <- character(0)
    if (isTRUE(input$use_cond_ref) && length(cd)) {
      cl    <- .labels_for(cd)          # names = display label, values = id
      extra <- stats::setNames(paste0("cond:", unname(cl)),
                               paste0("[condition] ", names(cl)))
    }
    updateSelectInput(session, "norm",
                      choices  = c(.labels_for(norms_r()), extra),
                      selected = isolate(input$norm)  %||% DEF$norm)
    updateSelectInput(session, "table",
                      choices  = c(.labels_for(tbls_r()), extra),
                      selected = isolate(input$table) %||% DEF$table)
    updateSelectInput(session, "cond",
                      choices  = c("manual (sliders)" = "__manual__", .labels_for(cd)),
                      selected = isolate(input$cond) %||% "__manual__")
    updateSelectInput(session, "edit_load",
                      choices  = c("(none)" = "", .labels_for(cd)),
                      selected = isolate(input$edit_load))
  })
  
  ## cached reference QALE over plotted ages. Recomputes ONLY on
  ## qx / qv / rho / discounting-mode -- NOT on W or sevset.
  Qref <- reactive(ref_vec(AGES, qx(), qv(), input$r, input$disc))
  
  # # plot_height <- reactiveVal("plot_height")
  # output$plot_height <- reactive({
  #   ph <- input$plot_height
  #   req(is.numeric(as.numeric(ph)) && ph >= 240)
  #   ph
  # })

  observeEvent(input$reload, {
    refresh_store()
    showNotification("Refreshed from disk", type = "message")
  })
  
  ## Remaining survival slider####
  ## unit state is authoritative; the slider value is read IN that unit.
  rs_unit <- reactiveVal("yr")            # "yr" | "mo" | "da"
  
  
  ## regime table: max, step, and the land-value when ENTERING each unit
  .rs_regime <- list(
    yr = list(min = 0, max = 40, step = 1,  land = 5,  lab = "RS (years)"),
    mo = list(min = 1, max = 24, step = 1,  land = 12, lab = "RS (months)"),
    da = list(min = 0, max = 90, step = 1,  land = 30, lab = "RS (days)"))
  
  .set_rs <- function(u, land = NULL) {
    g <- .rs_regime[[u]]; rs_unit(u)
    updateSliderInput(session, "rs", label = g$lab,
                      min = g$min, max = g$max, step = g$step,
                      value = if (is.null(land)) g$land else land)
  }
  
  
  ## downshift at value == 1 (zoom in); upshift with a dead band (zoom out)
  ## at > 60 days and > 22 months, so the scale never flaps on the boundary.
  observeEvent(input$rs, {
    u <- isolate(rs_unit()); v <- input$rs
    if (is.null(v)) return(invisible())
    if (u == "yr" && v <= 1)              .set_rs("mo")
    else if (u == "mo") {
      if (v <= 1)                         .set_rs("da")
      else if (v > 22)                    .set_rs("yr", land = 2)
    } else if (u == "da" && v > 60)       .set_rs("mo", land = 3)
  }, ignoreInit = TRUE)
  
  ## rho cap: rescale the shared slider, clamp the current value into range.
  ## Capped at 1.0 -- the degenerate corner, where the discount weights
  ## collapse onto the present and the shortfall very nearly vanishes.
  ## rho range. The cap is clamped to [0.01, 1] and written BACK to the
  ## field, so an out-of-range entry corrects itself visibly instead of
  ## being silently ignored. rho = 1 is the degenerate corner: the weights
  ## collapse onto the present and severity all but vanishes.
  observeEvent(list(input$r_max, input$r_neg), {
    cap <- input$r_max
    if (!isTruthy(cap) || !is.finite(cap)) return()
    cap_ok <- max(0.01, min(1, cap))
    if (!isTRUE(all.equal(cap_ok, cap)))
      updateNumericInput(session, "r_max", value = cap_ok)
    lo <- if (isTRUE(input$r_neg)) -cap_ok else 0
    updateSliderInput(session, "r", min = lo, max = cap_ok,
                      step = signif(cap_ok / 100, 1),
                      value = min(max(isolate(input$r), lo), cap_ok))
  }, ignoreInit = TRUE)

  ## regime-driven rho: choosing a regime moves the slider to that regime's
  ## native practice -- NICE 0.035, Norway and ZiN undiscounted so 0. A
  ## default, not a lock: the slider stays free, so the comparative move
  ## (switch regime, watch discounting stop) needs no prior knowledge.
  ## Authored sets carry the same fields, so they participate unchanged.
  observeEvent(input$sevset, {
    s <- set()
    if (is.null(s)) return()
    rho <- if (isTRUE(s$discount_shortfall)) s$discount_rate %||% 0 else 0
    if (!is.finite(rho)) rho <- 0
    cap <- max(0.01, min(1, isolate(input$r_max) %||% 0.10))
    lo  <- if (isTRUE(isolate(input$r_neg))) -cap else 0
    updateSliderInput(session, "r", value = min(max(rho, lo), cap))
  }, ignoreInit = TRUE)
  
  output$rho_warn <- renderUI({
    if (!isTruthy(input$r) || !is.finite(input$r) || input$r >= 0) return(NULL)
    div(style = sprintf("font-family:%s;color:%s;font-size:12px;margin-top:-4px;",
                        FONT_BODY, pal[['SPRed']]),
        HTML(sprintf("\u25B2 <b>Experimental:</b> \u03c1 = %.3f weights distant life-years <em>above</em> near ones. No jurisdiction's reference case permits this; shown to bracket the operator's effect, not as a policy option.", input$r)))
  })
  
  
  Wyears <- reactive({
    v <- input$rs; u <- rs_unit()
    if (is.null(v) || !is.finite(v)) return(NULL)
    y <- switch(u, yr = v, mo = v/12, da = v/365.25)
    if (!is.finite(y) || y <= 0) return(0.01)
    y
  })

  wlab <- reactive({
    v <- input$rs
    switch(rs_unit(),
           yr = sprintf("%g yr", v),
           mo = sprintf("%g mo", v),
           da = sprintf("%g da", v))
  })
  
  wlab_long <- reactive({
    v <- input$rs
    u <- switch(rs_unit(),
                yr = if (v == 1) "year"  else "years",
                mo = if (v == 1) "month" else "months",
                da = if (v == 1) "day"   else "days")
    sprintf("%g %s", v, u)
  })
  
  output$src <- renderText({
    paste0("table: ", attr(tbO(),"source") %||% "user",
           "  |  norm: ", attr(nmO(),"source") %||% "user",
           "  |  regime: ", set()$label %||% set()$id)
  })
  
  output$rs_hint <- renderText({
    switch(rs_unit(),
           yr = "drag to 1 to switch to months",
           mo = "drag to 1 -> days;  above 22 -> years",
           da = "above 60 -> months")
  })
  
  ## -- compatibility badge (never blocks) --------------------------##
  output$badge <- renderUI({
    cmp <- compatible(set(), nmO(), tbO(), input$norm, input$table)
    cls <- paste0("badge-", cmp$status)
    dot <- c(green="\u25CF Fully consistent pairing",
             amber="\u25B2 Pausible \u2013 unverified",
             red  ="\u25B2 Experimental/Off-label")[[cmp$status]]
    tagList(
      span(class = cls, style="font-weight:600;", dot),
      if (length(cmp$reasons))
        span(style="color:#666;", HTML(paste0(" \u2013 ",
                                              paste(cmp$reasons, collapse="; "))))
    )
  })
  
  
  ## -- the plot: ONE horizon, ONE line, guides from the sevset ------##
  output$plot <- renderPlotly({
    r <- input$r; W <- Wyears(); s <- set()
    if (is.null(W)) return(NULL)
    qxv <- qx(); qvv <- qv()
    
    Qv <- Qref()
    d <- t(vapply(seq_along(AGES), function(i)
      sf_Q(AGES[i], W, r, qxv, qvv, input$disc, Qv[i]),
      c(AS=0, PS=0)))
    as_max <- max(26, ceiling(max(d[,'AS'], na.rm=TRUE) / 5) * 5)
    
    
    
    p <- plot_ly()
    
    
    ## guide lines as SVG shapes (crisp; trace-rendering dashes them).
    ## AS solid on y, PS dashed on y2. Collected into hshapes, merged with
    ## the crossing vlines in the final layout().
    gAS <- if ("AS" %in% s$measures) guide_lines(s, "AS") else data.frame()
    gPS <- if ("PS" %in% s$measures) guide_lines(s, "PS") else data.frame()
    
    ## guide builders: pure functions returning lists of shapes / annotations
    ## (no <<-; combined with c() below). AS solid, PS dashed; top tier heavy.
    guide_shapes <- function(g, ax, dash) {
      if (!nrow(g)) return(list())
      lapply(seq_len(nrow(g)), function(k) {
        wln <- if (g$tier[k] == max(g$tier)) 2.2 else 1.4
        ln <- list(color = g$colour[k], width = wln)
        if (!identical(dash, 'solid')) ln$dash <- dash   # omit dash when solid
        list(type='line', xref='x', x0=min(AGES), x1=max(AGES),
             yref=ax, y0=g$at[k], y1=g$at[k], line=ln, layer='above')
      })
    }
    guide_anns <- function(g, ax, fmt, xside) {
      if (!nrow(g)) return(list())
      lapply(seq_len(nrow(g)), function(k)
        list(x=if (xside=='left') min(AGES) else max(AGES), y=g$at[k], yref=ax,
             xanchor=xside, yanchor='bottom', text=sprintf(fmt, g$at[k]),
             showarrow=FALSE, font=list(family=FONT_BODY, size=11, color='#777')))
    }
    hshapes <- c(guide_shapes(gAS, 'y', 'solid'), guide_shapes(gPS, 'y2', 'dash'))
    ann     <- c(guide_anns(gAS, 'y', 'AS %g', 'left'),
                 guide_anns(gPS, 'y2', 'PS %g', 'right'))
    
    ## SEVERITY RIBBON ####
    ## severity ribbon: colour = applied tier, along the age axis. 
    ## A second visual guide to the severity verdict at each onset age.
    ## Colour belongs to the TIER (pure fill, from the guide ramp).
    ## Vertical delimiters + labels carry WHICH criterion is decisive
    ## (AS / PS / AS.PS) -- labels shown only when that is non-constant.
    ##
    ## --- ribbon geometry (adjust freely; paper units, 0..1 = plot height) ---
    RIB_Y     <- -0.07      # ribbon centre below the x-axis (0 = axis; more negative = lower)
    RIB_H     <- -RIB_Y*0.2   # ribbon half-height (thickness)
    RIB_DELIM <- pal[['SPBlue']]   # delimiter tick colour
    RIB_LABEL <- TRUE     # draw AS / PS / AS.PS labels where informative
    ## ----------------------------------------------------------------------##
    
    ## per-age tier per measure, the applied weight (rule-aware), and the
    ## decisive criterion (meaning depends on the rule; see below).
    tier_of <- function(x, band) { i <- findInterval(x, band$lower); i[i < 1] <- 1L; i }
    tA <- if ("AS" %in% s$measures) tier_of(d[,'AS'], s$bands$AS) else rep(1L, nrow(d))
    tP <- if ("PS" %in% s$measures) tier_of(d[,'PS'], s$bands$PS) else rep(1L, nrow(d))
    
    ## applied weight per age, via the SAME engine as the curves (rule-correct
    ## for max/min/mean/AS/PS). This is what the ribbon labels and colours.
    wvec <- applied_weight(d[,'AS'], d[,'PS'], s)
    
    ## decisive criterion, per rule:
    ##   max  -> the measure achieving the max (AS/PS/AS.PS if tied)
    ##   min  -> the measure achieving the min (the binding one)
    ##   mean -> both contribute -> "AS.PS"
    ##   AS/PS (single) -> that measure
    rule <- s$rule
    decisive <- if (rule == "mean") {
      ifelse(wvec <= 1 + 1e-9, "none", "AS.PS")
    } else if (rule %in% c("AS","PS")) {
      ifelse(wvec <= 1 + 1e-9, "none", rule)
    } else {  # max or min
      aWt <- if ("AS" %in% s$measures) s$bands$AS$weight[tA] else rep(NA_real_, nrow(d))
      pWt <- if ("PS" %in% s$measures) s$bands$PS$weight[tP] else rep(NA_real_, nrow(d))
      ifelse(wvec <= 1 + 1e-9, "none",
             ifelse(!is.na(aWt) & !is.na(pWt) & abs(aWt - pWt) < 1e-9, "AS.PS",
                    ifelse(mapply(function(a,p) isTRUE(a == max(a,p,na.rm=TRUE)), aWt, pWt) &
                             (rule=="max"), "AS",
                           ifelse(rule=="max", "PS",
                                  ifelse(mapply(function(a,p) isTRUE(a == min(a,p,na.rm=TRUE)), aWt, pWt), "AS", "PS")))))
    }
    
    ## colour by the applied WEIGHT'S position in the weight range, so mean-
    ## rule (interpolated weights) colours sensibly and max/min/single keep
    ## their exact tier colours (where weight == a band weight).
    allw <- sort(unique(unlist(lapply(s$bands, `[[`, "weight"))))
    wmin <- min(allw); wmax <- max(allw)
    ramp_res <- 64
    ramp <- grDevices::colorRampPalette(c(pal[['SPYellow']], pal[['SPRed']]))(ramp_res)
    wt_col <- function(w) {
      if (is.na(w) || w <= 1 + 1e-9 || wmax <= wmin) return(NA_character_)  # neutral = blank
      frac <- (w - wmin) / (wmax - wmin)
      ramp[max(1L, min(ramp_res, round(frac * (ramp_res - 1)) + 1L))]
    }
    
    ## run-length encode on (weight, decisive) -> contiguous segments
    key <- paste(round(wvec, 4), decisive, sep = "|")
    rl  <- rle(key); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1
    show_labels <- RIB_LABEL # && length(unique(decisive[decisive != "none"])) > 1
    
    y0 <- RIB_Y - RIB_H; y1 <- RIB_Y + RIB_H
    for (j in seq_along(rl$lengths)) {
      i0 <- starts[j]; i1 <- ends[j]
      wt  <- wvec[i0]; dec <- decisive[i0]
      xa <- AGES[i0] - 0.5; xb <- AGES[i1] + 0.5            # cover the age cells
      col <- wt_col(wt)
      if (!is.na(col)) {                                    # weighted segment -> filled rect
        hshapes[[length(hshapes)+1]] <- list(type='rect', xref='x', yref='paper',
                                             x0=xa, x1=xb, y0=y0, y1=y1, fillcolor=col,
                                             line=list(width=0), layer='above')
        if (show_labels && dec != "none") {
          lbl <- paste0(formatC(wt, format="f", digits=1),
                        if (dec != "none") paste0(" \u00b7 ", sub("\\.", "\u00b7", dec)) else "")
          ann[[length(ann)+1]] <- list(x=(xa+xb)/2, y=RIB_Y, xref='x', yref='paper',
                                       text=lbl, showarrow=FALSE,
                                       xanchor='center', yanchor='middle',
                                       font=list(family=FONT_BODY, size=9, color='white'))
        }
      }
    }
    ## delimiters wherever the (tier, decisive) pair changes
    for (j in seq_len(length(rl$lengths) - 1)) {
      xd <- AGES[ends[j]] + 0.5
      hshapes[[length(hshapes)+1]] <- list(type='line', xref='x', yref='paper',
                                            x0=xd, x1=xd, y0=y0 - 0.006, y1=y1 + 0.006,
                                            line=list(color=RIB_DELIM, width=1), layer='above')
    }
    
    
    lab <- wlab()
    lab_l <- wlab_long()
    if ("AS" %in% s$measures)
      p <- add_trace(p, x=AGES, y=d[,'AS'], type='scatter', mode='lines',
                     name=paste0('AS at ', lab_l, ' RS'),        # verbose legend
                     line=list(color=LINE, width=3),
                     hovertemplate=paste0('age %{x}<br>AS %{y:.1f}<extra>',lab,'</extra>'))  # terse hover
    if ("PS" %in% s$measures)
      p <- add_trace(p, x=AGES, y=d[,'PS'], type='scatter', mode='lines', yaxis='y2',
                     name=paste0('PS at ', lab_l, ' RS'),        # verbose legend
                     line=list(color=LINE, width=2, dash='dot'),
                     hovertemplate=paste0('age %{x}<br>PS %{y:.0%}<extra>',lab,'</extra>'))  # terse hover
    
    ## crossing drop-lines: where each curve trips its own guides.
    ## labelled '<age> (AS)' / '<age> (PS)' so overlapping crossings read.
    ## crossing drop-lines: where each curve trips its own guides.
    ## Carry the measure's style -- AS solid, PS dashed -- matching the
    ## horizontal guides. Labelled '<age> (AS)' / '<age> (PS)'.
    ## crossing builders: pure functions returning shapes + annotations for
    ## the age where each curve trips its guides (no <<-; combined with c()).
    vlines <- list()
    if (isTRUE(input$show_cross)) {
      cross_shapes <- function(g, ycol, dash) {
        if (!nrow(g)) return(list())
        out <- lapply(seq_len(nrow(g)), function(k) {
          xa <- cross_at(AGES, d[, ycol], g$at[k])
          if (is.na(xa)) return(NULL)
          ln <- list(color = g$colour[k], width = 1)
          if (!identical(dash, 'solid')) ln$dash <- dash
          list(type='line', x0=xa, x1=xa, xref='x', y0=0, y1=1, yref='paper', line=ln)
        })
        Filter(Negate(is.null), out)
      }
      cross_anns <- function(g, ycol, tag) {
        if (!nrow(g)) return(list())
        out <- lapply(seq_len(nrow(g)), function(k) {
          xa <- cross_at(AGES, d[, ycol], g$at[k])
          if (is.na(xa)) return(NULL)
          list(x=xa, y=1, yref='paper', xref='x', yanchor='bottom', xanchor='center',
               text=sprintf('%.0f (%s)', xa, tag), showarrow=FALSE,
               font=list(family=FONT_BODY, size=10, color=g$colour[k]))
        })
        Filter(Negate(is.null), out)
      }
      if ("AS" %in% s$measures) {
        vlines <- c(vlines, cross_shapes(gAS, 'AS', 'solid'))
        ann    <- c(ann,    cross_anns(gAS, 'AS', 'AS'))
      }
      if ("PS" %in% s$measures) {
        vlines <- c(vlines, cross_shapes(gPS, 'PS', 'dash'))
        ann    <- c(ann,    cross_anns(gPS, 'PS', 'PS'))
      }
    }
    
    show_ps <- "PS" %in% s$measures
    p |> layout(
      xaxis=list(title='Age at onset = Age of shortfall estimation', showgrid=FALSE, zeroline=FALSE),
      yaxis=list(title='Absolute shortfall (QADLE)', range=c(0,as_max),
                 gridcolor='#EEEEEE', zeroline=FALSE),
      yaxis2=list(title=if (show_ps) 'Proportional shortfall' else '',
                  overlaying='y', side='right', range=c(0,1),
                  tickformat='.0%', showgrid=FALSE, zeroline=FALSE,
                  visible=show_ps),
      legend=list(orientation='h', x=0, y=1.08),
      margin=list(r=90, b=110), font=list(family=FONT_HEAD, size=14),
      shapes = c(hshapes, vlines), annotations = ann)
  })

  ## ==== Threshold author tab (server side) ====
  ## ---- Threshold author tab ---------------------------####
  ## editable band state, per measure. Each is a data.frame(lower,upper,weight).
  ed_bands <- reactiveValues(AS = NULL, PS = NULL)
  ed_measures <- reactiveVal(c("AS","PS"))
  authored <- reactiveVal(character(0))   # ids saved this session (for live menu)
  
  ## load a chosen set's bands into the editor
  observeEvent(input$ed_load, {
    s <- sevsets_r()[[input$ed_fork]]
    ed_bands$AS <- if ("AS" %in% s$measures) s$bands$AS else NULL
    ed_bands$PS <- if ("PS" %in% s$measures) s$bands$PS else NULL
    ed_measures(s$measures)
    updateTextInput(session, "ed_id",    value = paste0(s$id, "_edit"))
    updateTextInput(session, "ed_label", value = paste0(s$label %||% s$id, " (edited)"))
    updateSelectInput(session, "ed_rule", selected = s$rule)
    updateCheckboxGroupInput(session, "ed_meas", selected = s$measures)
    updateSelectInput(session, "ed_wn",   selected = s$weight_native)
  })
  
  ## render an editable numeric grid for one measure's bands
  band_grid <- function(m) {
    b <- ed_bands[[m]]; if (is.null(b)) return(div(em("(Measure not in this set)")))
    rows <- lapply(seq_len(nrow(b)), function(k) fluidRow(
      column(4, numericInput(paste0("ed_",m,"_lo_",k), if (k==1) "lower" else NULL, b$lower[k], step=0.01)),
      column(4, numericInput(paste0("ed_",m,"_up_",k), if (k==1) "upper" else NULL, b$upper[k], step=0.01)),
      column(4, numericInput(paste0("ed_",m,"_wt_",k), if (k==1) "weight" else NULL, b$weight[k], step=0.1))
    ))
    do.call(tagList, rows)
  }
  output$ed_AS_grid <- renderUI(band_grid("AS"))
  output$ed_PS_grid <- renderUI(band_grid("PS"))
  
  ## read the grid back into the reactive band frames
  read_grid <- function(m) {
    b <- ed_bands[[m]]; if (is.null(b)) return(NULL)
    n <- nrow(b)
    df <- data.frame(
      lower  = vapply(1:n, function(k) input[[paste0("ed_",m,"_lo_",k)]] %||% NA, 0),
      upper  = vapply(1:n, function(k) input[[paste0("ed_",m,"_up_",k)]] %||% NA, 0),
      weight = vapply(1:n, function(k) input[[paste0("ed_",m,"_wt_",k)]] %||% NA, 0))
    if (m == "AS") df$upper[n] <- Inf          # AS top band is always open-ended
    if (m == "PS") df$upper[n] <- 1            # PS top band is always exactly 1
    df
  }
  
  ## add / remove bands (append near top / drop last)
  addband <- function(m) { b <- ed_bands[[m]] %||% data.frame(lower=0,upper=Inf,weight=1)
  b <- read_grid(m) %||% b
  ed_bands[[m]] <- rbind(b[1,,drop=FALSE], b)   # duplicate first row as a stub
  }
  delband <- function(m) { b <- read_grid(m); if (!is.null(b) && nrow(b) > 1) ed_bands[[m]] <- b[-nrow(b),,drop=FALSE] }
  observeEvent(input$ed_AS_add, addband("AS")); observeEvent(input$ed_AS_del, delband("AS"))
  observeEvent(input$ed_PS_add, addband("PS")); observeEvent(input$ed_PS_del, delband("PS"))
  
  ## build a candidate sevset from the current editor state (or an error)
  ed_candidate <- reactive({
    chosen <- input$ed_meas %||% character(0)
    bands <- list()
    if ("AS" %in% chosen && !is.null(ed_bands$AS) && nrow(ed_bands$AS) > 0) bands$AS <- read_grid("AS")
    if ("PS" %in% chosen && !is.null(ed_bands$PS) && nrow(ed_bands$PS) > 0) bands$PS <- read_grid("PS")
    #meas <- names(bands)
    meas <- chosen
    rule <- if (length(meas) == 2) (input$ed_rule %||% "max")
    else if (length(meas) == 1) meas
    else "max"
    new_sevset_ui(
      id = input$ed_id %||% "", label = input$ed_label %||% "",
      region = "user", year = as.integer(format(Sys.Date(), "%Y")),
      measures = meas, rule = input$ed_rule, bands = bands,
      weight_native = input$ed_wn,
      ce_threshold = if (identical(input$ed_wn,"ce_threshold")) bands[[meas[1]]]$weight[1] else NA_real_,
      derived_from = sub("_edit$", "", input$ed_id %||% ""),
      provenance = list(source = "user-authored", notes = "User-modified via severity regime editor"))
  })
  
  ## live validation readout
  output$ed_valid <- renderUI({
    res <- tryCatch({ ed_candidate(); "VALID  (Can be saved)" }, error = function(e) conditionMessage(e))
    col <- if (grepl("^VALID", res)) pal[['SPGreen']] else pal[['SPRed']]
    span(style=sprintf("color:%s;", col), res)
  })
  
  ## mode-appropriate save control
  output$ed_saveui <- renderUI({
    if (identical(APP_MODE, "local")) actionButton("ed_save", "Save")
    else downloadButton("ed_download", "Download regime (.rds)")
  })
  
  ## LOCAL: persist to overlay (never touches canonical shortfalls_data.rds)
  observeEvent(input$ed_save, {
    set <- tryCatch(ed_candidate(), error = function(e) { showNotification(conditionMessage(e), type="error"); NULL })
    if (is.null(set)) return()
    if (set$id %in% names(sevsets_r()) && !isFALSE(sevsets_r()[[set$id]]$canonical)) {
      showNotification("id collides with a canonical regime; choose another id", type="error"); return() }
    save_user_sevset(set)
    authored(union(authored(), set$id))
    refresh_store()
    showNotification(paste0("saved '", set$id, "'"), type="message")
  })
  
  ## SERVER: download only, no disk write
  output$ed_download <- downloadHandler(
    filename = function() paste0(input$ed_id %||% "regime", ".rds"),
    content  = function(file) saveRDS(tryCatch(ed_candidate(), error=function(e) NULL), file))

  ## ---- delete an AUTHORED regime (local mode only) -----------------
  ## Targets are overlay sets ONLY (canonical == FALSE). Built-in regimes
  ## can never appear here, so they cannot be deleted. 
  overlay_ids <- reactiveVal(character(0))
  refresh_overlay_ids <- function() {
    ids <- if (identical(APP_MODE,"local") && file.exists(SF_USER)) {
      ov <- tryCatch(readRDS(SF_USER), error=function(e) NULL)
      if (!is.null(ov)) names(ov$sf_thresholds) else character(0)
    } else character(0)
    overlay_ids(ids)
  }
  refresh_overlay_ids()
  
  ## rule is a real choice only with BOTH measures; hidden for one/none.
  output$ed_rule_ui <- renderUI({
    if (length(input$ed_meas) == 2)
      selectInput("ed_rule", "Rule (combine AS & PS)", c("max","min","mean"),
                  selected = isolate(input$ed_rule) %||% "max")
    else NULL
  })
  
  output$ed_delui <- renderUI({
    if (!identical(APP_MODE,"local")) return(NULL)   # no overlay to delete on a server
    ids <- overlay_ids()
    if (!length(ids)) return(div(em("(no authored regimes to delete)")))
    tagList(
      selectInput("ed_del_id", "Delete authored regime", choices = ids),
      actionButton("ed_delete", "Delete", class = "btn-danger")
    )
  })
  # To be implemented
  observeEvent(input$ed_meas, {
    chosen <- input$ed_meas %||% character(0)

    for (m in c("AS", "PS")) {

      if (m %in% chosen && is.null(ed_bands[[m]]))
        ed_bands[[m]] <- data.frame(lower  = 0,upper  = Inf,weight = 1)

      if (!(m %in% chosen))
        ed_bands[[m]] <- NULL
    }
  })

  observeEvent(input$ed_delete, {
    id <- input$ed_del_id
    if (is.null(id) || !nzchar(id)) return()
    ## guard: never delete a canonical regime (should be impossible -- the
    ## menu lists overlay ids only -- but check the shipped store to be sure)
    if (id %in% names(sevsets_r()) && !isFALSE(sevsets_r()[[id]]$canonical)) {
      showNotification("refusing: that is a built-in regime", type="error"); return() }
    ov <- tryCatch(readRDS(SF_USER), error=function(e) NULL)
    if (is.null(ov) || !id %in% names(ov$sf_thresholds)) {
      showNotification("not found in overlay", type="warning"); return() }
    ov$sf_thresholds[[id]] <- NULL
    saveRDS(ov, SF_USER)
    refresh_overlay_ids()
    refresh_store()
    showNotification(paste0("Deleted '", id, "'"), type="message")
  })
  
  
  


  ## ==== Condition explorer ==========================================
  ## Reference rule (AS STORED): a loaded condition carries the ids of
  ## the norm and life table it was authored against, and is plotted
  ## against THOSE, overriding the header selection. The override is
  ## announced, never silent. Manual (slider) mode uses the header
  ## selection, since it has no stored provenance of its own.
  ## Re-applying a condition's hit_spec to the CURRENT reference is a
  ## separate, later toggle -- see the session note.


  current_cond <- reactive({
    if (is.null(input$cond) || identical(input$cond, "__manual__")) NULL
    else conds_r()[[input$cond]]
  })

  ## reference vectors in force on this tab
  ex_ref <- reactive({
    cc <- current_cond()
    if (is.null(cc)) return(list(qx = qx(), qv = qv(), src = NULL))
    rr <- attr(cc, "reference") %||% list()
    tb <- tbls_r()[[rr$table %||% ""]]
    nm <- norms_r()[[rr$norm  %||% ""]]
    if (is.null(tb) || is.null(nm))
      return(list(qx = qx(), qv = qv(), src = NULL))
    list(qx = gv(tb, "a", "mu"), qv = gv(nm, "age", "hrqol"),
         src = list(table = rr$table, norm = rr$norm))
  })

  output$ex_ref_note <- renderUI({
    s <- ex_ref()$src
    if (is.null(s))
      HTML(sprintf(paste0("Reference: life table <b>%s</b>; HRQoL norm <b>%s</b> ",
                          "(from the header selection)."),
                   .obj_label(tbls_r(),  input$table),
                   .obj_label(norms_r(), input$norm)))
    else
      HTML(sprintf(paste0("Reference set by the loaded condition: life table <b>%s</b>; ",
                          "HRQoL norm <b>%s</b> \u2013 the header selection is overridden ",
                          "so the stored condition is shown exactly as authored."),
                   .obj_label(tbls_r(), s$table),
                   .obj_label(norms_r(), s$norm)))
  })

  output$or_readout <- renderText(sprintf("OR = %.1f", LOG_BASE ^ input$or_log))

  ## OR / HRQoL-scale age-vectors, relative to the reference in force
  vecs <- reactive({
    cc <- current_cond(); rf <- ex_ref()
    if (is.null(cc)) {
      onset <- input$onset; OR <- LOG_BASE ^ input$or_log; m <- input$m
      list(or_v = ifelse(0:TOP >= onset, OR, 1),
           m_v  = ifelse(0:TOP >= onset, m,  1))
    } else {
      mu_c <- stats::approx(cc$age, cc$mu,    xout = 0:TOP, rule = 2)$y
      q_c  <- stats::approx(cc$age, cc$hrqol, xout = 0:TOP, rule = 2)$y
      or_v <- (mu_c * (1 - rf$qx)) / (rf$qx * (1 - mu_c))
      or_v[!is.finite(or_v)] <- 1
      m_v  <- q_c / rf$qv; m_v[!is.finite(m_v)] <- 1
      list(or_v = pmax(or_v, 1e-6), m_v = m_v)
    }
  })

  ex <- reactive({
    req(is.finite(input$r), input$r > -1, input$r <= 1)
    v <- vecs(); rf <- ex_ref()
    build_stack(input$age, v$or_v, v$m_v, input$r, rf$qx, rf$qv, input$disc)
  })

  output$explore_plot <- renderPlot({
    v <- vecs(); rf <- ex_ref()
    if (identical(input$view, "sbs")) {
      par(mfrow = c(1, 2))
      x0 <- build_stack(input$age, v$or_v, v$m_v, 0,        rf$qx, rf$qv, input$disc)
      xr <- build_stack(input$age, v$or_v, v$m_v, input$r,  rf$qx, rf$qv, input$disc)
      draw_stack(x0, bands = TRUE, main = "undiscounted (\u03c1 = 0.0%)")
      draw_stack(xr, bands = TRUE, disc_view = TRUE,
                 main = sprintf("discounted (\u03c1 = %.1f%%)", 100 * input$r))
      par(mfrow = c(1, 1))
    } else {
      draw_stack(ex(), bands = TRUE)
    }
  })

  output$legend <- renderUI({
    x <- ex()
    chip <- function(c, lab) tags$span(
      style = "margin-right:14px;font-size:13px;white-space:nowrap;",
      tags$span(style = sprintf(paste0("display:inline-block;width:12px;height:12px;",
                                       "border-radius:2px;background:%s;margin-right:5px;",
                                       "vertical-align:-1px;"), c)), lab)
    tags$div(style = "margin:8px 0;line-height:1.9;",
      chip(BAND$norm, "norm HRQoL (population)"),
      chip(BAND$mort, sprintf("mortality shortfall \u2248 %.1f", x$mort)),
      chip(BAND$morb, sprintf("morbidity shortfall \u2248 %.1f", x$morb)),
      chip(BAND$qale, sprintf("delivered QALE (undisc) \u2248 %.1f", x$QALEc)))
  })

  ## the weight row is regime-sensitive: applied_weight() on the header's sevset
  output$compare <- renderUI(compare_table(
    ex(),
    if (is.null(input$cond) || identical(input$cond, "__manual__"))
      sprintf("onset %d, from age %d", input$onset, input$age)
    else sprintf("from age %d", input$age),
    set()))

  output$cond_info <- renderUI({
    cc <- current_cond(); info <- attr(cc, "info"); req(info)
    tagList(tags$p(style = "font-style:italic;", info$summary %||% ""),
      lapply(info$sources %||% list(), function(s)
        tags$div(tags$a(href = s$url, target = "_blank", s$cite %||% s$url))))
  })

  ## ==== condition editor: knot state ================================
  ## Knots are the RELATIVE description (an OR profile and an HRQoL scale),
  ## so they re-apply to any reference. The stored columns are absolute.
  ## Age 0 is pinned (OR 1 / scale 1) and TOP inherits the last knot, so
  ## only interior knots are user-editable and only they are stored.
  
  
  KN <- reactiveValues(
    or = data.frame(age = 40, value = 2),
    m  = data.frame(age = 40, value = 0.8),
    or_active = 1L, m_active = 1L)

  
  ## Double-click toggles a knot. Within TOL years of an existing marker it
  ## removes that one; otherwise it adds a knot AT THE CURRENT CURVE VALUE,
  ## so the profile is unchanged by the addition -- you gain a control point,
  ## not a new shape. Ages stay distinct: interp_knots drops duplicates, so a
  ## knot landing on an occupied age would silently lose one.
  KNOT_TOL <- 2                      # years; roughly the marker's own width
  KNOT_MAX <- 20
  
  toggle_knot <- function(which, dbl, vec) {
    if (is.null(dbl) || !is.finite(dbl$x)) return(invisible())
    cur <- KN[[which]]; act <- paste0(which, "_active")
    hit <- if (nrow(cur)) which(abs(cur$age - dbl$x) <= KNOT_TOL) else integer(0)
    if (length(hit)) {                                   # remove the nearest
      j <- hit[which.min(abs(cur$age[hit] - dbl$x))]
      KN[[which]] <- cur[-j, , drop = FALSE]
      KN[[act]]   <- max(1L, min(j, nrow(KN[[which]])))
      return(invisible())
    }
    a <- round(dbl$x)                                    # otherwise add
    if (a < 1 || a > TOP - 1 || a %in% cur$age) return(invisible())
    if (nrow(cur) >= KNOT_MAX) {
      showNotification(sprintf("%s interior knots is the maximum", KNOT_MAX), type = "warning")
      return(invisible())
    }
    new <- rbind(cur, data.frame(age = a, value = vec[a + 1]))
    new <- new[order(new$age), , drop = FALSE]
    KN[[which]] <- new
    KN[[act]]   <- which(new$age == a)[1]
  }
  observeEvent(input$or_dbl, toggle_knot("or", input$or_dbl, or_vec()))
  observeEvent(input$m_dbl,  toggle_knot("m",  input$m_dbl,  m_vec()))
  
  
  strip_anchors <- function(full) {
    if (is.null(full) || nrow(full) == 0)
      return(data.frame(age = numeric(0), value = numeric(0)))
    int <- full[full$age > 0 & full$age < TOP, , drop = FALSE]
    data.frame(age = int$age, value = int$value)
  }
  
  full_knots <- function(which, end0) {          # age 0 pinned; TOP inherits last
    cur <- KN[[which]]
    if (nrow(cur) == 0) return(data.frame(age = c(0, TOP), value = c(end0, end0)))
    ord  <- cur[order(cur$age), , drop = FALSE]
    endT <- ord$value[nrow(ord)]
    rbind(data.frame(age = 0, value = end0), ord,
          data.frame(age = TOP, value = endT))
  }
  or_knots <- reactive(full_knots("or", 1))
  m_knots  <- reactive(full_knots("m",  1))
  or_vec <- reactive(pmax(interp_knots(or_knots(), 1, input$or_rule %||% "constant"), 1e-6))
  m_vec  <- reactive(pmin(pmax(interp_knots(m_knots(), 1, input$m_rule %||% "linear"), 0), 1))
  
  
  
  ## ---- click a marker to activate; the slider binds to the active knot
  activate <- function(which, click) {
    cur <- KN[[which]]; if (is.null(click) || nrow(cur) == 0) return()
    KN[[paste0(which, "_active")]] <- which.min(abs(cur$age - click$x))
  }
  
  ## The active knot's age is bounded by its NEIGHBOURS, with age 0 and age
  ## TOP acting as implicit knots either side. Neighbours are found by age
  ## order, not row order -- KN$or is not kept sorted. The +1/-1 keeps ages
  ## distinct: interp_knots drops duplicates, so two knots sharing an age
  ## would silently lose one.
  knot_bounds <- function(cur, i) {
    if (nrow(cur) == 0 || i > nrow(cur)) return(c(1, TOP - 1))
    a <- cur$age[i]; others <- cur$age[-i]
    lo <- if (any(others < a)) max(others[others < a]) else 0
    hi <- if (any(others > a)) min(others[others > a]) else TOP
    if (lo + 1 > hi - 1) c(a, a) else c(lo + 1, hi - 1)
  }
  
  ## Push the active knot to its two sliders. Called BOTH by the activation
  ## observers and directly on load: assigning an unchanged value to
  ## KN$*_active may not invalidate, so the observer cannot be relied on.
  sync_sliders <- function(which) {
    cur <- KN[[which]]; i <- KN[[paste0(which, "_active")]]
    if (nrow(cur) == 0 || i > nrow(cur)) {
      lab <- "(no knots \u2013 double-click the plot to add one)"
      updateSliderInput(session, paste0(which, "_age"), label = lab)
      return(invisible())
    }
    b <- knot_bounds(cur, i)
    if (identical(which, "or"))
      updateSliderInput(session, "or_slider",
                        value = log(max(cur$value[i], 1), base = LOG_BASE))
    else
      updateSliderInput(session, "m_slider", value = cur$value[i])
    updateSliderInput(session, paste0(which, "_age"),
                      label = sprintf("Knot %d age", i),
                      min = b[1], max = b[2], value = cur$age[i])
  }
  
  
  observeEvent(input$or_click, activate("or", input$or_click))
  observeEvent(input$m_click,  activate("m",  input$m_click))

  observeEvent(KN$or_active, sync_sliders("or"))
  observeEvent(KN$m_active,  sync_sliders("m"))
  
  
  observeEvent(input$or_slider, {
    cur <- KN$or; i <- KN$or_active
    if (nrow(cur) >= i) { cur$value[i] <- LOG_BASE ^ input$or_slider; KN$or <- cur }
  })
  observeEvent(input$m_slider, {
    cur <- KN$m; i <- KN$m_active
    if (nrow(cur) >= i) { cur$value[i] <- input$m_slider; KN$m <- cur }
  })
  
  observeEvent(input$cond_reset,  {
    KN$or <- data.frame(age = 40, value = 2)
    KN$m  <- data.frame(age = 40, value = 0.8)
    KN$or_active <- 1L
    KN$m_active  <- 1L
    sync_sliders("or"); sync_sliders("m")
    updateSelectInput(session, "or_rule", selected = "constant")
    updateSelectInput(session, "m_rule",  selected = "linear")
    updateTextInput(session, "cond_label",   value = "New condition")
    updateTextInput(session, "cond_id",      value = "new_condition")
    updateTextInput(session, "cond_summary", value = "")
    updateTextInput(session, "cond_cite",    value = "")
    updateTextInput(session, "cond_url",     value = "")
    output$edit_load_msg <- renderUI(NULL)
    showNotification("Reset condition editor", type = "message")})
  
  
  ## Neighbours may have moved, or knots been added/removed. Refresh the
  ## bounds without touching the value. Keyed on the AGES alone, so dragging
  ## a knot's VALUE does not churn this on every tick.
  observeEvent(paste(KN$or$age, collapse = "|"), {
    cur <- KN$or; i <- KN$or_active
    if (nrow(cur) >= i) {
      b <- knot_bounds(cur, i)
      updateSliderInput(session, "or_age", min = b[1], max = b[2])
    }
  }, ignoreInit = TRUE)
  observeEvent(paste(KN$m$age, collapse = "|"), {
    cur <- KN$m; i <- KN$m_active
    if (nrow(cur) >= i) {
      b <- knot_bounds(cur, i)
      updateSliderInput(session, "m_age", min = b[1], max = b[2])
    }
  }, ignoreInit = TRUE)
  
  ## readouts show the knot in ABSOLUTE terms against the selected
  ## reference, so the author sees the implied soc value, not just the
  ## multiplier. They therefore track the header's norm/table.
  output$or_age_ui <- renderUI({
    cur <- KN$or; if (nrow(cur) == 0) return(NULL)
    i <- KN$or_active; age <- cur$age[i]; OR <- cur$value[i]
    qref <- qx()[age + 1]; qsoc <- or_hit(qref, OR)
    div(style = sprintf("font-family:%s;font-size:12px;color:#555;margin-top:-8px;", FONT_BODY),
        HTML(sprintf("OR <b>%.2f</b> at age <b>%d</b> \u2013 ref q = %.4f &rarr; soc q = <b>%.4f</b>",
                     OR, age, qref, qsoc)))
  })
  output$m_age_ui <- renderUI({
    cur <- KN$m; if (nrow(cur) == 0) return(NULL)
    i <- KN$m_active; age <- cur$age[i]; sc <- cur$value[i]
    href <- qv()[age + 1]; hsoc <- href * sc
    div(style = sprintf("font-family:%s;font-size:12px;color:#555;margin-top:-8px;", FONT_BODY),
        HTML(sprintf("scale <b>%.2f</b> at age <b>%d</b> \u2013 ref hrqol = %.3f &rarr; soc hrqol = <b>%.3f</b>",
                     sc, age, href, hsoc)))
  })
  
  ## Age write-back fires on the SETTLED value: dragging across 119
  ## positions would otherwise rebuild the knot frame and redraw both
  ## previews on every step. The bounds are the slider's own, so no clamp.
  or_age_settled <- debounce(reactive(input$or_age), 250)
  m_age_settled  <- debounce(reactive(input$m_age),  250)
  
  observeEvent(or_age_settled(), {
    a <- or_age_settled(); cur <- KN$or; i <- KN$or_active
    if (isTruthy(a) && nrow(cur) >= i && !identical(cur$age[i], a)) {
      cur$age[i] <- a; KN$or <- cur }
  })
  observeEvent(m_age_settled(), {
    a <- m_age_settled(); cur <- KN$m; i <- KN$m_active
    if (isTruthy(a) && nrow(cur) >= i && !identical(cur$age[i], a)) {
      cur$age[i] <- a; KN$m <- cur }
  })
  
  ## ---- knot previews: induced survival and induced HRQoL
  output$or_preview <- renderPlot({
    par(mar = c(3, 3.4, 0.5, 0.5))
    qxv <- qx(); ov <- or_vec()
    Sc  <- c(1, cumprod(1 - or_hit(qxv, ov)[-length(ov)]))
    plot(0:TOP, Sc, type = "l", col = STROKE$cond, lwd = 2, ylim = c(0, 1),
         xlab = "age", ylab = "induced survival S_c", family = "serif")
    lines(0:TOP, c(1, cumprod(1 - qxv[-length(qxv)])), col = STROKE$surv, lty = 2)
    cur <- KN$or
    if (nrow(cur) > 0) {
      yy <- Sc[pmin(cur$age, TOP) + 1]
      points(cur$age, yy, pch = 21, bg = "white", col = STROKE$onset, cex = 1.4, lwd = 2)
      i <- KN$or_active
      points(cur$age[i], yy[i], pch = 19, col = STROKE$active, cex = 1.7)
    }
  })
  output$m_preview <- renderPlot({
    par(mar = c(3, 3.4, 0.5, 0.5))
    qvv <- qv(); mv <- m_vec()
    plot(0:TOP, mv * qvv, type = "l", col = STROKE$cond, lwd = 2, ylim = c(0, 1),
         xlab = "age", ylab = "induced HRQoL q_c", family = "serif")
    lines(0:TOP, qvv, col = STROKE$ref, lty = 2)
    cur <- KN$m
    if (nrow(cur) > 0) {
      yy <- (mv * qvv)[pmin(cur$age, TOP) + 1]
      points(cur$age, yy, pch = 21, bg = "white", col = STROKE$onset, cex = 1.4, lwd = 2)
      i <- KN$m_active
      points(cur$age[i], yy[i], pch = 19, col = STROKE$active, cex = 1.7)
    }
  })
  
  ## ---- the authored condition's own stack, at the shared rho
  auth <- reactive({
    req(is.finite(input$r), input$r > -1, input$r <= 1)
    build_stack(input$a_auth, or_vec(), m_vec(), input$r, qx(), qv(), input$disc)
  })
  output$auth_plot <- renderPlot({
    if (identical(input$auth_view, "sbs")) {
      par(mfrow = c(1, 2))
      x0 <- build_stack(input$a_auth, or_vec(), m_vec(), 0,       qx(), qv(), input$disc)
      xr <- build_stack(input$a_auth, or_vec(), m_vec(), input$r, qx(), qv(), input$disc)
      draw_stack(x0, bands = TRUE, main = "undiscounted (\u03c1 = 0)")
      draw_stack(xr, bands = TRUE, disc_view = TRUE,
                 main = sprintf("discounted (\u03c1 = %.1f%%)", 100 * input$r))
      par(mfrow = c(1, 1))
    } else {
      draw_stack(auth(), bands = TRUE)
    }
  })
  output$auth_compare <- renderUI(
    compare_table(auth(), sprintf("preview from age %d", input$a_auth), set()))
  
  
  ## ---- load an existing condition back into the knot editor
  observeEvent(input$edit_load_go, {
    id <- input$edit_load
    if (is.null(id) || !nzchar(id)) { output$edit_load_msg <- renderUI(NULL); return() }
    obj <- conds_r()[[id]]
    if (is.null(obj)) {
      output$edit_load_msg <- renderUI(span(style = "color:#b00;", "Could not read condition!"))
      return()
    }
    hs <- attr(obj, "hit_spec"); rr <- attr(obj, "reference") %||% list()
    
    ## reference note: the condition was authored against a named pair. The
    ## editor works against the CURRENT header selection, so re-saving
    ## re-measures the soc arm. Say so rather than silently re-basing.
    ref_note <- if (!identical(rr$table %||% "", input$table) ||
                    !identical(rr$norm  %||% "", input$norm))
      span(style = "color:#a60;",
           sprintf("Authored against %s / %s; the editor is on %s / %s. Knots are restored, but the saved soc values will be re-measured against the current selection.",
                   rr$table %||% "?", rr$norm %||% "?", input$table, input$norm))
    
    if (is.null(hs) || is.null(hs$or_knots) || is.null(hs$hrqol_knots)) {
      output$edit_load_msg <- renderUI(tagList(
        span(style = "color:#a60;",
             "This condition has no knot structure \u2013 it can be explored, but not knot-edited here."),
        if (!is.null(ref_note)) tagList(br(), ref_note)))
      return()
    }
    
    or_int <- strip_anchors(hs$or_knots)
    m_int  <- strip_anchors(hs$hrqol_knots)
    
    updateSelectInput(session, "or_rule", selected = hs$or_interp    %||% "constant")
    updateSelectInput(session, "m_rule",  selected = hs$hrqol_interp %||% "linear")
    
    ## push the loaded knots, then sync the sliders explicitly: assigning an
    ## unchanged value to KN$*_active may not invalidate, so the activation
    ## observers cannot be relied on here.
    KN$or <- or_int
    KN$m  <- m_int
    KN$or_active <- 1L; KN$m_active <- 1L
    sync_sliders("or"); sync_sliders("m")
    
    info <- attr(obj, "info") %||% list()
    srcs <- info$sources %||% list()
    src  <- if (length(srcs) >= 1) srcs[[1]] else list()
    updateTextInput(session, "cond_label",   value = attr(obj, "menu_label") %||% "")
    updateTextInput(session, "cond_id",      value = id)
    updateTextInput(session, "cond_summary", value = info$summary %||% "")
    updateTextInput(session, "cond_cite",    value = src$cite %||% "")
    updateTextInput(session, "cond_url",     value = src$url  %||% "")
    
    output$edit_load_msg <- renderUI(tagList(
      span(style = "color:#060;",
           sprintf("loaded '%s'  (%d OR knots, %d HRQoL knots)",
                   attr(obj, "menu_label") %||% id, nrow(or_int), nrow(m_int))),
      if (!is.null(ref_note)) tagList(br(), ref_note)))
  })
  
  
  ## ---- delete an AUTHORED condition (local mode only) ---------------
  ## Targets are overlay conditions ONLY. Shipped conditions never appear
  ## here, so they cannot be deleted from the app. 
  ##
  ## The write is a read-modify-write of the WHOLE overlay: sf_thresholds
  ## shares this file, and replacing rather than editing the object would
  ## silently destroy the authored regimes.
  cond_overlay_ids <- reactiveVal(character(0))
  refresh_cond_ids <- function() {
    ids <- if (identical(APP_MODE, "local") && file.exists(SF_USER)) {
      ov <- tryCatch(readRDS(SF_USER), error = function(e) NULL)
      if (!is.null(ov)) names(ov$sf_conditions) %||% character(0) else character(0)
    } else character(0)
    cond_overlay_ids(ids)
  }
  refresh_cond_ids()
  
  output$cond_delbtn <- renderUI({
    if (!identical(APP_MODE, "local") || !length(cond_overlay_ids())) return(NULL)
    actionButton("cond_delete", "Delete", class = "btn-danger",
                 style = "margin-top:25px;width:100%;")
  })
  
  output$cond_delsel <- renderUI({
    if (!identical(APP_MODE, "local")) return(NULL)
    ids <- cond_overlay_ids()
    if (!length(ids))
      return(div(style = "margin-top:28px;font-size:12px;color:#888;",
                 em("(No authored condition(s) to delete)")))
    lab <- vapply(ids, function(i) {
      o <- conds_r()[[i]]
      if (is.null(o)) i else attr(o, "menu_label") %||% i
    }, "")
    selectInput("cond_del_id", "Delete condition",
                choices = stats::setNames(ids, lab), width = "100%")
  })
  
  observeEvent(input$cond_delete, {
    id <- input$cond_del_id
    if (is.null(id) || !nzchar(id)) return()
    ## guard: never delete a shipped condition. Should be unreachable --
    ## the menu lists overlay ids only -- but check the store to be sure.
    if (id %in% names(conds_r()) &&
        isTRUE(attr(conds_r()[[id]], "canonical"))) {
      showNotification("Shipped condition cannot be deleted from the app", type = "error"); return() }
    ov <- tryCatch(readRDS(SF_USER), error = function(e) NULL)
    if (is.null(ov) || !id %in% names(ov$sf_conditions)) {
      showNotification("Not found in overlay", type = "warning"); return() }
    ov$sf_conditions[[id]] <- NULL
    saveRDS(ov, SF_USER)
    refresh_cond_ids()
    refresh_store()
    showNotification(paste0("Deleted '", id, "'"),
                     type = "message")
  })
  
  
  ## ---- build the condition object from the current editor state
  cond_candidate <- reactive({
    qxv <- qx(); qvv <- qv()
    df <- data.frame(age = 0:TOP,
                     hrqol = pmin(pmax(m_vec() * qvv, 0), 1),
                     mu    = or_hit(qxv, or_vec()))
    attr(df, "menu_label") <- input$cond_label
    attr(df, "info") <- list(
      summary = input$cond_summary,
      sources = if (nzchar(input$cond_cite %||% "") || nzchar(input$cond_url %||% ""))
        list(list(cite = input$cond_cite, url = input$cond_url)) else list())
    ## reference by CONTAINER ID -- resolvable, checkable, never duplicated
    attr(df, "reference") <- list(table = input$table, norm = input$norm,
                                  fingerprint = NA_character_)
    attr(df, "hit_spec") <- list(or_knots = or_knots(), hrqol_knots = m_knots(),
                                 or_interp = input$or_rule, hrqol_interp = input$m_rule,
                                 rule = "rule2 (flat outside knots)")
    attr(df, "canonical") <- FALSE
    df
  })
  
  output$cond_saveui <- renderUI({
    if (identical(APP_MODE, "local"))
      actionButton("cond_save", "Save", class = "btn-primary")
    else
      downloadButton("cond_download", "Download condition (.rds)")
  })
  
  ## LOCAL: read-modify-write the WHOLE overlay. Never write only this
  ## sublist -- a partial write would destroy authored severity regimes.
  observeEvent(input$cond_save, {
    id  <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(input$cond_id %||% "")))
    lab <- input$cond_label %||% ""
    if (!nzchar(id) || !nzchar(lab)) {
      output$export_msg <- renderUI(span(style = "color:#b00;",
                                         "A condition needs both an id and a menu label."))
      return()
    }
    if (id %in% names(conds_r()) &&
        isTRUE(attr(conds_r()[[id]], "canonical"))) {
      output$export_msg <- renderUI(span(style = "color:#b00;",
                                         sprintf("'%s' is a built-in condition; choose another id.", id)))
      return()
    }
    ov <- if (file.exists(SF_USER)) readRDS(SF_USER) else list()
    if (is.null(ov$sf_conditions)) ov$sf_conditions <- list()
    ov$sf_conditions[[id]] <- cond_candidate()
    saveRDS(ov, SF_USER)
    refresh_store()
    output$export_msg <- renderUI(span(style = "color:#060;",
                                       sprintf("saved '%s' (%s).", id, lab)))
  })
  
  ## ==== Value potential =============================================
  ## Effective force of discounting. Year-wise at rate r IS continuous at
  ## force log(1+r), so the shared disc radio is honoured by choosing the
  ## force rather than by branching through the kernel.
  vp_force <- reactive({
    r <- input$r
    req(is.finite(r), r > -1)
    if (identical(input$disc, "disc")) log(1 + r) else r
  })
  
  ## the horizon resolves to one scalar, from whichever mode is active
  vp_horizon <- reactive({
    if (identical(input$vp_hmode, "t")) {
      req(is.finite(input$vp_T), input$vp_T > 0)
      list(t = input$vp_T,
           note = sprintf("Horizon set directly: t = %.1f yr.", input$vp_T))
    } else {
      req(is.finite(input$vp_age), input$vp_age >= 0)
      t <- e_at(input$vp_age, qx())
      req(is.finite(t), t > 0)
      list(t = t,
           note = sprintf("Age %g \u2192 e(age) = %.1f yr, from %s.",
                          input$vp_age, t, .obj_label(tbls_r(), input$table)))
    }
  })
  
  ## keep the near-window slider inside the current horizon
  observeEvent(vp_horizon()$t, {
    tmax <- vp_horizon()$t
    updateSliderInput(session, "vp_N", max = round(tmax, 1),
                      value = min(isolate(input$vp_N), tmax))
  })
  
  vp_calc <- reactive({
    f <- vp_force(); t <- vp_horizon()$t
    N <- min(input$vp_N, t)
    p <- head_frac(f * t)
    list(f = f, t = t, N = N, u = f * t,
         s = p * t, rp = 1 - p,               # balance split; r' = head value share
         nshare = near_share(N, t, f))
  })
    output$vp_plot <- renderPlot({
    d <- vp_calc(); f <- d$f; t <- d$t
    tt <- seq(0, t, length.out = 500); y <- exp(-f * tt)
    ## rho < 0 makes the kernel grow, so the ceiling follows the data
    ytop <- max(1, max(y, na.rm = TRUE))
    par(mar = c(3.2, 1, 1, 1))
    plot(tt, y, type = "n", ylim = c(0, ytop), xlab = "", ylab = "",
         axes = FALSE, family = "serif")

    tf <- seq(d$s, t, length.out = 250)                       # far region
    polygon(c(d$s, tf, t), c(0, exp(-f * tf), 0), col = "grey88", border = NA)
    tn <- seq(0, d$s, length.out = 250)                       # near region
    polygon(c(0, tn, d$s), c(0, exp(-f * tn), 0),
            col = adjustcolor(pal[["SPBlue"]], 0.20), border = NA)
    tN <- seq(0, d$N, length.out = 250)                       # first N years
    polygon(c(0, tN, d$N), c(0, exp(-f * tN), 0),
            col = adjustcolor(pal[["SPRed"]], 0.22), border = NA)
    abline(v = d$N, col = pal[["SPRed"]], lwd = 1.5)
    ## rho = 0 is not a degenerate case to be survived: it is the state in
    ## which value share IS time share, and the reference case under which
    ## Norway and the Netherlands measure shortfall. Say so.
    if (abs(f) < 1e-9)
      text(t/2, 0.5, family = "serif", col = "grey35", cex = 1.05,
           labels = "\u03c1 = 0: every year weighs the same.\nValue share = time share.")
    lines(tt, y, col = pal[["SPBlue"]], lwd = 2.5)
    abline(v = d$s, lty = 2, col = "grey45")
    ticks <- sort(unique(round(c(0, d$N, d$s, t), 1)))
    axis(1, at = ticks,
         labels = ifelse(ticks == round(t, 1), paste0(ticks, " yr"), ticks))


  output$vp_nsh_label <- renderUI(tags$small(sprintf(
    "first %d%% of future (%.1f of %.1f yr) holds",
    round(100 * vp_calc()$N / vp_calc()$t), vp_calc()$N, vp_calc()$t)))

  output$vp_sentence <- renderUI({
    d <- vp_calc()
    if (abs(d$f) < 1e-9)
      return(HTML(sprintf(paste0("At <b>\u03c1 = 0</b> the first <b>%.1f years</b> hold ",
                                 "<b>%.0f%%</b> of the value \u2013 exactly their share of ",
                                 "the horizon. No balance split exists: the kernel is flat, ",
                                 "so every split is a balance point."),
                          d$N, 100 * d$nshare)))
    HTML(sprintf(paste0("The first <b>%.1f years</b> hold <b>%.1f%%</b> of the present value. ",
                        "Balance split at <b>%.1f yr</b>: the near <b>%d%%</b> of the horizon ",
                        "holds <b>%d%%</b> of the value."),
                 d$N, 100 * d$nshare, d$s, round(100 * d$s / d$t), round(100 * d$rp)))
  })

  output$vp_rp   <- renderText(sprintf("%.3f", vp_calc()$rp))
  output$vp_nsh  <- renderText(sprintf("%.1f%% of value", 100 * vp_calc()$nshare))
  output$vp_u    <- renderText(sprintf("%.2f", vp_calc()$u))
  output$vp_note <- renderText(vp_horizon()$note)
    
  })
  
  ## SERVER mode: no disk write
  output$cond_download <- downloadHandler(
    filename = function() paste0(input$cond_id %||% "condition", ".rds"),
    content  = function(file) saveRDS(cond_candidate(), file))
}


### Final CALL ####
shinyApp(ui, server)


