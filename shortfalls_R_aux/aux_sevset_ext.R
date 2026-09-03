## sevset_ext.R  (spec 2.0.0) -- container + lifecycle + setters ####
## ---------------------------------------------------------------------#
## Sourced AFTER sevset.R. Leaves the trusted core untouched; adds:
##   - lifecycle fields (status, supersedes) via a re-exported constructor
##   - container accessors (list_sevsets, read_sevset, sevset_info,
##     list_conditions, read_condition, condition_info)
##   - guarded setters (add_sevset, add_norm, add_ltbl, add_condition)
##   - load_shortfalls (canonical + overlay) and compatible()
##
## The container `shortfalls_data` is a list of FOUR keyed sublists
## (sf_thresholds, hrqol_norms, life_tables, sf_conditions) with
## attributes spec_version / built / builder / defaults.
## See SPEC_threshold_sets.md.
##
## SPEC 2.0.0 vs 1.0.0: adds the sf_conditions sublist, and renames the
## container family (new_shortfalls / validate_shortfalls /
## load_shortfalls, class "shortfalls_data", files shortfalls_data.rds
## and shortfalls_data_user.rds). The major bump is deliberate: a 1.x
## reader must NOT silently accept a 2.x container, and distinct names
## keep the two pedigrees separable on disk and in grep.
##
## A condition names its reference by CONTAINER ID (not by copied
## vectors): the norms and tables it was measured against live in the
## same object, so the reference is resolvable and its presence is
## checkable. add_condition enforces that; validate_shortfalls re-checks
## it for the whole store.
## =====================================================================#

SPEC_VERSION  <- "2.0.0"
STATUS_LEVELS <- c("current", "provisional", "superseded", "deprecated")

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

## lifecycle-aware constructor ####
## ---------------------------------------------------------------------#
## Wraps new_sevset() from sevset.R, then attaches the lifecycle and
## origin fields the core v0.1 does not carry, and re-validates including
## the new invariants (15,16). `supersedes_ok` lets the builder assemble
## a set whose predecessor is added later in the same session (checked at
## container level by validate_shortfalls instead).
new_sevset2 <- function(..., status = "current", supersedes = NA_character_,
                        derived_from = NA_character_, canonical = TRUE,
                        validate = TRUE) {
  set <- new_sevset(..., canonical = canonical, validate = FALSE)
  set$status       <- status
  set$supersedes   <- supersedes
  set$derived_from <- derived_from
  if (validate) validate_sevset2(set)
  set
}

## extra invariants (15,16 of the spec) layered on the core validator.
validate_sevset2 <- function(set) {
  validate_sevset(set)                                   # core 1-14
  err <- function(...) stop(sprintf("sevset '%s': %s",
                                    set$id %||% "?", paste0(...)), call. = FALSE)
  st <- set$status %||% "current"
  if (!st %in% STATUS_LEVELS)
    err("status must be one of ", paste(STATUS_LEVELS, collapse = "/"))
  ## (16) supersedes-target existence is a container-level check
  ##      (the predecessor lives in the same sublist) -> validate_shortfalls.
  invisible(TRUE)
}

## container constructor + validator ####
## ---------------------------------------------------------------------#
new_shortfalls <- function(sf_thresholds = list(), hrqol_norms = list(),
                           life_tables = list(), sf_conditions = list(),
                           defaults = list(), builder = NA_character_) {
  structure(
    list(sf_thresholds = sf_thresholds,
         hrqol_norms   = hrqol_norms,
         life_tables   = life_tables,
         sf_conditions = sf_conditions),
    class        = "shortfalls_data",
    spec_version = SPEC_VERSION,
    built        = Sys.Date(),
    builder      = builder,
    defaults     = defaults)
}

validate_shortfalls <- function(store) {
  if (!inherits(store, "shortfalls_data"))
    stop("not a shortfalls_data container", call. = FALSE)
  err <- function(...) stop(paste0("shortfalls_data: ", ...), call. = FALSE)

  if (!identical(attr(store, "spec_version"), SPEC_VERSION))
    err("spec_version mismatch (have ", attr(store, "spec_version") %||% "NULL",
        ", expect ", SPEC_VERSION, ")")

  for (sub in c("sf_thresholds", "hrqol_norms", "life_tables")) {
    lst <- store[[sub]]
    if (is.null(lst)) err("missing sublist '", sub, "'")
    ids <- names(lst)
    if (length(ids) && (anyNA(ids) || any(ids == "")))
      err(sub, ": every entry must be named")
    if (anyDuplicated(ids)) err(sub, ": duplicate ids")
    for (id in ids) if (!identical(lst[[id]]$id %||% id, id) && sub == "sf_thresholds")
      err(sub, ": entry '", id, "' id-field disagrees with key")
  }

  ## each sevset validates, and any supersedes-target is present
  ids <- names(store$sf_thresholds)
  for (id in ids) {
    s <- store$sf_thresholds[[id]]
    validate_sevset2(s)
    sup <- s$supersedes %||% NA_character_
    if (!is.na(sup) && !sup %in% ids)
      err("sevset '", id, "' supersedes absent id '", sup, "'")
  }

  ## sf_conditions: tolerated when absent (a hand-built or partially
  ## migrated store may not carry it), fully checked when present. Each
  ## condition must resolve its reference INSIDE this store -- the
  ## integrity guarantee the loose-file format could not offer.
  cond <- store$sf_conditions %||% list()
  cids <- names(cond)
  if (length(cids) && (anyNA(cids) || any(cids == "")))
    err("sf_conditions: every entry must be named")
  if (anyDuplicated(cids)) err("sf_conditions: duplicate ids")
  for (id in cids) {
    o <- cond[[id]]
    if (!is.data.frame(o) || !all(c("age", "hrqol", "mu") %in% names(o)))
      err("sf_conditions: '", id, "' must be a data.frame with age/hrqol/mu")
    if (is.null(attr(o, "menu_label")))
      err("sf_conditions: '", id, "' has no menu_label")
    r <- attr(o, "reference") %||% list()
    if (is.null(r$table) || !r$table %in% names(store$life_tables))
      err("sf_conditions: '", id, "' references absent life table '",
          r$table %||% "NULL", "'")
    if (is.null(r$norm) || !r$norm %in% names(store$hrqol_norms))
      err("sf_conditions: '", id, "' references absent norm '",
          r$norm %||% "NULL", "'")
  }

  ## defaults name entries that exist
  d <- attr(store, "defaults") %||% list()
  if (!is.null(d$sevset) && !d$sevset %in% names(store$sf_thresholds))
    err("defaults$sevset '", d$sevset, "' not present")
  if (!is.null(d$norm) && !d$norm %in% names(store$hrqol_norms))
    err("defaults$norm '", d$norm, "' not present")
  if (!is.null(d$table) && !d$table %in% names(store$life_tables))
    err("defaults$table '", d$table, "' not present")
  if (!is.null(d$condition) && !d$condition %in% names(cond))
    err("defaults$condition '", d$condition, "' not present")
  invisible(TRUE)
}

## guarded setters -- validate the incoming object at the door ####
## ---------------------------------------------------------------------#
add_sevset <- function(store, id, obj) {
  if (!inherits(obj, "sevset")) stop("add_sevset: obj is not a sevset", call. = FALSE)
  obj$id <- id
  validate_sevset2(obj)
  store$sf_thresholds[[id]] <- obj
  attr(store, "built") <- Sys.Date()
  store
}

## norms/tables: require the provenance attributes compatible() relies on.
.require_attrs <- function(obj, need, what, id) {
  miss <- need[vapply(need, function(a) is.null(attr(obj, a)), logical(1))]
  if (length(miss))
    stop(sprintf("add_%s('%s'): missing attribute(s): %s",
                 what, id, paste(miss, collapse = ", ")), call. = FALSE)
}

add_norm <- function(store, id, obj) {
  .require_attrs(obj, c("region", "valueset", "source"), "norm", id)
  store$hrqol_norms[[id]] <- obj
  attr(store, "built") <- Sys.Date()
  store
}

add_ltbl <- function(store, id, obj) {
  .require_attrs(obj, c("region", "source"), "ltbl", id)
  store$life_tables[[id]] <- obj
  attr(store, "built") <- Sys.Date()
  store
}

## conditions: a data.frame(age, hrqol, mu) holding the standard-of-care
## arm, plus menu_label / info / hit_spec, and a reference naming
## CONTAINER IDS. The reference must resolve in THIS store, so add the
## norms and tables BEFORE the conditions that name them.
add_condition <- function(store, id, obj) {
  if (!is.data.frame(obj) || !all(c("age", "hrqol", "mu") %in% names(obj)))
    stop(sprintf("add_condition('%s'): need a data.frame with age/hrqol/mu", id),
         call. = FALSE)
  .require_attrs(obj, c("menu_label", "reference"), "condition", id)
  r <- attr(obj, "reference")
  if (is.null(r$table) || !r$table %in% names(store$life_tables))
    stop(sprintf("add_condition('%s'): reference$table '%s' is not in the container",
                 id, r$table %||% "NULL"), call. = FALSE)
  if (is.null(r$norm) || !r$norm %in% names(store$hrqol_norms))
    stop(sprintf("add_condition('%s'): reference$norm '%s' is not in the container",
                 id, r$norm %||% "NULL"), call. = FALSE)
  if (is.null(store$sf_conditions)) store$sf_conditions <- list()
  store$sf_conditions[[id]] <- obj
  attr(store, "built") <- Sys.Date()
  store
}

## accessors ####
## ---------------------------------------------------------------------#
list_sevsets <- function(store, include_historical = FALSE) {
  lst <- store$sf_thresholds
  rows <- lapply(names(lst), function(id) {
    s <- lst[[id]]
    data.frame(id = id, label = s$label %||% id,
               region = s$region %||% NA, year = s$year %||% NA,
               measures = paste(s$measures, collapse = "+"),
               n_bands = max(vapply(s$bands, nrow, 0L)),
               status = s$status %||% "current",
               discount = isTRUE(s$discount_shortfall),
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(id = character(), label = character(), region = character(),
               year = integer(), measures = character(), n_bands = integer(),
               status = character(), discount = logical())
  if (!include_historical)
    out <- out[out$status %in% c("current", "provisional"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

read_sevset <- function(store, id) {
  s <- store$sf_thresholds[[id]]
  if (is.null(s)) stop("no sevset '", id, "' in store", call. = FALSE)
  s
}

sevset_info <- function(store, id) {
  s <- read_sevset(store, id)
  cat(sprintf("%s  [%s]\n", s$label %||% id, s$status %||% "current"))
  cat(sprintf("  region/year : %s / %s\n", s$region %||% "?", s$year %||% "?"))
  cat(sprintf("  measures    : %s   rule: %s\n",
              paste(s$measures, collapse = "+"), s$rule))
  cat(sprintf("  discount SF : %s%s\n", isTRUE(s$discount_shortfall),
              if (isTRUE(s$discount_shortfall)) sprintf(" @ %.1f%%", 100 * s$discount_rate) else ""))
  cat(sprintf("  weight      : %s (base %s %s)\n", s$weight_native,
              s$ce_threshold %||% "-", s$currency %||% ""))
  if (!is.na(s$supersedes %||% NA)) cat(sprintf("  supersedes  : %s\n", s$supersedes))
  hint <- s$reference_hint %||% list()
  if (length(hint)) cat(sprintf("  pairs with  : table=%s norm=%s\n",
                                hint$table %||% "-", hint$norm %||% "-"))
  p <- s$provenance %||% list()
  if (!is.null(p$source)) cat(sprintf("  source      : %s\n", p$source))
  if (!is.null(p$notes))  cat(sprintf("  notes       : %s\n", p$notes))
  invisible(s)
}

list_conditions <- function(store) {
  lst <- store$sf_conditions %||% list()
  if (!length(lst))
    return(data.frame(id = character(0), label = character(0),
                      table = character(0), norm = character(0),
                      knots = logical(0), canonical = logical(0),
                      stringsAsFactors = FALSE))
  refget <- function(o, f) (attr(o, "reference") %||% list())[[f]] %||% NA_character_
  data.frame(
    id        = names(lst),
    label     = vapply(lst, function(o) attr(o, "menu_label") %||% "", ""),
    table     = vapply(lst, refget, "", f = "table"),
    norm      = vapply(lst, refget, "", f = "norm"),
    knots     = vapply(lst, function(o) {
                  hs <- attr(o, "hit_spec")
                  !is.null(hs) && !is.null(hs$or_knots) && !is.null(hs$hrqol_knots)
                }, logical(1)),
    canonical = vapply(lst, function(o) isTRUE(attr(o, "canonical")), logical(1)),
    row.names = NULL, stringsAsFactors = FALSE)
}

read_condition <- function(store, id) {
  o <- (store$sf_conditions %||% list())[[id]]
  if (is.null(o)) stop("no condition '", id, "' in store", call. = FALSE)
  o
}

condition_info <- function(store, id) {
  o <- read_condition(store, id)
  r <- attr(o, "reference") %||% list()
  hs <- attr(o, "hit_spec")
  cat(sprintf("%s\n", attr(o, "menu_label") %||% id))
  cat(sprintf("  reference   : table=%s  norm=%s\n",
              r$table %||% "-", r$norm %||% "-"))
  if (!is.null(r$fingerprint)) cat(sprintf("  fingerprint : %s\n", r$fingerprint))
  if (!is.null(r$remapped_from))
    cat(sprintf("  remapped    : table='%s' norm='%s'\n",
                r$remapped_from$table %||% "-", r$remapped_from$norm %||% "-"))
  if (is.null(hs)) {
    cat("  knots       : none (viewable, not knot-editable)\n")
  } else {
    cat(sprintf("  knots       : OR %d, HRQoL %d   interp: %s / %s\n",
                nrow(hs$or_knots %||% data.frame()),
                nrow(hs$hrqol_knots %||% data.frame()),
                hs$or_interp %||% "?", hs$hrqol_interp %||% "?"))
  }
  inf <- attr(o, "info") %||% list()
  if (!is.null(inf$summary) && nzchar(inf$summary))
    cat(sprintf("  summary     : %s\n", inf$summary))
  cat(sprintf("  canonical   : %s\n", isTRUE(attr(o, "canonical"))))
  invisible(o)
}

## load + overlay ####
## ---------------------------------------------------------------------#
## Canonical is read-only; overlay (if present) is unioned on top, but
## canonical ids win a collision. Overlay entries are marked canonical
## = FALSE on the way in. Revalidation of overlay fingerprints is done by
## compatible()/the app, not here.
##
## NOTE: the overlay is written by read-modify-write of the WHOLE object
## (see the app's save handlers). Never write only your own sublist -- a
## partial write silently destroys the other sublists' entries.
load_shortfalls <- function(canonical = "shortfalls_data/shortfalls_data.rds",
                            overlay   = "shortfalls_data/shortfalls_data_user.rds") {
  store <- readRDS(canonical)
  validate_shortfalls(store)
  if (nzchar(overlay) && file.exists(overlay)) {
    ov <- readRDS(overlay)
    for (sub in c("sf_thresholds", "hrqol_norms", "life_tables", "sf_conditions")) {
      for (id in names(ov[[sub]])) {
        if (id %in% names(store[[sub]])) next        # canonical wins
        obj <- ov[[sub]][[id]]
        ## a sevset is a list (field); a condition is a data.frame (attr)
        if (sub == "sf_thresholds") obj$canonical <- FALSE
        if (sub == "sf_conditions") attr(obj, "canonical") <- FALSE
        store[[sub]][[id]] <- obj
      }
    }
    attr(store, "has_overlay") <- TRUE
  }
  store
}

## compatibility ####
## ---------------------------------------------------------------------#
## CHARACTERISE a triplet, never block. Returns list(status, reasons).
## norm_id/table_id are the chosen container keys (authoritative for
## hint-matching; no reliance on a stamped id_key attr).
##
## KEEP IN SYNC with the inlined copy in the app: the app copy renders the
## badge, this one feeds the build report. They must agree.

## jurisdiction-agnostic references (e.g. the full-health norm). Compared
## case-insensitively: the build script stamps "Any", earlier code "any".
ANY_REGION <- c("any", "universal", "generic")
.is_any_region <- function(x) !is.null(x) && !is.na(x) && tolower(x) %in% ANY_REGION

compatible <- function(set, norm, ltbl, norm_id, table_id, override = NULL) {
  ### NEVER RENAME <var> "status" or "reasons" without correcting demote's <var> <<- lines!!
  status <- "green"; reasons <- character(0)
  demote <- function(to, why) {
    rank <- c(green = 1L, amber = 2L, red = 3L)
    if (rank[[to]] > rank[[status]]) status <<- to ## WARNING must be updated on renaming local variable status
    reasons <<- c(reasons, why) ## WARNING must be updated on renaming local variable reasons
  }
  ### NEVER RENAME <var> "status" or "reasons" without correcting demote's <var> <<- lines!!
  hint <- set$reference_hint %||% list()
  hint_named <- length(hint) && !is.na(hint$norm %||% NA) && !is.na(hint$table %||% NA)
  hint_met   <- hint_named &&
    identical(norm_id,  hint$norm) &&
    identical(table_id, hint$table)

  nr <- attr(norm, "region"); tr <- attr(ltbl, "region"); sr <- set$region

  if (hint_met) {
    ## koscher: skip region check (set region "England & Wales" is a
    ## deliberate superset of the England-coverage reference data).
  } else if (hint_named) {
    demote("amber", sprintf("Non-standard pairing: %s expects norm='%s', table='%s' (loaded norm='%s', table='%s')",
                            set$id, hint$norm, hint$table, norm_id, table_id))
  } else {
    regs <- unique(stats::na.omit(c(sr, nr, tr)))
    regs <- regs[!vapply(regs, .is_any_region, logical(1))]
    if (length(regs) > 1)
      demote("red", sprintf("Region mismatch (set=%s, norm=%s, table=%s); tariff vintage differs, measured AS not comparable",
                            sr %||% "?", nr %||% "?", tr %||% "?"))
    else
      demote("amber", "no reference_hint; pairing plausible but unverified")
  }

  ## cross-jurisdiction norm vs table (independent of the hint): a pooled
  ## norm was survivor-weighted against ITS OWN table's sex-mix, so an
  ## off-diagonal pairing is doubly conditional. A jurisdiction-agnostic
  ## reference ("any") is exempt -- it belongs to no population.
  if (!is.null(nr) && !is.null(tr) && !is.na(nr) && !is.na(tr) &&
      !.is_any_region(nr) && !.is_any_region(tr) && !identical(nr, tr))
    demote("red", sprintf(paste0("Norm (%s) and life table (%s) are from different jurisdictions; ",
                                 "the pooled norm was mixed against <em>its own</em> table's sex-mix ",
                                 "\u2013 re-pool upstream for a coherent off-diagonal analysis"), nr, tr))

  ## a jurisdiction-agnostic reference is usable but is not a
  ## jurisdiction-matched pairing: omnibus, suitable if not entirely kosher.
  if (.is_any_region(nr) || .is_any_region(tr))
    demote("amber", "jurisdiction-agnostic reference in use (e.g. full health); omnibus, not a jurisdiction-matched pairing")

  ## forced / non-container reference (canonical attr present and FALSE)
  if (isFALSE(attr(norm, "canonical")) || isFALSE(attr(ltbl, "canonical")))
    demote("red", "a forced (non-container) reference is in use; provenance not guaranteed")

  ## user-edited sevset
  if (!is.na(set$derived_from %||% NA))
    demote("red", sprintf("user-edited set (derived from '%s')", set$derived_from))

  ## discount override contradicting the set's native practice
  if (!is.null(override)) {
    native <- isTRUE(set$discount_shortfall)
    if (!identical(isTRUE(override), native))
      demote("red", sprintf("discount override (%s) contradicts set's native practice (%s)",
                            isTRUE(override), native))
  }
  list(status = status, reasons = reasons)
}
