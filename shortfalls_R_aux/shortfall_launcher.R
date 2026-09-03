## =====================================================================
## shortfall_launcher.R -- detached instances of the shortfalls app
## ---------------------------------------------------------------------
## Adapted from corapp's launcher/registry (same author, same doctrine):
## callr::r_bg with cleanup = FALSE so the child outlives the handle, a
## disk registry keyed pid + process-creation-time (pids are recycled;
## the ctime stops a stale entry naming an unrelated process), readiness
## read from the child's log. What corapp has and this deliberately does
## NOT: snapshot/push data plumbing (the app has its own container +
## overlay contract on disk) and dump mode (the app is one script, so
## the child just setwd()s and runs it).
##
## Usage, from any R session:
##   source("shortfalls_R_aux/shortfall_launcher.R")
##   shortfall()            # launch, open browser, session stays free
##   shortfall_ps()         # list instances (k, port, pid, started)
##   shortfall_kill(1)      # terminate instance k
##   shortfall_kill("all")
##
## NOTE on multiple instances: all instances under one app_home share
## shortfalls_data/shortfalls_data_user.rds. Viewing concurrently is
## fine; AUTHORING from two instances at once can lose a write (the
## save handlers are read-modify-write with no lock). shortfall()
## therefore warns when an instance is already running.
## All strings ASCII (deploy locale is not ours to assume).
## =====================================================================

.shortfall_handles <- new.env(parent = emptyenv())

## registry ------------------------------------------------------------#
.shortfall_root <- function() {
  r <- getOption("shortfall.root", tools::R_user_dir("shortfall", "cache"))
  dir.create(file.path(r, "registry"),  recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(r, "instances"), recursive = TRUE, showWarnings = FALSE)
  r
}

.shortfall_alive <- function(pid, ctime) {
  if (is.null(pid) || is.na(pid)) return(FALSE)
  isTRUE(tryCatch({
    h <- ps::ps_handle(as.integer(pid))
    isTRUE(all.equal(as.numeric(ps::ps_create_time(h)), as.numeric(ctime),
                     tolerance = 1e-3))
  }, error = function(e) FALSE))
}

.shortfall_registry <- function(prune = TRUE) {
  rd <- file.path(.shortfall_root(), "registry")
  f  <- list.files(rd, pattern = "\\.rds$", full.names = TRUE)
  if (!length(f)) return(list())
  reg <- lapply(f, function(p) tryCatch(readRDS(p), error = function(e) NULL))
  names(reg) <- basename(f)
  reg <- Filter(Negate(is.null), reg)
  if (!length(reg)) return(list())
  if (prune) {
    dead <- !vapply(reg, function(e) .shortfall_alive(e$pid, e$ctime), logical(1))
    if (any(dead)) {
      unlink(file.path(rd, names(reg)[dead]))
      unlink(vapply(reg[dead], function(e) e$dir, character(1)), recursive = TRUE)
      reg <- reg[!dead]
    }
  }
  if (!length(reg)) return(list())
  reg[order(vapply(reg, function(e) as.numeric(e$started), numeric(1)))]
}

.shortfall_reg_write <- function(entry) {
  saveRDS(entry, file.path(.shortfall_root(), "registry",
                           paste0(entry$id, ".rds")))
  invisible(entry)
}

## app location --------------------------------------------------------#
## The newest shortfalls_app_<v>_<v>_<v>.R under app_home, by version.
.shortfall_find_app <- function(app_home) {
  f <- list.files(app_home, pattern = "^shortfalls_app_\\d+_\\d+_\\d+\\.R$")
  if (!length(f)) stop("shortfall: no shortfalls_app_*.R found in '",
                       app_home, "'.", call. = FALSE)
  v <- lapply(regmatches(f, regexec("_(\\d+)_(\\d+)_(\\d+)\\.R$", f)),
              function(m) as.integer(m[-1]))
  f[order(vapply(v, function(x) sum(x * c(1e6, 1e3, 1)), numeric(1)))][length(f)]
}

## launcher ------------------------------------------------------------#

#' Launch a detached shortfalls-app instance.
#'
#' Starts the app in a background process and returns at once; the
#' calling session stays free and the instance survives it. With
#' instances already running and no arguments, an interactive menu
#' offers open / terminate / add (mirrors corapp()).
#'
#' app_home: the folder holding the app script and shortfalls_data/.
#'           Defaults to option "shortfall.app_home", else getwd().
shortfall <- function(app_home = getOption("shortfall.app_home", getwd()),
                      open = TRUE, tabname = NULL) {
  for (p in c("shiny", "callr", "ps", "httpuv"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("shortfall: package '", p, "' is required.", call. = FALSE)

  app_home <- normalizePath(app_home, winslash = "/", mustWork = TRUE)
  reg <- .shortfall_registry()

  if (length(reg) && interactive() && missing(app_home) && missing(tabname)) {
    act <- .shortfall_menu(reg)
    if (is.null(act)) return(invisible(NULL))
    if (identical(act$what, "open")) {
      utils::browseURL(reg[[act$k]]$url)
      return(invisible(reg[[act$k]]$url))
    }
    if (identical(act$what, "kill"))    { shortfall_kill(act$k); return(invisible(NULL)) }
    if (identical(act$what, "killall")) { shortfall_kill("all"); return(invisible(NULL)) }
    ## "new" falls through to launch
  }

  if (length(reg))
    message("shortfall: ", length(reg), " instance(s) already running. ",
            "Concurrent VIEWING is fine; concurrent AUTHORING can lose ",
            "a write to the shared user overlay.")

  script <- .shortfall_find_app(app_home)
  id  <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S-"), sample(1e4:9e4, 1))
  dir <- normalizePath(file.path(.shortfall_root(), "instances", id),
                       winslash = "/", mustWork = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  log  <- file.path(dir, "log.txt")
  port <- httpuv::randomPort()

  ## cleanup = FALSE: processx would otherwise kill the child when the
  ## local handle is garbage-collected. supervise = FALSE governs death
  ## on session exit and is a separate mechanism. (See corapp launcher.)
  pr <- callr::r_bg(
    function(app_home, script, port) {
      setwd(app_home)
      shiny::runApp(script, port = port, host = "127.0.0.1",
                    launch.browser = FALSE)
    },
    args = list(app_home = app_home, script = script, port = port),
    stdout = log, stderr = log,
    supervise = FALSE, cleanup = FALSE
  )

  url <- sprintf("http://127.0.0.1:%d", port)
  if (!.shortfall_wait(url, timeout = getOption("shortfall.timeout", 60),
                       proc = pr, log = log)) {
    message(if (pr$is_alive()) "shortfall: instance did not answer in time."
            else "shortfall: child process died during startup.")
    message("Log tail:")
    if (file.exists(log))
      writeLines(utils::tail(readLines(log, warn = FALSE), 20))
    try(pr$kill(), silent = TRUE)
    return(invisible(NULL))
  }

  entry <- list(id = id, dir = dir, port = port, url = url,
                app_home = app_home, script = script,
                pid = pr$get_pid(),
                ctime = as.numeric(ps::ps_create_time(ps::ps_handle(pr$get_pid()))),
                started = Sys.time(),
                tabname = tabname %||% script, log = log)
  .shortfall_reg_write(entry)
  assign(id, pr, envir = .shortfall_handles)

  message("shortfall: instance running at ", url, "  [pid ", entry$pid,
          ", ", script, "]")
  if (open) utils::browseURL(url)
  invisible(url)
}

.shortfall_menu <- function(reg) {
  cat("\nshortfall: ", length(reg), " instance",
      if (length(reg) != 1) "s", " running\n\n", sep = "")
  for (i in seq_along(reg)) {
    e <- reg[[i]]
    cat(sprintf("  %d  port %-6s started %s  %s  [pid %s]\n",
                i, e$port, format(e$started, "%H:%M"), e$script, e$pid))
  }
  cat("\n  n  new instance\n  o  open <k> in browser\n  t  terminate <k>",
      "\n  T  terminate all\n  q  cancel\n\n", sep = "")
  ans <- trimws(readline("shortfall> "))
  if (!nzchar(ans) || substr(ans, 1, 1) == "q") return(NULL)
  cmd <- substr(ans, 1, 1)
  k   <- suppressWarnings(as.integer(trimws(substring(ans, 2))))
  if (cmd %in% c("o", "t") && (is.na(k) || k < 1 || k > length(reg))) {
    k <- suppressWarnings(as.integer(trimws(readline("which instance? "))))
    if (is.na(k) || k < 1 || k > length(reg)) return(NULL)
  }
  switch(cmd,
         n = list(what = "new"),
         o = list(what = "open", k = k),
         t = list(what = "kill", k = k),
         T = list(what = "killall"),
         NULL)
}

## Readiness: shiny announces itself on stdout, teed to the log; watch
## that first (a cold child loading plotly takes a while), then probe
## the socket, and test liveness LAST so a fast-dying child still has
## its log read.
.shortfall_wait <- function(target, timeout = 60, proc = NULL, log = NULL) {
  t0 <- Sys.time()
  repeat {
    if (!is.null(log) && file.exists(log)) {
      txt <- tryCatch(readLines(log, warn = FALSE),
                      error = function(e) character())
      if (any(grepl("Listening on", txt, fixed = TRUE))) return(TRUE)
    }
    con <- suppressWarnings(try(url(target, open = "rb"), silent = TRUE))
    if (!inherits(con, "try-error")) {
      try(close(con), silent = TRUE)
      return(TRUE)
    }
    if (!is.null(proc) && !proc$is_alive()) return(FALSE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      return(FALSE)
    Sys.sleep(0.3)
  }
}

## ps / kill -----------------------------------------------------------#
shortfall_ps <- function() {
  reg <- .shortfall_registry()
  if (!length(reg)) {
    message("shortfall: no instances running.")
    return(invisible(data.frame()))
  }
  d <- data.frame(
    k       = seq_along(reg),
    port    = vapply(reg, function(e) e$port, numeric(1)),
    started = format(do.call(c, lapply(reg, function(e) e$started)), "%H:%M"),
    script  = vapply(reg, function(e) e$script, character(1)),
    pid     = vapply(reg, function(e) e$pid, numeric(1)),
    stringsAsFactors = FALSE)
  rownames(d) <- NULL
  d
}

shortfall_kill <- function(k = "all") {
  reg <- .shortfall_registry()
  if (!length(reg)) {
    message("shortfall: no instances running.")
    return(invisible(NULL))
  }
  idx <- if (identical(k, "all")) seq_along(reg) else as.integer(k)
  idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(reg)]
  for (i in idx) {
    e <- reg[[i]]
    tryCatch(
      if (.shortfall_alive(e$pid, e$ctime))
        ps::ps_kill(ps::ps_handle(as.integer(e$pid))),
      error = function(err) NULL)
    unlink(file.path(.shortfall_root(), "registry", paste0(e$id, ".rds")))
    unlink(e$dir, recursive = TRUE)
    message("shortfall: terminated instance on port ", e$port, ".")
  }
  invisible(NULL)
}

if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x
