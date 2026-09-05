# shortfalls

Shortfall and severity explorer (Shiny): absolute and proportional QALE
shortfall under the NICE, Norwegian (Magnussen) and Dutch (ZiN) severity
regimes. Presented at PoPS26.

## Run

```r
renv::restore()   # once, after cloning
shiny::runApp("shortfalls_app_0_99_1.R")
```

Or detached (your R session stays free):

```r
source("shortfalls_R_aux/shortfall_launcher.R")
shortfall()        # shortfall_ps() lists instances, shortfall_kill() stops them
```

## A note on documentation (0.99.0)

This release candidate is only modestly self-documenting. The app is
meant for play-and-learn and is fairly intuitive -- pick a severity
regime, a reference pairing and a condition, and watch the shortfall
measures respond -- but tooltips, mouseovers and in-app explanation are
thin. Later versions will carry more of the documentation into the
interface itself. Until then: the compatibility badge under the
selectors is worth watching (it explains itself in words), and the
build scripts in shortfalls_R_aux/ document what every shipped object
is and where its numbers come from.

## Data contract

The app reads shortfalls_data/shortfalls_data.rds (shipped, read-only);
your own authored regimes and conditions are saved to
shortfalls_data_user.rds beside it (created on first save, never
committed). shortfalls_R_aux/ documents the build chain from published
figures to the shipped container.
