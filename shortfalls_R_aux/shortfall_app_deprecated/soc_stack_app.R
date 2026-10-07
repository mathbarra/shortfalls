## =====================================================================
## Shortfall stack: what a condition does to a future, decomposed
## ---------------------------------------------------------------------
## From a vantage age a, builds the reference QALE stack (survival ->
## x HRQoL -> x discount) and overlays a CONDITION arm hit two ways from
## an onset age a':
##   - mortality: one-year death prob q_x raised by an ODDS RATIO
##       q' = OR*q / (1 - q + OR*q)         (maps [0,1] -> [0,1], any OR)
##   - morbidity: norm HRQoL scaled by a factor m in (0, 1]
## Shortfall is the area between the reference and condition QALE curves,
## split into a MORTALITY band and a MORBIDITY band (mortality-first
## ordering; the split is order-dependent by the cross-term - see note).
##
## Scalars from onset now; the per-age hit is vectorised, so an OR-vector
## or an m-vector drops in later with no structural change.
##
## Reference arm = pooled ONS life table + pooled ONS norm, loaded from
## .rds beside this file. Self-contained: no ltbl.mb at runtime.
##   AS/PS are robust to the half-year convention (they are differences);
##   cross-check absolute QALE levels against shortfall() when aligning.
## =====================================================================

library(shiny)

## --- data layer: pooled life table + pooled norm ---------------------
load_or <- function(local, envvar, fallback) {
  p <- if (file.exists(local)) local else Sys.getenv(envvar, "")
  if (nzchar(p) && file.exists(p)) readRDS(p) else fallback()
}
tb_pool <- load_or("tb_pool_ons_2022_2024.rds", "LTBL_APP_TABLE", function() {
  ages <- 0:120
  structure(data.frame(a = ages,
    mu = pmin(1 - exp(-(0.0002 + 1.8e-5 * exp(0.095 * ages))), 1)),
    source = "synthetic Gompertz-Makeham (illustrative)")
})
qn_pool <- load_or("qn_pool_ons_2022_2024.rds", "LTBL_APP_NORM", function() {
  ages <- 0:120
  structure(data.frame(age = ages,
    hrqol = pmin(pmax(0.93 - 0.003 * pmax(ages - 25, 0) -
                        0.002 * pmax(ages - 60, 0), 0.60), 1)),
    source = "synthetic norm (illustrative)")
})

## lookup vectors on integer ages 0..120, flat-extrapolated at the top
grid_vec <- function(df, agecol, valcol, top = 120) {
  v <- numeric(top + 1)
  v[df[[agecol]] + 1] <- df[[valcol]]
  last <- max(df[[agecol]])
  if (last < top) v[(last + 2):(top + 1)] <- df[[valcol]][which.max(df[[agecol]])]
  v
}
qxvec <- grid_vec(as.data.frame(tb_pool), "a", "mu")
qvec  <- grid_vec(as.data.frame(qn_pool), "age", "hrqol")

src_note <- paste0("table: ", attr(tb_pool, "source") %||% "user",
                   "   |   norm: ", attr(qn_pool, "source") %||% "user")
`%||%` <- function(x, y) if (is.null(x)) y else x

## --- the stack: reference + condition, all vectors on one grid -------
build_stack <- function(a, onset, m, OR, r, top = 120, top_plot = 105) {
  K   <- max(1, min(top_plot, top) - a)
  j   <- 0:K
  age <- a + j
  qref <- qvec[pmin(age, top) + 1]                         # norm HRQoL
  qx   <- qxvec[pmin(age, top) + 1]                        # one-year death prob
  hit  <- age >= onset
  qxc  <- ifelse(hit, OR * qx / (1 - qx + OR * qx), qx)    # condition mortality (odds ratio)
  qc   <- ifelse(hit, m * qref, qref)                      # condition HRQoL (scaled)

  surv <- function(qv) c(1, cumprod(1 - qv[-length(qv)]))  # survival to start of year
  Sref <- surv(qx); Sc <- surv(qxc); disc <- (1 + r)^(-j)

  Csq <- Sref * qref; Scm <- Sc * qref; Scc <- Sc * qc     # nested QALE densities
  S <- function(x) sum(x)
  QALEref <- S(Csq); QALEc <- S(Scc)
  QALEref_d <- S(Csq * disc); QALEc_d <- S(Scc * disc)
  list(
    j = j, K = K, a = a, onset = onset,
    Sref = Sref, Csq = Csq, Scm = Scm, Scc = Scc, Scd = Scc * disc,
    LE = S(Sref), QALEref = QALEref, QALEc = QALEc,
    AS = QALEref - QALEc, PS = (QALEref - QALEc) / QALEref,
    mort = S(Csq - Scm), morb = S(Scm - Scc),                 # the two bands
    AS_d = QALEref_d - QALEc_d, PS_d = (QALEref_d - QALEc_d) / QALEref_d,
    QALEref_d = QALEref_d, QALEc_d = QALEc_d
  )
}
nice_w <- function(AS, PS) { w <- 1; if (AS >= 12 || PS >= 0.85) w <- 1.2
                             if (AS >= 18 || PS >= 0.95) w <- 1.7; w }

## =====================================================================
ui <- fluidPage(
  tags$h4("What a condition does to a future"),
  tags$p(style = "color:#666; margin-top:-6px;",
         HTML("Reference QALE stack (survival &times; HRQoL &times; discount) with a condition hit from onset: mortality by odds ratio, HRQoL by a scale factor. Shortfall = area between reference and condition, split into mortality and morbidity bands.")),

  fluidRow(
    column(6, sliderInput("age", "vantage age a (years)", min = 0, max = 90, value = 35, step = 1)),
    column(6, sliderInput("onset", "onset age a' (years)", min = 0, max = 100, value = 45, step = 1))
  ),
  fluidRow(
    column(6, sliderInput("m", "HRQoL scale from onset (1 = none)", min = 0.1, max = 1, value = 0.6, step = 0.05)),
    column(6, sliderInput("OR", "mortality odds ratio from onset (1 = none)", min = 1, max = 20, value = 3, step = 0.5))
  ),
  fluidRow(
    column(4, numericInput("r", "discount rate r (in [0,1])", value = 0.035, min = 0, max = 1, step = 0.005)),
    column(8, tags$p(style = "color:#888; font-size:12px; margin-top:26px;", textOutput("src")))
  ),

  plotOutput("plot", height = "320px"),
  uiOutput("legend"),
  uiOutput("sentence"),

  fluidRow(
    column(3, wellPanel(tags$small("AS undiscounted"), tags$h3(textOutput("AS", inline = TRUE)))),
    column(3, wellPanel(tags$small("PS undiscounted"), tags$h3(textOutput("PS", inline = TRUE)))),
    column(3, wellPanel(tags$small("AS discounted"),  tags$h3(textOutput("ASd", inline = TRUE)))),
    column(3, wellPanel(tags$small("PS discounted"),  tags$h3(textOutput("PSd", inline = TRUE))))
  )
)

## =====================================================================
server <- function(input, output, session) {
  d <- reactive({
    req(is.finite(input$r), input$r >= 0, input$r <= 1)
    build_stack(input$age, input$onset, input$m, input$OR, input$r)
  })

  cols <- list(norm = "#9a988f", mort = "#d85a30", morb = "#e6a817",
               disc = "#2a78d6", qale = "#1d9e75")

  output$plot <- renderPlot({
    x <- d(); j <- x$j; K <- x$K
    par(mar = c(3.4, 1, 0.5, 1))
    plot(NULL, xlim = c(0, K), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE)
    poly <- function(y, col) polygon(c(0, j, K), c(0, y, 0), border = NA, col = col)
    poly(x$Sref, adjustcolor(cols$norm, 0.30))     # [Csq, Sref]  norm HRQoL
    poly(x$Csq,  adjustcolor(cols$mort, 0.55))     # [Scm, Csq]   mortality shortfall
    poly(x$Scm,  adjustcolor(cols$morb, 0.60))     # [Scc, Scm]   morbidity shortfall
    poly(x$Scc,  adjustcolor(cols$disc, 0.42))     # [Scd, Scc]   discounting loss
    poly(x$Scd,  adjustcolor(cols$qale, 0.55))     # under Scd    discounted condition QALE
    lines(j, x$Sref, col = "#6f6e69", lwd = 1.5)
    lines(j, x$Csq,  col = "#7a2e14", lwd = 2)      # reference QALE
    lines(j, x$Scc,  col = "#0c447c", lwd = 2)      # condition QALE
    lines(j, x$Scd,  col = "#0f6e56", lwd = 1.4, lty = 2)
    if (x$onset > x$a) abline(v = x$onset - x$a, col = "grey35", lty = 3)
    at <- seq(0, K, by = 10)
    axis(1, at = at, labels = x$a + at)
    mtext("age  (vantage a + t)", side = 1, line = 2.3, col = "#666", cex = 0.95)
  })

  output$legend <- renderUI({
    x <- d()
    chip <- function(c, lab) tags$span(style = "margin-right:14px; font-size:13px; white-space:nowrap;",
      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;border-radius:2px;background:%s;margin-right:5px;vertical-align:-1px;", c)), lab)
    tags$div(style = "margin:8px 0 4px; line-height:1.9;",
      chip(cols$norm, "norm HRQoL (population)"),
      chip(cols$mort, sprintf("mortality shortfall \u2248 %.1f", x$mort)),
      chip(cols$morb, sprintf("morbidity shortfall \u2248 %.1f", x$morb)),
      chip(cols$disc, "discounting loss"),
      chip(cols$qale, sprintf("discounted condition QALE \u2248 %.1f", x$QALEc_d)))
  })

  output$sentence <- renderUI(HTML({
    x <- d()
    sprintf(paste0("From age <b>%d</b>: reference QALE <b>%.1f</b>. Condition (onset %d, HRQoL &times;%.2f, mortality OR %.1f) leaves <b>%.1f</b>. ",
      "Undiscounted shortfall <b>AS %.1f</b> (PS <b>%.0f%%</b>), weight <b>%.1f</b> \u2014 this is what happens. ",
      "Discounted at r=%.1f%%: <b>AS %.1f</b> (PS <b>%.0f%%</b>), weight <b>%.1f</b>."),
      x$a, x$QALEref, x$onset, input$m, input$OR, x$QALEc,
      x$AS, 100 * x$PS, nice_w(x$AS, x$PS),
      100 * input$r, x$AS_d, 100 * x$PS_d, nice_w(x$AS_d, x$PS_d))
  }))

  output$src <- renderText(src_note)
  output$AS  <- renderText(sprintf("%.1f", d()$AS))
  output$PS  <- renderText(sprintf("%.0f%%", 100 * d()$PS))
  output$ASd <- renderText(sprintf("%.1f", d()$AS_d))
  output$PSd <- renderText(sprintf("%.0f%%", 100 * d()$PS_d))
}

shinyApp(ui, server)
