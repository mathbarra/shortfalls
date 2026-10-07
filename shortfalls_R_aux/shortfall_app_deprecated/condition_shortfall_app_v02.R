## =====================================================================
## Condition shortfall & discounting: explorer + hitter (v01)
## ---------------------------------------------------------------------
## Two tabs, one shared pooled reference (ONS pooled life table + norm):
##
##   EXPLORE : reference QALE stack (survival x HRQoL x discount) with a
##             condition overlaid; shortfall split into mortality and
##             morbidity bands; overlaid or side-by-side. Condition either
##             MANUAL (two sliders) or LOADED from the conditions folder.
##
##   AUTHOR  : the "hitter". Two knot editors (age-specific mortality OR;
##             age-specific HRQoL scale) over the current reference, a live
##             preview of the induced arm, and export to the conditions
##             folder as an attributed data.frame (absolute hit profile +
##             reference fingerprint + hit_spec for reproducibility).
##
## Stored condition contract (folder .rds):
##   data.frame(age, hrqol, mu)                 # already-hit absolute arm
##   attr menu_label : character                # dropdown label
##   attr info       : list(summary, sources=list(list(cite,url), ...))
##   attr reference  : list(table=..., norm=..., fingerprint=...)  # MANDATORY
##   attr hit_spec   : list(onset, or_knots, hrqol_knots, ...)     # recipe
##
## Self-contained: no ltbl.mb at runtime. AS/PS are differences, robust to
## the half-year convention; cross-check absolute levels vs shortfall().
## =====================================================================

library(shiny)

`%||%` <- function(x, y) if (is.null(x)) y else x

pal <- c(
  SPBlue = "#002768", SPGreen = "#64A620", SPRed = "#EF2B2D",
  SPYellow = "#FECA00", SPPurple = "#756FB9", SPBlueLight = "#80A8D9",
  SPBlueLighter = "#CDD2E5", SPBlueLightest = "#E5E8F1",
  SPGreenLight = "#64D292", SPPurpleLight = "#BCB8DB")

## semantic band palette (health = green ramp; harm = warm; discount = cool)
BAND <- list(
  norm = pal[["SPGreenLight"]],   # reference potential ceiling
  mort = pal[["SPRed"]],          # mortality shortfall
  morb = pal[["SPYellow"]],       # morbidity shortfall
  disc = pal[["SPBlueLightest"]], # discounting loss (cool, neutral)
  qale = pal[["SPGreen"]])        # delivered discounted QALE (floor)
STROKE <- list(
  surv = pal[["SPBlue"]],
  ref  = pal[["SPGreen"]],
  cond = pal[["SPRed"]],
  disc = pal[["SPBlue"]],
  onset = pal[["SPPurple"]])

TOP     <- 120
COND_DIR <- Sys.getenv("SP_COND_DIR", "conditions")

## --- reference data layer --------------------------------------------
load_or <- function(local, envvar, fallback) {
  p <- if (file.exists(local)) local else Sys.getenv(envvar, "")
  if (nzchar(p) && file.exists(p)) readRDS(p) else fallback()
}
tb_pool <- load_or("tb_pool_ons_2022_2024.rds", "LTBL_APP_TABLE", function() {
  ages <- 0:TOP
  structure(data.frame(a = ages,
    mu = pmin(1 - exp(-(0.0002 + 1.8e-5 * exp(0.095 * ages))), 1)),
    source = "synthetic Gompertz-Makeham (illustrative)", region = "synthetic")
})
qn_pool <- load_or("qn_pool_ons_2022_2024.rds", "LTBL_APP_NORM", function() {
  ages <- 0:TOP
  structure(data.frame(age = ages,
    hrqol = pmin(pmax(0.93 - 0.003 * pmax(ages - 25, 0) -
                        0.002 * pmax(ages - 60, 0), 0.60), 1)),
    source = "synthetic norm (illustrative)")
})
grid_vec <- function(df, ac, vc, top = TOP) {
  v <- numeric(top + 1); v[df[[ac]] + 1] <- df[[vc]]
  last <- max(df[[ac]])
  if (last < top) v[(last + 2):(top + 1)] <- df[[vc]][which.max(df[[ac]])]
  v
}
qxvec <- grid_vec(as.data.frame(tb_pool), "a", "mu")     # ref one-year death prob
qvec  <- grid_vec(as.data.frame(qn_pool), "age", "hrqol")# ref HRQoL
REF_FP <- digest_like <- {                               # cheap fingerprint (no extra pkg)
  paste0(format(sum(qxvec * seq_along(qxvec)), digits = 15), "_",
         format(sum(qvec  * seq_along(qvec)),  digits = 15))
}
ref_label <- paste0("table: ", attr(tb_pool, "source") %||% "user",
                    "  |  norm: ", attr(qn_pool, "source") %||% "user")

## --- knot -> full age-vector interpolation ---------------------------
## knots: data.frame(age, value). Two fixed ends at 0 and TOP are ensured
## by the caller. Linear interpolation, flat outside the knot span.
interp_knots <- function(knots, base_value) {
  k <- knots[order(knots$age), , drop = FALSE]
  if (nrow(k) == 0) return(rep(base_value, TOP + 1))
  approx(k$age, k$value, xout = 0:TOP, method = "linear", rule = 2)$y
}

## --- odds-ratio mortality hit + scale HRQoL hit ----------------------
or_hit  <- function(q, OR) OR * q / (1 - q + OR * q)      # [0,1] -> [0,1]

## build the stack from explicit age-vectors of OR and HRQoL-scale -----
## or_v, m_v : length TOP+1 vectors over ages 0..TOP (1 = no hit)
build_stack <- function(a, or_v, m_v, r, top_plot = 105) {
  K   <- max(1, min(top_plot, TOP) - a)
  j   <- 0:K; age <- a + j
  qref <- qvec[age + 1]; qx <- qxvec[age + 1]
  qxc  <- or_hit(qx, or_v[age + 1])
  qc   <- m_v[age + 1] * qref
  surv <- function(qv) c(1, cumprod(1 - qv[-length(qv)]))
  Sref <- surv(qx); Sc <- surv(qxc); disc <- (1 + r)^(-j)
  Csq <- Sref * qref; Scm <- Sc * qref; Scc <- Sc * qc
  S <- sum
  list(j = j, K = K, a = a,
       Sref = Sref, Csq = Csq, Scm = Scm, Scc = Scc, Scd = Scc * disc,
       qxc = qxc, qc = qc, Sc = Sc,
       QALEref = S(Csq), QALEc = S(Scc),
       AS = S(Csq) - S(Scc), PS = (S(Csq) - S(Scc)) / S(Csq),
       mort = S(Csq - Scm), morb = S(Scm - Scc),
       AS_d = S(Csq * disc) - S(Scc * disc),
       PS_d = (S(Csq * disc) - S(Scc * disc)) / S(Csq * disc),
       QALEc_d = S(Scc * disc), QALEref_d = S(Csq * disc))
}
nice_w <- function(AS, PS) { w <- 1; if (AS >= 12 || PS >= 0.85) w <- 1.2
                             if (AS >= 18 || PS >= 0.95) w <- 1.7; w }

## draw one composed stack (overlaid: bands; or a plain reference stack) -
draw_stack <- function(x, bands = TRUE, main = "") {
  j <- x$j; K <- x$K
  par(mar = c(3.4, 1, if (nzchar(main)) 1.6 else 0.5, 1))
  plot(NULL, xlim = c(0, K), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE, main = main)
  poly <- function(y, col) polygon(c(0, j, K), c(0, y, 0), border = NA, col = col)
  if (bands) {
    poly(x$Sref, adjustcolor(BAND$norm, 0.30))
    poly(x$Csq,  adjustcolor(BAND$mort, 0.60))
    poly(x$Scm,  adjustcolor(BAND$morb, 0.60))
    poly(x$Scc,  adjustcolor(BAND$disc, 0.75))
    poly(x$Scd,  adjustcolor(BAND$qale, 0.55))
    lines(j, x$Csq, col = STROKE$ref, lwd = 2)
    lines(j, x$Scc, col = STROKE$cond, lwd = 2)
    lines(j, x$Scd, col = STROKE$disc, lwd = 1.3, lty = 2)
  } else {
    poly(x$Sref, adjustcolor(BAND$norm, 0.30))
    poly(x$Csq,  adjustcolor(BAND$disc, 0.55))
    poly(x$Csq * (1 + 0)^0, adjustcolor(BAND$qale, 0.0)) # noop keep structure
  }
  lines(j, x$Sref, col = STROKE$surv, lwd = 1.5)
  at <- seq(0, K, by = 10); axis(1, at = at, labels = x$a + at)
  mtext("age  (vantage a + t)", side = 1, line = 2.3, col = "#666", cex = 0.9)
}

## harvest conditions folder ------------------------------------------
harvest <- function(dir = COND_DIR) {
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  out <- list()
  for (f in files) {
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    ok  <- is.data.frame(obj) && all(c("age","hrqol","mu") %in% names(obj)) &&
           !is.null(attr(obj, "menu_label"))
    if (ok) out[[attr(obj, "menu_label")]] <- f
  }
  out
}

## =====================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(sprintf(
    ".irs-bar,.irs-single{background:%s!important;border-color:%s!important}",
    pal[["SPBlue"]], pal[["SPBlue"]])))),
  titlePanel("Condition shortfall & discounting"),
  tabsetPanel(id = "tab",

    ## ---------------- EXPLORE ---------------------------------------
    tabPanel("Explore",
      br(),
      fluidRow(
        column(3, selectInput("cond", "condition",
                 choices = c("manual (sliders)" = ""))),
        column(3, radioButtons("view", "layout", inline = TRUE,
                 c("overlaid" = "ov", "side-by-side" = "sbs"), selected = "ov")),
        column(3, numericInput("r", "discount rate \u03c1 [0,1]",
                 value = 0.035, min = 0, max = 1, step = 0.005)),
        column(3, sliderInput("age", "vantage age a", min = 0, max = 90, value = 35, step = 1))
      ),
      conditionalPanel("input.cond == ''",
        fluidRow(
          column(4, sliderInput("onset", "onset a'", min = 0, max = 100, value = 45, step = 1)),
          column(4, sliderInput("m", "HRQoL scale from onset (1=none)",
                   min = 0.1, max = 1, value = 0.6, step = 0.05)),
          column(4, sliderInput("or_log", "mortality OR from onset (log; 1=none)",
                   min = 0, max = 4, value = log10(3), step = 0.01))
        ),
        div(style = "color:#666; font-size:12px; margin-top:-6px;",
            textOutput("or_readout"))),
      plotOutput("explore_plot", height = "330px"),
      uiOutput("legend"),
      uiOutput("compare"),
      uiOutput("cond_info"),
      # fluidRow(
      #   column(3, wellPanel(tags$small("AS undisc"),  tags$h3(textOutput("AS", inline = TRUE)))),
      #   column(3, wellPanel(tags$small("PS undisc"),  tags$h3(textOutput("PS", inline = TRUE)))),
      #   column(3, wellPanel(tags$small("AS disc"),    tags$h3(textOutput("ASd", inline = TRUE)))),
      #   column(3, wellPanel(tags$small("PS disc"),    tags$h3(textOutput("PSd", inline = TRUE))))
      # ),
      div(style = "color:#888; font-size:11px;", textOutput("ref_lab"))
    ),

    ## ---------------- AUTHOR (hitter) --------------------------------
    tabPanel("Author",
      br(),
      fluidRow(
        column(6,
          h5("Mortality OR knots (age, OR 1..10000)"),
          helpText("Ends at 0 and", TOP, "are fixed. Add up to 8 interior knots."),
          numericInput("or_nk", "interior knots", value = 1, min = 0, max = 8, step = 1),
          uiOutput("or_knot_ui"),
          plotOutput("or_preview", height = "150px")),
        column(6,
          h5("HRQoL scale knots (age, scale 0..1)"),
          helpText("1 = no morbidity; lower = worse. Ends fixed as above."),
          numericInput("m_nk", "interior knots", value = 1, min = 0, max = 8, step = 1),
          uiOutput("m_knot_ui"),
          plotOutput("m_preview", height = "150px"))
      ),
      hr(),
      h5("Preview against current reference (vantage age)"),
      fluidRow(
        column(3, sliderInput("a_auth", "vantage age", min = 0, max = 90, value = 0, step = 1)),
        column(3, numericInput("r_auth", "r", value = 0.035, min = 0, max = 1, step = 0.005))
      ),
      plotOutput("auth_plot", height = "300px"),
      uiOutput("auth_sentence"),
      hr(),
      h5("Export condition"),
      fluidRow(
        column(4, textInput("cond_label", "menu label", value = "New condition")),
        column(8, textInput("cond_summary", "info summary", value = ""))),
      fluidRow(
        column(6, textInput("cond_cite", "source citation", value = "")),
        column(6, textInput("cond_url", "source url", value = ""))),
      fluidRow(
        column(4, textInput("cond_file", "file name (.rds)", value = "new_condition.rds")),
        column(4, br(), actionButton("export", "Export to conditions folder", class = "btn-primary"))),
      div(style = "color:#666; font-size:12px;", textOutput("export_msg"))
    )
  )
)

## =====================================================================
server <- function(input, output, session) {

  output$ref_lab <- renderText(ref_label)
  output$or_readout <- renderText(sprintf("OR = %.1f", 10 ^ input$or_log))

  ## ---- conditions dropdown (harvested at startup) ----------------
  conds <- harvest()
  updateSelectInput(session, "cond",
                    choices = c("manual (sliders)" = "", conds))
  current_cond <- reactive({
    if (identical(input$cond, "")) return(NULL)
    readRDS(input$cond)
  })

  ## ---- EXPLORE: assemble OR / HRQoL vectors ----------------------
  vecs <- reactive({
    cc <- current_cond()
    if (is.null(cc)) {
      onset <- input$onset; OR <- 10 ^ input$or_log; m <- input$m
      or_v <- ifelse(0:TOP >= onset, OR, 1)
      m_v  <- ifelse(0:TOP >= onset, m,  1)
    } else {
      ## stored condition already carries absolute (hrqol, mu); recover the
      ## hit vs current reference so the same build_stack path is used.
      cage <- cc$age
      mu_c <- approx(cage, cc$mu,    xout = 0:TOP, rule = 2)$y
      q_c  <- approx(cage, cc$hrqol, xout = 0:TOP, rule = 2)$y
      ## invert or_hit to recover OR(x): OR = q'(1-q) / (q(1-q'))
      or_v <- (mu_c * (1 - qxvec)) / (qxvec * (1 - mu_c)); or_v[!is.finite(or_v)] <- 1
      or_v <- pmax(or_v, 1e-6)
      m_v  <- q_c / qvec; m_v[!is.finite(m_v)] <- 1
    }
    list(or_v = or_v, m_v = m_v)
  })

  ex <- reactive({
    req(is.finite(input$r), input$r >= 0, input$r <= 1)
    v <- vecs(); build_stack(input$age, v$or_v, v$m_v, input$r)
  })

  output$explore_plot <- renderPlot({
    x <- ex()
    if (identical(input$view, "sbs")) {
      par(mfrow = c(1, 2))
      ## reference-only stack (no condition): unit OR, unit scale
      xr <- build_stack(input$age, rep(1, TOP + 1), rep(1, TOP + 1), input$r)
      draw_stack(xr, bands = FALSE, main = "reference")
      draw_stack(x,  bands = TRUE,  main = "with condition")
      par(mfrow = c(1, 1))
    } else {
      draw_stack(x, bands = TRUE)
    }
  })

  output$legend <- renderUI({
    x <- ex()
    chip <- function(c, lab) tags$span(style = "margin-right:14px;font-size:13px;white-space:nowrap;",
      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;border-radius:2px;background:%s;margin-right:5px;vertical-align:-1px;", c)), lab)
    tags$div(style = "margin:8px 0;line-height:1.9;",
      chip(BAND$norm, "norm HRQoL (population)"),
      chip(BAND$mort, sprintf("mortality shortfall \u2248 %.1f", x$mort)),
      chip(BAND$morb, sprintf("morbidity shortfall \u2248 %.1f", x$morb)),
      chip(BAND$disc, "discounting loss"),
      chip(BAND$qale, sprintf("discounted condition QALE \u2248 %.1f", x$QALEc_d)))
  })

  output$compare <- renderUI({
    x <- ex(); r <- input$r
    cell   <- function(v) tags$td(style = "text-align:right;padding:5px 16px;font-variant-numeric:tabular-nums;", v)
    hd     <- function(v) tags$th(style = "text-align:right;padding:5px 16px;font-weight:600;", v)
    rowlab <- function(v) tags$td(style = "text-align:left;padding:5px 16px;color:#555;", v)
    w0 <- nice_w(x$AS, x$PS); wd <- nice_w(x$AS_d, x$PS_d)
    tags$table(style = "border-collapse:collapse;margin:10px 0 16px;font-size:16px;",
               tags$thead(tags$tr(hd(sprintf("Onset at age %d evaluated at age %d", input$onset, x$a)), hd("undiscounted"),
                                  hd(sprintf("Discounted (\u03c1 = %.1f%%)", 100 * r)))),
               tags$tbody(
                 tags$tr(rowlab("reference QALE"), cell(sprintf("%.1f", x$QALEref)), cell(sprintf("%.1f", x$QALEref_d))),
                 tags$tr(rowlab("condition QALE"), cell(sprintf("%.1f", x$QALEc)),   cell(sprintf("%.1f", x$QALEc_d))),
                 tags$tr(style = "border-top:1px solid #ddd;",
                         rowlab(tags$b("absolute shortfall (AS)")),
                         cell(tags$b(sprintf("%.1f", x$AS))), cell(tags$b(sprintf("%.1f", x$AS_d)))),
                 tags$tr(rowlab(tags$b("proportional shortfall (PS)")),
                         cell(tags$b(sprintf("%.0f%%", 100 * x$PS))), cell(tags$b(sprintf("%.0f%%", 100 * x$PS_d)))),
                 tags$tr(style = "border-top:1px solid #ddd;",
                         rowlab("NICE weight"), cell(sprintf("%.1f", w0)), cell(sprintf("%.1f", wd)))))
  })

  output$cond_info <- renderUI({
    cc <- current_cond(); info <- attr(cc, "info"); req(info)
    tagList(tags$p(style = "font-style:italic;", info$summary %||% ""),
      lapply(info$sources %||% list(), function(s)
        tags$div(tags$a(href = s$url, target = "_blank", s$cite %||% s$url))))
  })

  output$AS  <- renderText(sprintf("%.1f", ex()$AS))
  output$PS  <- renderText(sprintf("%.0f%%", 100 * ex()$PS))
  output$ASd <- renderText(sprintf("%.1f", ex()$AS_d))
  output$PSd <- renderText(sprintf("%.0f%%", 100 * ex()$PS_d))

  ## ---- AUTHOR: dynamic knot fields -------------------------------
  ## interior knot ages default spread across (0, TOP); ends fixed.
  knot_fields <- function(prefix, n, vmin, vmax, vdef, vstep) {
    if (n < 1) return(NULL)
    ages <- round(seq(0, TOP, length.out = n + 2))[-c(1, n + 2)]
    lapply(seq_len(n), function(i) fluidRow(
      column(6, numericInput(paste0(prefix, "_age", i), paste("knot", i, "age"),
                             value = ages[i], min = 1, max = TOP - 1, step = 1)),
      column(6, numericInput(paste0(prefix, "_val", i), "value",
                             value = vdef, min = vmin, max = vmax, step = vstep))))
  }
  output$or_knot_ui <- renderUI(knot_fields("or", input$or_nk, 1, 10000, 3, 0.5))
  output$m_knot_ui  <- renderUI(knot_fields("m",  input$m_nk,  0.05, 1, 0.6, 0.05))

  gather_knots <- function(prefix, n, end0, endT) {
    ag <- c(0); vl <- c(end0)
    if (n >= 1) for (i in seq_len(n)) {
      a <- input[[paste0(prefix, "_age", i)]]; v <- input[[paste0(prefix, "_val", i)]]
      if (!is.null(a) && !is.null(v)) { ag <- c(ag, a); vl <- c(vl, v) }
    }
    ag <- c(ag, TOP); vl <- c(vl, endT)
    data.frame(age = ag, value = vl)
  }
  or_knots <- reactive(gather_knots("or", input$or_nk, 1, 1))   # ends = no hit (OR 1)
  m_knots  <- reactive(gather_knots("m",  input$m_nk,  1, 1))   # ends = no hit (scale 1)

  or_vec <- reactive(pmax(interp_knots(or_knots(), 1), 1e-6))
  m_vec  <- reactive(pmin(pmax(interp_knots(m_knots(), 1), 0), 1))

  output$or_preview <- renderPlot({
    par(mar = c(3, 3.4, 0.5, 0.5))
    ov <- or_vec(); Sc <- c(1, cumprod(1 - or_hit(qxvec, ov)[-length(ov)]))
    plot(0:TOP, Sc, type = "l", col = STROKE$cond, lwd = 2, ylim = c(0, 1),
         xlab = "age", ylab = "induced survival S_c")
    lines(0:TOP, c(1, cumprod(1 - qxvec[-length(qxvec)])), col = STROKE$surv, lty = 2)
    kn <- or_knots(); points(kn$age, rep(0.02, nrow(kn)), pch = 17, col = STROKE$onset)
  })
  output$m_preview <- renderPlot({
    par(mar = c(3, 3.4, 0.5, 0.5))
    mv <- m_vec()
    plot(0:TOP, mv * qvec, type = "l", col = STROKE$cond, lwd = 2, ylim = c(0, 1),
         xlab = "age", ylab = "induced HRQoL q_c")
    lines(0:TOP, qvec, col = STROKE$ref, lty = 2)
    kn <- m_knots(); points(kn$age, rep(0.02, nrow(kn)), pch = 17, col = STROKE$onset)
  })

  auth <- reactive({
    req(is.finite(input$r_auth))
    build_stack(input$a_auth, or_vec(), m_vec(), input$r_auth)
  })
  output$auth_plot <- renderPlot(draw_stack(auth(), bands = TRUE))
  output$auth_sentence <- renderUI(HTML({
    x <- auth()
    sprintf(paste0("Preview from age <b>%d</b>: AS <b>%.1f</b> (PS <b>%.0f%%</b>) undisc; ",
      "AS <b>%.1f</b> (PS <b>%.0f%%</b>) at r=%.1f%%."),
      x$a, x$AS, 100 * x$PS, x$AS_d, 100 * x$PS_d, 100 * input$r_auth)
  }))

  ## ---- EXPORT ----------------------------------------------------
  observeEvent(input$export, {
    if (!dir.exists(COND_DIR)) dir.create(COND_DIR, recursive = TRUE)
    ov <- or_vec(); mv <- m_vec()
    df <- data.frame(age = 0:TOP,
                     hrqol = pmin(pmax(mv * qvec, 0), 1),
                     mu    = or_hit(qxvec, ov))
    attr(df, "menu_label") <- input$cond_label
    attr(df, "info") <- list(
      summary = input$cond_summary,
      sources = if (nzchar(input$cond_cite) || nzchar(input$cond_url))
                  list(list(cite = input$cond_cite, url = input$cond_url)) else list())
    attr(df, "reference") <- list(
      table = attr(tb_pool, "source") %||% "user",
      norm  = attr(qn_pool, "source") %||% "user",
      fingerprint = REF_FP)
    attr(df, "hit_spec") <- list(or_knots = or_knots(), hrqol_knots = m_knots(),
                                 interp = "linear-rule2")
    fn <- input$cond_file; if (!grepl("\\.rds$", fn)) fn <- paste0(fn, ".rds")
    path <- file.path(COND_DIR, fn)
    saveRDS(df, path)
    updateSelectInput(session, "cond",
                      choices = c("manual (sliders)" = "", harvest()))
    output$export_msg <- renderText(sprintf("wrote %s  (fingerprint %s)", path, REF_FP))
  })
}

shinyApp(ui, server)


