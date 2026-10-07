## =====================================================================
## Condition shortfall & discounting: explorer + hitter  (v02)
## ---------------------------------------------------------------------
## Two tabs, one shared pooled reference (ONS pooled life table + norm):
##
##   EXPLORE : reference QALE stack (survival x HRQoL x discount) with a
##             condition overlaid; shortfall split into mortality and
##             morbidity bands. Layout: OVERLAID, or SIDE-BY-SIDE showing
##             the SAME condition undiscounted (left) vs discounted (right),
##             with the reference discounted in the right panel too.
##             Condition either MANUAL (sliders) or LOADED from the folder.
##
##   AUTHOR  : the "hitter". Two knot editors (age-specific mortality OR;
##             age-specific HRQoL scale). Choose N interior knots -> markers
##             appear on the induced-curve preview; CLICK a marker to make it
##             active; the bound slider (log-OR / linear-scale) edits the
##             active knot's value; a numeric field sets its age. The age-0
##             anchor is pinned at no-hit; the max-age anchor INHERITS the
##             last interior knot (flat extrapolation of the final state).
##
## Stored condition contract (folder .rds):
##   data.frame(age, hrqol, mu)   + attrs menu_label, info, reference, hit_spec
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
  surv = pal[["SPBlue"]],  ref = pal[["SPGreen"]], cond = pal[["SPRed"]],
  disc = pal[["SPBlue"]],  onset = pal[["SPPurple"]], active = pal[["SPPurple"]])

TOP      <- 120
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
qxvec <- grid_vec(as.data.frame(tb_pool), "a", "mu")      # ref one-year death prob
qvec  <- grid_vec(as.data.frame(qn_pool), "age", "hrqol") # ref HRQoL
REF_FP <- paste0(format(sum(qxvec * seq_along(qxvec)), digits = 15), "_",
                 format(sum(qvec  * seq_along(qvec)),  digits = 15))
ref_label <- paste0("table: ", attr(tb_pool, "source") %||% "user",
                    "  |  norm: ", attr(qn_pool, "source") %||% "user")

## --- knot -> full age-vector interpolation ---------------------------
interp_knots <- function(knots, base_value, method = "linear") {
  k <- knots[order(knots$age), , drop = FALSE]
  k <- k[!duplicated(k$age), , drop = FALSE]
  if (nrow(k) == 0) return(rep(base_value, TOP + 1))
  approx(k$age, k$value, xout = 0:TOP, method = method, rule = 2, f = 0)$y
}

or_hit <- function(q, OR) OR * q / (1 - q + OR * q)        # [0,1] -> [0,1]

## v04 edit: helper function for inverting hits to reference
or_on_q <- function(q, OR) (OR * q) / (1 - q + OR * q)

## --- the stack from OR / HRQoL-scale age-vectors ---------------------
build_stack <- function(a, or_v, m_v, r, top_plot = 105) {
  K   <- max(1, min(top_plot, TOP) - a)
  j   <- 0:K; age <- a + j
  qref <- qvec[age + 1]; qx <- qxvec[age + 1]
  qxc  <- or_hit(qx, or_v[age + 1]); qc <- m_v[age + 1] * qref
  surv <- function(qv) c(1, cumprod(1 - qv[-length(qv)]))
  Sref <- surv(qx); Sc <- surv(qxc); disc <- (1 + r)^(-j)
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
nice_w <- function(AS, PS) { w <- 1; if (AS >= 12 || PS >= 0.85) w <- 1.2
                             if (AS >= 18 || PS >= 0.95) w <- 1.7; w }

## --- draw one stack. disc_view=TRUE plots every curve on the ---------
##     discounted footing (reference included).
draw_stack <- function(x, bands = TRUE, main = "", disc_view = FALSE) {
  j <- x$j; K <- x$K
  Sref <- if (disc_view) x$Sref_d else x$Sref
  Csq  <- if (disc_view) x$Csq_d  else x$Csq
  Scm  <- if (disc_view) x$Scm_d  else x$Scm
  Scc  <- if (disc_view) x$Scd    else x$Scc
  par(mar = c(3.4, 1, if (nzchar(main)) 1.8 else 0.5, 1))
  plot(NULL, xlim = c(0, K), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE, main = main)
  poly <- function(y, col) polygon(c(0, j, K), c(0, y, 0), border = NA, col = col)
  if (bands) {
    poly(Sref, adjustcolor(BAND$norm, 0.30))
    poly(Csq,  adjustcolor(BAND$mort, 0.60))     # [Scm,Csq] mortality shortfall
    poly(Scm,  adjustcolor(BAND$morb, 0.60))     # [Scc,Scm] morbidity shortfall
    if (disc_view) {
      poly(Scc, adjustcolor(BAND$qale, 0.55))    # arm already discounted = delivered
    } else {
      poly(Scc,   adjustcolor(BAND$disc, 0.75))  # [Scd,Scc] discounting loss
      poly(x$Scd, adjustcolor(BAND$qale, 0.55))  # delivered discounted QALE
    }
    lines(j, Csq, col = STROKE$ref,  lwd = 2)
    lines(j, Scc, col = STROKE$cond, lwd = 2)
    if (!disc_view) lines(j, x$Scd, col = STROKE$disc, lwd = 1.3, lty = 2)
  } else {
    poly(Sref, adjustcolor(BAND$norm, 0.30))
    poly(Csq,  adjustcolor(BAND$qale, 0.45))
    lines(j, Csq, col = STROKE$ref, lwd = 2)
  }
  lines(j, Sref, col = STROKE$surv, lwd = 1.5)
  at <- seq(0, K, by = 10); axis(1, at = at, labels = x$a + at)
  mtext("age  (vantage a + t)", side = 1, line = 2.3, col = "#666", cex = 0.9)
}

## --- shared comparison table (both tabs) -----------------------------
compare_table <- function(x, header) {
  cell   <- function(v) tags$td(style = "text-align:right;padding:5px 16px;font-variant-numeric:tabular-nums;", v)
  hd     <- function(v) tags$th(style = "text-align:right;padding:5px 16px;font-weight:600;", v)
  rowlab <- function(v) tags$td(style = "text-align:left;padding:5px 16px;color:#555;", v)
  w0 <- nice_w(x$AS, x$PS); wd <- nice_w(x$AS_d, x$PS_d)
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
              rowlab("NICE weight"), cell(sprintf("%.1f", w0)), cell(sprintf("%.1f", wd)))))
}

## --- harvest conditions folder ---------------------------------------
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
    paste0(
      ".irs-bar,.irs-single{background:%1$s!important;border-color:%1$s!important}",
      ".knot-row  { display:flex; align-items:center; gap:12px; margin-top:6px; min-height:52px; }",
      ".knot-age  { flex:0 0 90px; }",
      ".knot-read { font-size:12px; color:#444; line-height:1.4; }",
      ".knot-read b { color:%1$s; }"
    ),
    pal[["SPBlue"]])))),
  titlePanel("Condition shortfall & discounting"),
  tabsetPanel(id = "tab",
              
              
    ## ---------------- EXPLORE ---------------------------------------
    tabPanel("Explore",
      br(),
      fluidRow(
        column(3, selectInput("cond", "condition", choices = c("manual (sliders)" = ""))),
        column(3, radioButtons("view", "layout", inline = TRUE,
                 c("overlaid" = "ov", "undisc | disc" = "sbs"), selected = "ov")),
        column(3,
               sliderInput("r", "discount rate \u03c1", min = 0, max = 0.10,
                           value = 0.035, step = 0.001, width = "100%"),
               numericInput("r_max", "\u03c1 slider max", value = 0.10,
                            min = 0.01, max = 1, step = 0.01)),
        column(3, sliderInput("age", "vantage age a", min = 0, max = 90, value = 35, step = 1))
      ),
      conditionalPanel("input.cond == ''",
        fluidRow(
          column(4, sliderInput("onset", "onset a'", min = 0, max = 100, value = 45, step = 1)),
          column(4, sliderInput("m", "HRQoL scale from onset (1=none)", min = 0.1, max = 1, value = 0.6, step = 0.05)),
          column(4, sliderInput("or_log", "mortality OR from onset (log; 1=none)", min = 0, max = 4, value = log10(3), step = 0.01))
        ),
        div(style = "color:#666; font-size:12px; margin-top:-6px;", textOutput("or_readout"))),
      plotOutput("explore_plot", height = "330px"),
      uiOutput("legend"),
      uiOutput("compare"),
      uiOutput("cond_info"),
      div(style = "color:#888; font-size:11px;", textOutput("ref_lab"))
    ),

    ## ---------------- EDITOR (conditions) --------------------------------
    tabPanel("Author",
      br(),
      fluidRow(
        column(5, selectInput("edit_load", "load condition into editor",
                              choices = c("(none)" = ""))),
        column(2, actionButton("edit_load_go", "Load", class = "btn-primary",
                               style = "margin-top:25px;")),
        column(5, div(style = "margin-top:28px;font-size:12px;",
                      uiOutput("edit_load_msg")))
      ),
      fluidRow(
        column(6,
          h5("Mortality OR profile"),
          helpText("Age 0 pinned at OR 1; age", TOP, "inherits the last knot. ",
                   "Click a marker to activate it, then use the slider."),
          fluidRow(
            column(4, numericInput("or_nk", "interior knots", value = 1, min = 0, max = 8, step = 1)),
            column(4, selectInput("or_rule", "between knots",
                                  c("shock (step)" = "constant", "ramp (linear)" = "linear"),
                                  selected = "constant")),
            column(4, sliderInput("or_slider", "active knot OR (log)", min = 0, max = 4,
                                  value = log10(3), step = 0.01, width = "100%"))),
          uiOutput("or_age_ui"),
          plotOutput("or_preview", height = "170px", click = "or_click")),
        column(6,
          h5("HRQoL scale profile"),
          helpText("Age 0 pinned at scale 1; age", TOP, "inherits the last knot. ",
                   "Click a marker to activate, then use the slider (1 = no morbidity)."),
          fluidRow(
            column(4, numericInput("m_nk", "interior knots", value = 1, min = 0, max = 8, step = 1)),
            column(4, selectInput("m_rule", "between knots",
                                  c("shock (step)" = "constant", "ramp (linear)" = "linear"),
                                  selected = "linear")),
            column(4, sliderInput("m_slider", "active knot scale", min = 0.05, max = 1,
                                  value = 0.6, step = 0.01, width = "100%"))),
          uiOutput("m_age_ui"),
          plotOutput("m_preview", height = "170px", click = "m_click"))
      ),
      hr(),
      h5("Preview against current reference"),
      fluidRow(
        column(3, sliderInput("a_auth", "vantage age", min = 0, max = 90, value = 0, step = 1)),
        column(3, sliderInput("r_auth", "discount rate \u03c1", min = 0, max = 0.10,
                              value = 0.035, step = 0.001, width = "100%")),
        column(3, radioButtons("auth_view", "layout", inline = TRUE,
                               c("overlaid" = "ov", "undisc | disc" = "sbs"), selected = "ov"))
      ),
      plotOutput("auth_plot", height = "300px"),
      uiOutput("auth_compare"),
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

  output$ref_lab    <- renderText(ref_label)
  output$or_readout <- renderText(sprintf("OR = %.1f", 10 ^ input$or_log))

  ## server — rescale slider when the cap changes, clamp current value
  observeEvent(input$r_max, {
    cap <- input$r_max
    req(isTruthy(cap), cap > 0, cap <= 1)
    updateSliderInput(session, "r", max = cap,
                      step = signif(cap / 100, 1),
                      value = min(isolate(input$r), cap))
    updateSliderInput(session, "r_auth", max = cap,
                      step = signif(cap / 100, 1),
                      value = min(isolate(input$r_auth), cap))
  })
  
  
  ## ---- conditions dropdown ---------------------------------------
  updateSelectInput(session, "cond", choices = c("manual (sliders)" = "", harvest()))
  current_cond <- reactive({ if (identical(input$cond, "")) NULL else readRDS(input$cond) })

  ## ---- EXPLORE: OR / HRQoL vectors -------------------------------
  vecs <- reactive({
    cc <- current_cond()
    if (is.null(cc)) {
      onset <- input$onset; OR <- 10 ^ input$or_log; m <- input$m
      list(or_v = ifelse(0:TOP >= onset, OR, 1), m_v = ifelse(0:TOP >= onset, m, 1))
    } else {
      mu_c <- approx(cc$age, cc$mu,    xout = 0:TOP, rule = 2)$y
      q_c  <- approx(cc$age, cc$hrqol, xout = 0:TOP, rule = 2)$y
      or_v <- (mu_c * (1 - qxvec)) / (qxvec * (1 - mu_c)); or_v[!is.finite(or_v)] <- 1
      m_v  <- q_c / qvec; m_v[!is.finite(m_v)] <- 1
      list(or_v = pmax(or_v, 1e-6), m_v = m_v)
    }
  })
  ex <- reactive({
    req(is.finite(input$r), input$r >= 0, input$r <= 1)
    v <- vecs(); build_stack(input$age, v$or_v, v$m_v, input$r)
  })

  output$explore_plot <- renderPlot({
    v <- vecs()
    if (identical(input$view, "sbs")) {
      par(mfrow = c(1, 2))
      x0 <- build_stack(input$age, v$or_v, v$m_v, 0)
      xr <- build_stack(input$age, v$or_v, v$m_v, input$r)
      draw_stack(x0, bands = TRUE, main = "undiscounted (\u03c1 = 0)")
      draw_stack(xr, bands = TRUE, disc_view = TRUE,
                 main = sprintf("discounted (\u03c1 = %.1f%%)", 100 * input$r))
      par(mfrow = c(1, 1))
    } else {
      draw_stack(ex(), bands = TRUE)
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
      chip(BAND$qale, sprintf("delivered QALE (undisc) \u2248 %.1f", x$QALEc)))
  })

  output$compare <- renderUI(compare_table(
    ex(), if (identical(input$cond, ""))
            sprintf("onset %d, from age %d", input$onset, input$age)
          else sprintf("from age %d", input$age)))

  output$cond_info <- renderUI({
    cc <- current_cond(); info <- attr(cc, "info"); req(info)
    tagList(tags$p(style = "font-style:italic;", info$summary %||% ""),
      lapply(info$sources %||% list(), function(s)
        tags$div(tags$a(href = s$url, target = "_blank", s$cite %||% s$url))))
  })

  ## =================================================================
  ## EDITOR: knot state, click-to-activate, bound slider
  ## =================================================================
  default_ages <- function(n) if (n < 1) numeric(0) else
    round(seq(0, TOP, length.out = n + 2))[-c(1, n + 2)]

  KN <- reactiveValues(
    or = data.frame(age = default_ages(1), value = 3),
    m  = data.frame(age = default_ages(1), value = 0.6),
    or_active = 1L, m_active = 1L,
    loading = FALSE)

  sync_count <- function(which, n, vdef) {
    if (isTRUE(KN$loading)) return(invisible())   # suppressed during a load
    cur <- KN[[which]]
    if (nrow(cur) == n) return(invisible())
    ages <- default_ages(n); vals <- rep(vdef, n)
    if (nrow(cur) > 0 && n > 0) {
      k <- min(n, nrow(cur))
      vals[seq_len(k)] <- cur$value[seq_len(k)]; ages[seq_len(k)] <- cur$age[seq_len(k)]
    }
    KN[[which]] <- if (n > 0) data.frame(age = ages, value = vals) else cur[0, ]
    KN[[paste0(which, "_active")]] <- if (n > 0) min(KN[[paste0(which,"_active")]], n) else 1L
  }
  observeEvent(input$or_nk, sync_count("or", max(0, input$or_nk %||% 0), 3))
  observeEvent(input$m_nk,  sync_count("m",  max(0, input$m_nk  %||% 0), 0.6))
  ## here ####
  observe(updateSelectInput(session, "edit_load",
                            choices = c("(none)" = "", harvest())))
  strip_anchors <- function(full) {
    if (is.null(full) || nrow(full) == 0) return(data.frame(age = numeric(0), value = numeric(0)))
    int <- full[full$age > 0 & full$age < TOP, , drop = FALSE]
    data.frame(age = int$age, value = int$value)
  }
  
  full_knots <- function(which, end0) {                # age 0 pinned; TOP inherits last
    cur <- KN[[which]]
    if (nrow(cur) == 0) return(data.frame(age = c(0, TOP), value = c(end0, end0)))
    ord <- cur[order(cur$age), , drop = FALSE]
    endT <- ord$value[nrow(ord)]
    rbind(data.frame(age = 0, value = end0), ord, data.frame(age = TOP, value = endT))
  }
  or_knots <- reactive(full_knots("or", 1))
  m_knots  <- reactive(full_knots("m",  1))
  or_vec <- reactive(pmax(interp_knots(or_knots(), 1, input$or_rule %||% "constant"), 1e-6))
  m_vec  <- reactive(pmin(pmax(interp_knots(m_knots(), 1, input$m_rule %||% "linear"), 0), 1))

  activate <- function(which, click) {
    cur <- KN[[which]]; if (is.null(click) || nrow(cur) == 0) return()
    KN[[paste0(which, "_active")]] <- which.min(abs(cur$age - click$x))
  }
  observeEvent(input$or_click, activate("or", input$or_click))
  observeEvent(input$m_click,  activate("m",  input$m_click))

  observeEvent(KN$or_active, {
    cur <- KN$or; i <- KN$or_active
    if (nrow(cur) >= i) updateSliderInput(session, "or_slider", value = log10(max(cur$value[i], 1)))
  })
  observeEvent(KN$m_active, {
    cur <- KN$m; i <- KN$m_active
    if (nrow(cur) >= i) updateSliderInput(session, "m_slider", value = cur$value[i])
  })
  observeEvent(input$or_slider, {
    cur <- KN$or; i <- KN$or_active
    if (nrow(cur) >= i) { cur$value[i] <- 10 ^ input$or_slider; KN$or <- cur }
  })
  observeEvent(input$m_slider, {
    cur <- KN$m; i <- KN$m_active
    if (nrow(cur) >= i) { cur$value[i] <- input$m_slider; KN$m <- cur }
  })
  
  # v03
  # output$or_age_ui <- renderUI({
  #   cur <- KN$or; if (nrow(cur) == 0) return(NULL)
  #   numericInput("or_age", sprintf("active knot age (knot %d)", KN$or_active),
  #                value = cur$age[KN$or_active], min = 1, max = TOP - 1, step = 1)
  # })
  ## v04 edit 
  output$or_age_ui <- renderUI({
    cur <- KN$or; if (nrow(cur) == 0) return(NULL)
    i   <- KN$or_active
    age <- cur$age[i]
    OR  <- cur$value[i]
    qref <- qxvec[age + 1]                 # reference one-year death prob at age
    qsoc <- or_on_q(qref, OR)              # implied soc mortality
    div(class = "knot-row",
        div(class = "knot-age",
            numericInput("or_age", sprintf("knot %d age", i),
                         value = age, min = 1, max = TOP - 1, step = 1, width = "120px")),
        div(class = "knot-read",
            HTML(sprintf(
              "OR <b>%.2f</b> at age <b>%d</b><br>ref q = %.4f &rarr; soc q = <b>%.4f</b>",
              OR, age, qref, qsoc)))
    )
  })
  # v03:
  # output$m_age_ui <- renderUI({
  #   cur <- KN$m; if (nrow(cur) == 0) return(NULL)
  #   numericInput("m_age", sprintf("active knot age (knot %d)", KN$m_active),
  #                value = cur$age[KN$m_active], min = 1, max = TOP - 1, step = 1)
  # })
  # v04 edit:
  output$m_age_ui <- renderUI({
    cur <- KN$m; if (nrow(cur) == 0) return(NULL)
    i    <- KN$m_active
    age  <- cur$age[i]
    sc   <- cur$value[i]
    href <- qvec[age + 1]                  # reference HRQoL at age
    hsoc <- href * sc                      # implied soc HRQoL
    div(class = "knot-row",
        div(class = "knot-age",
            numericInput("m_age", sprintf("knot %d age", i),
                         value = age, min = 1, max = TOP - 1, step = 1, width = "120px")),
        div(class = "knot-read",
            HTML(sprintf(
              "scale <b>%.2f</b> at age <b>%d</b><br>ref hrqol = %.3f &rarr; soc hrqol = <b>%.3f</b>",
              sc, age, href, hsoc)))
    )
  })
  
  
  observeEvent(input$or_age, {
    a <- input$or_age; cur <- KN$or; i <- KN$or_active
    if (isTruthy(a) && nrow(cur) >= i) { cur$age[i] <- max(1, min(TOP - 1, a)); KN$or <- cur }
  })
  observeEvent(input$m_age, {
    a <- input$m_age; cur <- KN$m; i <- KN$m_active
    if (isTruthy(a) && nrow(cur) >= i) { cur$age[i] <- max(1, min(TOP - 1, a)); KN$m <- cur }
  })

  # v04 edit: redundant
  # output$or_active_lab <- renderText({
  #   cur <- KN$or; if (nrow(cur) == 0) "no interior knots" else
  #     sprintf("active: knot %d  (age %d, OR %.1f)", KN$or_active,
  #             cur$age[KN$or_active], cur$value[KN$or_active]) })
  # output$m_active_lab <- renderText({
  #   cur <- KN$m; if (nrow(cur) == 0) "no interior knots" else
  #     sprintf("active: knot %d  (age %d, scale %.2f)", KN$m_active,
  #             cur$age[KN$m_active], cur$value[KN$m_active]) })

  output$or_preview <- renderPlot({
    par(mar = c(3, 3.4, 0.5, 0.5))
    ov <- or_vec(); Sc <- c(1, cumprod(1 - or_hit(qxvec, ov)[-length(ov)]))
    plot(0:TOP, Sc, type = "l", col = STROKE$cond, lwd = 2, ylim = c(0, 1),
         xlab = "age", ylab = "induced survival S_c")
    lines(0:TOP, c(1, cumprod(1 - qxvec[-length(qxvec)])), col = STROKE$surv, lty = 2)
    cur <- KN$or
    if (nrow(cur) > 0) {
      yy <- Sc[pmin(cur$age, TOP) + 1]
      points(cur$age, yy, pch = 21, bg = "white", col = STROKE$onset, cex = 1.4, lwd = 2)
      i <- KN$or_active; points(cur$age[i], yy[i], pch = 19, col = STROKE$active, cex = 1.7)
    }
  })
  output$m_preview <- renderPlot({
    par(mar = c(3, 3.4, 0.5, 0.5))
    mv <- m_vec()
    plot(0:TOP, mv * qvec, type = "l", col = STROKE$cond, lwd = 2, ylim = c(0, 1),
         xlab = "age", ylab = "induced HRQoL q_c")
    lines(0:TOP, qvec, col = STROKE$ref, lty = 2)
    cur <- KN$m
    if (nrow(cur) > 0) {
      yy <- (mv * qvec)[pmin(cur$age, TOP) + 1]
      points(cur$age, yy, pch = 21, bg = "white", col = STROKE$onset, cex = 1.4, lwd = 2)
      i <- KN$m_active; points(cur$age[i], yy[i], pch = 19, col = STROKE$active, cex = 1.7)
    }
  })

  auth <- reactive({
    req(is.finite(input$r_auth), input$r_auth >= 0, input$r_auth <= 1)
    build_stack(input$a_auth, or_vec(), m_vec(), input$r_auth)
  })
  output$auth_plot <- renderPlot({
    if (identical(input$auth_view, "sbs")) {
      par(mfrow = c(1, 2))
      x0 <- build_stack(input$a_auth, or_vec(), m_vec(), 0)
      xr <- build_stack(input$a_auth, or_vec(), m_vec(), input$r_auth)
      draw_stack(x0, bands = TRUE, main = "undiscounted (\u03c1 = 0)")
      draw_stack(xr, bands = TRUE, disc_view = TRUE,
                 main = sprintf("discounted (\u03c1 = %.1f%%)", 100 * input$r_auth))
      par(mfrow = c(1, 1))
    } else {
      draw_stack(auth(), bands = TRUE)
    }
  })
  output$auth_compare <- renderUI(compare_table(auth(), sprintf("preview from age %d", input$a_auth)))

  ## ---- EXPORT ----------------------------------------------------
  ## the actual write, called from both paths
  do_export <- function() {
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
      norm  = attr(qn_pool, "source") %||% "user", fingerprint = REF_FP)
    attr(df, "hit_spec") <- list(or_knots = or_knots(), hrqol_knots = m_knots(),
                                 or_interp = input$or_rule, hrqol_interp = input$m_rule,
                                 rule = "rule2 (flat outside knots)")
    path <- export_path()
    saveRDS(df, path)
    updateSelectInput(session, "cond", choices = c("manual (sliders)" = "", harvest()))
    output$export_msg <- renderText(sprintf("wrote %s  (fingerprint %s)", path, REF_FP))
  }
  
  export_path <- reactive({
    fn <- input$cond_file
    if (!grepl("\\.rds$", fn)) fn <- paste0(fn, ".rds")
    file.path(COND_DIR, fn)
  })
  
  ## button: guard on collision
  ## path the export will write to (used by both guard and writer)
  export_path <- reactive({
    fn <- input$cond_file
    if (!grepl("\\.rds$", fn)) fn <- paste0(fn, ".rds")
    file.path(COND_DIR, fn)
  })
  
  observeEvent(input$edit_load_go, {
    f <- input$edit_load
    if (!nzchar(f)) { output$edit_load_msg <- renderUI(NULL); return() }
    
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(obj)) {
      output$edit_load_msg <- renderUI(span(style = "color:#b00;", "could not read file"))
      return()
    }
    
    hs  <- attr(obj, "hit_spec")
    ref <- attr(obj, "reference")
    
    ## --- reference check (apples vs pears) ---
    ref_note <- NULL
    if (!is.null(ref) && !is.null(ref$fingerprint)) {
      if (!identical(ref$fingerprint, REF_FP)) {
        ref_note <- span(style = "color:#b00;font-weight:bold;",
                         sprintf("reference MISMATCH: condition built against fp %s; app has %s. ",
                                 ref$fingerprint, REF_FP),
                         "Knots are restored, but soc values are re-measured against the current reference.")
      }
    }
    
    ## --- knotless condition: cannot knot-edit ---
    if (is.null(hs) || is.null(hs$or_knots) || is.null(hs$hrqol_knots)) {
      output$edit_load_msg <- renderUI(tagList(
        span(style = "color:#a60;",
             "loaded condition has no knot structure - it can be viewed in the Explorer, ",
             "but not knot-edited here. (Knot retrofit is not yet available.)"),
        if (!is.null(ref_note)) tagList(br(), ref_note)))
      return()
    }
    
    ## --- restore interior knots (strip the 0 / TOP anchors) ---
    or_int <- strip_anchors(hs$or_knots)
    m_int  <- strip_anchors(hs$hrqol_knots)
    
    ## set the interpolation-rule selectors first
    updateSelectInput(session, "or_rule", selected = hs$or_interp %||% "constant")
    updateSelectInput(session, "m_rule",  selected = hs$hrqol_interp %||% "linear")
    
    ## set counts and knots with sync_count suppressed via KN$loading, so the
    ## nk-observer reshape cannot clobber the loaded knots. Release the guard
    ## AFTER the current flush completes (i.e. after sync_count has been skipped).
    KN$loading <- TRUE
    updateNumericInput(session, "or_nk", value = nrow(or_int))
    updateNumericInput(session, "m_nk",  value = nrow(m_int))
    KN$or <- if (nrow(or_int) > 0) or_int else KN$or[0, ]
    KN$m  <- if (nrow(m_int)  > 0) m_int  else KN$m[0, ]
    KN$or_active <- 1L
    KN$m_active  <- 1L
    session$onFlushed(function() KN$loading <- FALSE, once = TRUE)
    
    ## carry the metadata into the editor fields so re-export round-trips
    updateTextInput(session, "cond_label",   value = attr(obj, "menu_label") %||% "")
    info <- attr(obj, "info")
    updateTextInput(session, "cond_summary", value = (info$summary %||% ""))
    srcs <- info$sources %||% list()
    src  <- if (length(srcs) >= 1) srcs[[1]] else list()
    updateTextInput(session, "cond_cite", value = (src$cite %||% ""))
    updateTextInput(session, "cond_url",  value = (src$url  %||% ""))
    ## default the export filename to the loaded file (so re-save overwrites
    ## the same file -> the overwrite guard fires, protecting against clobber)
    updateTextInput(session, "cond_file", value = basename(f))
    
    output$edit_load_msg <- renderUI(tagList(
      span(style = "color:#060;",
           sprintf("loaded '%s'  (%d OR knots, %d HRQoL knots)",
                   attr(obj, "menu_label") %||% basename(f),
                   nrow(or_int), nrow(m_int))),
      if (!is.null(ref_note)) tagList(br(), ref_note)))
  })
  ## menu_labels currently in use, excluding the file we're about to replace
  labels_in_use <- function(exclude = NULL) {
    files <- list.files(COND_DIR, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) == 0) return(character(0))
    if (!is.null(exclude)) {
      ex <- normalizePath(exclude, mustWork = FALSE)
      files <- files[normalizePath(files, mustWork = FALSE) != ex]
    }
    if (length(files) == 0) return(character(0))
    labs <- vapply(files, function(f) {
      lb <- tryCatch(attr(readRDS(f), "menu_label"), error = function(e) NA_character_)
      if (is.null(lb)) NA_character_ else as.character(lb)
    }, character(1))
    stats::setNames(labs[!is.na(labs)], basename(files)[!is.na(labs)])
  }
 
  observeEvent(input$export, {
    path <- export_path()
    lab  <- input$cond_label
    used <- labels_in_use(exclude = path)
    
    if (!nzchar(lab)) {
      showModal(modalDialog(title = "Missing label",
                            "A condition needs a menu label to appear in the Explore dropdown.",
                            footer = modalButton("OK"), easyClose = TRUE))
      return()
    }
    if (lab %in% used) {
      clash <- names(used)[match(lab, used)]
      showModal(modalDialog(title = "Label already in use",
                            tags$p(sprintf("The label \"%s\" is already used by %s.", lab, clash)),
                            tags$p("Two files with the same label collide in the dropdown: only one will be reachable. Choose a different label."),
                            footer = modalButton("OK"), easyClose = TRUE))
      return()
    }
    if (file.exists(path)) {
      existing <- tryCatch(attr(readRDS(path), "menu_label"), error = function(e) NA)
      showModal(modalDialog(title = "File already exists",
                            tags$p(sprintf("%s already exists%s.", basename(path),
                                           if (!is.na(existing)) sprintf(" (label: \"%s\")", existing) else "")),
                            tags$p("Overwrite it?"),
                            footer = tagList(modalButton("Cancel"),
                                             actionButton("confirm_overwrite", "Overwrite", class = "btn-danger")),
                            easyClose = TRUE))
    } else {
      do_export()
    }
  })
  
  ## confirm path
  observeEvent(input$confirm_overwrite, {
    removeModal()
    do_export()
  })
}

shinyApp(ui, server)
