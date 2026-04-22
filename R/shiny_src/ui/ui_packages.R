# ==============================================================================
# UI_PACKAGES.R  – Package Status Dashboard
# Shows every GExPipe dependency with a live green / red signal so users can
# spot missing packages and understand which analysis steps they power.
# ==============================================================================

ui_packages <- shinydashboard::tabItem(
  tabName = "packages",

  # ── Shared CSS for this tab ────────────────────────────────────────────────
  shiny::tags$head(shiny::tags$style(shiny::HTML("
    /* Summary stat cards */
    .pkg-stat-card {
      border-radius: 14px;
      padding: 18px 22px;
      text-align: center;
      color: #fff;
      box-shadow: 0 4px 14px rgba(0,0,0,0.18);
      transition: transform .2s ease, box-shadow .2s ease;
    }
    .pkg-stat-card:hover { transform: translateY(-3px); box-shadow: 0 8px 22px rgba(0,0,0,0.22); }
    .pkg-stat-card .stat-num  { font-size: 32px; font-weight: 800; line-height: 1; }
    .pkg-stat-card .stat-lbl  { font-size: 12px; font-weight: 600; opacity: .9; margin-top: 4px; letter-spacing: .4px; }
    .pkg-stat-card.total   { background: linear-gradient(135deg,#667eea,#764ba2); }
    .pkg-stat-card.ok      { background: linear-gradient(135deg,#2ecc71,#27ae60); }
    .pkg-stat-card.miss-req{ background: linear-gradient(135deg,#e74c3c,#c0392b); }
    .pkg-stat-card.miss-opt{ background: linear-gradient(135deg,#f39c12,#d68910); }

    /* Status pill */
    .pkg-ok   { display:inline-block; background:#e8f8f0; color:#1e8449;
                border:1.5px solid #82e0aa; border-radius:20px; padding:3px 12px;
                font-size:12px; font-weight:700; white-space:nowrap; }
    .pkg-miss { display:inline-block; background:#fdecea; color:#922b21;
                border:1.5px solid #f1948a; border-radius:20px; padding:3px 12px;
                font-size:12px; font-weight:700; white-space:nowrap; }
    .pkg-opt  { display:inline-block; background:#fef9e7; color:#9a7d0a;
                border:1.5px solid #f9e79f; border-radius:20px; padding:3px 12px;
                font-size:12px; font-weight:700; white-space:nowrap; }

    /* Category badge */
    .cat-req  { display:inline-block; background:#e8d5ff; color:#6c3483;
                border-radius:10px; padding:2px 9px; font-size:11px; font-weight:700; }
    .cat-core { display:inline-block; background:#d6eaf8; color:#1a5276;
                border-radius:10px; padding:2px 9px; font-size:11px; font-weight:700; }
    .cat-opt  { display:inline-block; background:#fdebd0; color:#784212;
                border-radius:10px; padding:2px 9px; font-size:11px; font-weight:700; }

    /* Version chip */
    .pkg-ver { font-family: monospace; font-size: 12px; color: #555; }

    /* Table row tint on hover */
    #pkg_status_table_wrapper table.dataTable tbody tr:hover td {
      background: #f0f4ff !important;
    }
    #pkg_status_table_wrapper table.dataTable thead th {
      background: linear-gradient(135deg,#667eea,#764ba2) !important;
      color: #fff !important;
      font-weight: 700 !important;
      font-size: 13px !important;
      border: none !important;
    }
    #pkg_status_table_wrapper table.dataTable {
      border-radius: 8px;
      overflow: hidden;
      border: 1px solid #e0e0e0 !important;
    }

    /* Install-missing button glow */
    #pkg_install_btn {
      background: linear-gradient(135deg,#e74c3c,#c0392b) !important;
      animation: pkgInstPulse 2.5s ease-in-out infinite;
    }
    @keyframes pkgInstPulse {
      0%,100% { box-shadow:0 0 0 0 rgba(231,76,60,.4); }
      50%      { box-shadow:0 0 0 8px rgba(231,76,60,0); }
    }
    #pkg_install_btn.no-missing { animation: none; background: linear-gradient(135deg,#95a5a6,#7f8c8d) !important; }
  "))),

  # ── Page title ─────────────────────────────────────────────────────────────
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::tags$div(
        style = "margin-bottom: 18px;",
        shiny::tags$h2(
          shiny::icon("box-open", style = "color:#764ba2; margin-right:10px;"),
          "Package Status",
          style = "color:#2c3e50; font-weight:800; margin:0 0 6px 0; border-bottom:3px solid #764ba2; display:inline-block; padding-bottom:6px;"
        ),
        shiny::tags$p(
          "All GExPipe dependencies are listed below. ",
          shiny::tags$strong("Green ✓"), " = installed and ready. ",
          shiny::tags$strong("Red ✗"), " = missing (required — must install before that analysis step will work). ",
          shiny::tags$strong("Orange ⚠"), " = missing optional (app runs without it; unlocks extra features).",
          style = "color:#555; font-size:14px; margin:0;"
        )
      )
    )
  ),

  # ── Summary stat cards ─────────────────────────────────────────────────────
  shiny::fluidRow(
    shiny::column(3, shiny::uiOutput("pkg_stat_total")),
    shiny::column(3, shiny::uiOutput("pkg_stat_ok")),
    shiny::column(3, shiny::uiOutput("pkg_stat_miss_req")),
    shiny::column(3, shiny::uiOutput("pkg_stat_miss_opt"))
  ),

  # ── Action buttons ─────────────────────────────────────────────────────────
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shinydashboard::box(
        width = NULL, status = "primary",
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::actionButton(
              "pkg_recheck_btn",
              shiny::tagList(shiny::icon("sync-alt"), " Re-check All Packages"),
              class = "btn-info btn-lg",
              style = "border-radius:25px; font-weight:700; padding:12px 28px; margin-right:12px;"
            ),
            shiny::actionButton(
              "pkg_install_btn",
              shiny::tagList(shiny::icon("download"), " Install / Fix Missing"),
              class = "btn-danger btn-lg",
              id = "pkg_install_btn",
              style = "border-radius:25px; font-weight:700; padding:12px 28px;"
            )
          ),
          shiny::column(
            width = 6,
            shiny::tags$div(
              id = "pkg_action_log",
              style = "padding:10px 14px; background:#f8f9fa; border-radius:8px; border:1px solid #dee2e6;
                       font-size:13px; color:#2c3e50; min-height:44px; line-height:1.5;",
              shiny::uiOutput("pkg_action_status")
            )
          )
        )
      )
    )
  ),

  # ── Main status table ──────────────────────────────────────────────────────
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shinydashboard::box(
        width = NULL, status = "primary",
        title = shiny::tagList(
          shiny::icon("table"),
          " All Dependencies — live status"
        ),
        solidHeader = TRUE,
        DT::dataTableOutput("pkg_status_table")
      )
    )
  ),

  # ── Install-command helper (shown only when missing packages exist) ─────────
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::uiOutput("pkg_install_hint_box")
    )
  )
)
