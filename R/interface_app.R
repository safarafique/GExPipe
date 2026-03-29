## Full analysis UI (15 tabs). Built only when user enters analysis mode so cold start
## and shinytest2 do not source every tab file on first HTTP response.
gexp_app_analysis_dashboard_ui <- function() {
  # Ensure full namespace attach before any tab UI sources (race: user clicks Go before onFlushed).
  if (!isTRUE(getOption("gexpipe.attach.done", FALSE))) {
    options(gexpipe.attach.allow_full_now = TRUE)
    gexp_app_attach_packages()
  }
  shinydashboard::dashboardPage(
    skin = "purple",
    shinydashboard::dashboardHeader(
      title = shiny::tags$span(
        shiny::icon("dna", class = "fa-spin", style = "margin-right: 10px;"),
        shiny::tags$strong("GExPipe", style = "font-size: 24px; color: #fff;")
      ),
      titleWidth = 300,
      shiny::tags$li(
        class = "dropdown",
        shiny::tags$a(
          href = "#",
          shiny::icon("flask"),
          "GExPipe",
          style = "color: #fff; font-weight: bold; padding: 15px;"
        )
      ),
      shiny::tags$li(
        class = "dropdown",
        shiny::tags$div(
          id = "net_status_badge",
          class = "net-status online",
          shiny::icon("wifi"),
          shiny::tags$span(" Online")
        )
      ),
      shiny::tags$li(
        class = "dropdown",
        shiny::actionButton(
          "start_tour",
          shiny::tagList(shiny::icon("book-open"), " User Guideline"),
          class = "btn-info",
          style =
            "margin: 10px; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); 
             border: none; color: white; font-weight: bold; padding: 8px 15px; 
             border-radius: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.3);"
        )
      )
    ),
    shinydashboard::dashboardSidebar(
      width = 280,
      shinydashboard::sidebarMenu(
        id = "sidebar_menu",
        shinydashboard::menuItem("1. Download Data", tabName = "download", icon = shiny::icon("download", class = "fa-lg"), badgeLabel = "Start", badgeColor = "green"),
        shinydashboard::menuItem("2. QC & Visualization", tabName = "qc", icon = shiny::icon("chart-bar", class = "fa-lg"), badgeLabel = "View", badgeColor = "blue"),
        shinydashboard::menuItem("3. Normalize Data", tabName = "normalize", icon = shiny::icon("balance-scale", class = "fa-lg"), badgeLabel = "Process", badgeColor = "purple"),
        shinydashboard::menuItem("4. Select Groups", tabName = "groups", icon = shiny::icon("users", class = "fa-lg"), badgeLabel = "Categorize", badgeColor = "orange"),
        shinydashboard::menuItem("5. Batch Correction", tabName = "batch", icon = shiny::icon("filter", class = "fa-lg"), badgeLabel = "Correct", badgeColor = "red"),
        shinydashboard::menuItem("6. Differential Expression Analysis", tabName = "results", icon = shiny::icon("dna", class = "fa-lg"), badgeLabel = "DE Analysis", badgeColor = "yellow"),
        shinydashboard::menuItem("7. WGCNA Analysis", tabName = "wgcna", icon = shiny::icon("project-diagram", class = "fa-lg"), badgeLabel = "Network", badgeColor = "purple"),
        shinydashboard::menuItem("8. Common Genes (DEG & WGCNA)", tabName = "common_genes", icon = shiny::icon("venus-double", class = "fa-lg"), badgeLabel = "GO/KEGG", badgeColor = "green"),
        shinydashboard::menuItem("9. PPI Interaction", tabName = "ppi", icon = shiny::icon("project-diagram", class = "fa-lg"), badgeLabel = "Network", badgeColor = "teal"),
        shinydashboard::menuItem("10. Machine Learning Process", tabName = "ml", icon = shiny::icon("brain", class = "fa-lg"), badgeLabel = "ML", badgeColor = "maroon"),
        shinydashboard::menuItem("11. Validation Setup", tabName = "validation", icon = shiny::icon("shield-alt", class = "fa-lg"), badgeLabel = "Validate", badgeColor = "olive"),
        shinydashboard::menuItem("12. ROC Curve Analysis", tabName = "roc", icon = shiny::icon("chart-line", class = "fa-lg"), badgeLabel = "AUC", badgeColor = "green"),
        shinydashboard::menuItem("13. Diagnostic Nomogram", tabName = "nomogram", icon = shiny::icon("calculator", class = "fa-lg"), badgeLabel = "Nomogram", badgeColor = "maroon"),
        shinydashboard::menuItem("14. GSEA Analysis", tabName = "gsea", icon = shiny::icon("project-diagram", class = "fa-lg"), badgeLabel = "GSEA", badgeColor = "teal"),
        shinydashboard::menuItem("15. Results Summary", tabName = "results_summary", icon = shiny::icon("file-alt", class = "fa-lg"), badgeLabel = "PDF", badgeColor = "red")
      ),
      shiny::tags$div(
        style = "padding: 15px; text-align: center; border-top: 1px solid #ddd; background: #f8f9fa;",
        shiny::tags$label("File name (optional)", style = "font-size: 11px; color: #555; display: block; text-align: left; margin-bottom: 4px;"),
        shiny::textInput("workspace_save_filename", NULL, placeholder = "e.g. my_analysis", value = "my_analysis", width = "100%"),
        shiny::tags$div(
          shiny::actionButton("save_workspace_to_folder", shiny::tagList(shiny::icon("save"), " Save to folder"), class = "btn-success btn-block", style = "font-size: 13px; margin-top: 8px; margin-bottom: 6px;"),
          shiny::downloadButton("download_workspace", shiny::tagList(shiny::icon("download"), " Save workspace (download)"), class = "btn-info btn-block", style = "font-size: 12px; margin-bottom: 10px;")
        ),
        shiny::tags$p(shiny::tags$strong("Save to folder:"), " Saves to 'saved_workspaces' in the app directory. Use this if the download button does nothing.", style = "font-size: 11px; color: #555; margin: 4px 0 2px 0;"),
        shiny::tags$p(shiny::tags$strong("Save (download):"), " Same save + browser may download a copy.", style = "font-size: 11px; color: #555; margin: 0 0 4px 0;"),
        shiny::tags$p(shiny::tags$strong("Load:"), " Step 1 (Download) -> choose file from 'saved_workspaces' or Downloads -> Load.", style = "font-size: 11px; color: #555; margin: 0;")
      ),
      shiny::tags$div(
        style = "padding: 20px; text-align: center; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; margin-top: auto;",
        shiny::tags$h4(shiny::icon("info-circle"), " GExPipe", style = "color: white; font-weight: bold;"),
        shiny::tags$p("Gene Expression Pipeline", style = "font-size: 12px; margin-top: 10px; opacity: 0.9;")
      )
    ),
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      shiny::tags$div(
        class = "gexpipe-watermark",
        style = "position: fixed; bottom: 12px; right: 18px; font-size: 11px; font-weight: 700; letter-spacing: 1.2px;
                 color: rgba(102, 126, 234, 0.18); pointer-events: none; z-index: 0;",
        "GExPipe"
      ),
      if (requireNamespace("cicerone", quietly = TRUE)) cicerone::use_cicerone(),
      shiny::tags$div(id = "download-toast", shiny::icon("download"), " Downloading..."),
      shiny::tags$script(shiny::HTML("
        (function() {
          var toast = document.getElementById('download-toast');
          if (toast) {
            document.addEventListener('click', function(e) {
              var t = e.target;
              while (t && t !== document) {
                if (t.classList && t.classList.contains('shiny-download-link')) {
                  toast.classList.add('show');
                  setTimeout(function() { toast.classList.remove('show'); }, 2500);
                  break;
                }
                t = t.parentElement;
              }
            });
          }
        })();
      ")),
      # Styles/scripts + online/offline badge wiring are in the existing UI modules.
      shiny::tags$head(
        shiny::tags$script(shiny::HTML(
          if (isTRUE(getOption("shiny.testmode"))) {
            # No setInterval: periodic setInputValue prevents shinytest2 "stable" detection.
            "
          (function() {
            function setStatus(isOnline) {
              var el = document.getElementById('net_status_badge');
              if (!el) return;
              el.classList.remove('online','offline');
              el.classList.add(isOnline ? 'online' : 'offline');
              el.innerHTML = (isOnline ? '<i class=\"fa fa-wifi\"></i><span> Online</span>' : '<i class=\"fa fa-exclamation-triangle\"></i><span> Offline</span>');
              if (window.Shiny) Shiny.setInputValue('online_status', !!isOnline, {priority: 'event'});
            }
            setStatus(navigator.onLine);
            window.addEventListener('online', function() { setStatus(true); });
            window.addEventListener('offline', function() { setStatus(false); });
          })();
        "
          } else {
            "
          (function() {
            function setStatus(isOnline) {
              var el = document.getElementById('net_status_badge');
              if (!el) return;
              el.classList.remove('online','offline');
              el.classList.add(isOnline ? 'online' : 'offline');
              el.innerHTML = (isOnline ? '<i class=\"fa fa-wifi\"></i><span> Online</span>' : '<i class=\"fa fa-exclamation-triangle\"></i><span> Offline</span>');
              if (window.Shiny) Shiny.setInputValue('online_status', !!isOnline, {priority: 'event'});
            }
            setStatus(navigator.onLine);
            window.addEventListener('online', function() { setStatus(true); });
            window.addEventListener('offline', function() { setStatus(false); });
            setInterval(function() { setStatus(navigator.onLine); }, 5000);
          })();
        "
          }
        ))
      ),
      shiny::uiOutput("pipeline_progress"),
      shiny::tags$script(shiny::HTML("
        $(document).on('click', '.pipeline-step[data-tab]', function() {
          var tab = $(this).data('tab');
          if (tab) {
            var link = $('a[data-value=\"' + tab + '\"]');
            if (link.length) link.click();
          }
        });
      ")),
      shinydashboard::tabItems(
        gexp_ui_download(),
        gexp_ui_qc(),
        gexp_ui_normalize(),
        gexp_ui_groups(),
        gexp_ui_batch(),
        gexp_ui_results(),
        gexp_ui_wgcna(),
        gexp_ui_common_genes(),
        gexp_ui_ppi(),
        gexp_ui_ml(),
        gexp_ui_validation(),
        gexp_ui_roc(),
        gexp_ui_nomogram(),
        gexp_ui_gsea(),
        gexp_ui_results_summary()
      )
    )
  )
}

gexp_app_ui <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the GExPipe app.")
  }

  gexp_app_attach_packages()

  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::conditionalPanel(condition = "!output.show_analysis", gexp_ui_welcome()),
    shiny::uiOutput("analysis_dashboard")
  )
}

