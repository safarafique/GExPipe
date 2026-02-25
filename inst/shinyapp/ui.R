# ==============================================================================
# UI.R - Main UI File (Modular Structure)
# ==============================================================================
# 
# This file combines all UI modules:
#   - ui_download.R: Step 1 - Download Data tab
#   - ui_qc.R: Step 2 - QC & Visualization tab
#   - ui_normalize.R: Step 3 - Normalize Data tab
#   - ui_groups.R: Step 4 - Select Groups tab
#   - ui_batch.R: Step 5 - Batch Correction tab
#   - ui_results.R: Step 6 - Differential Gene Expression Analysis tab
# ==============================================================================

# Source all UI modules (local = FALSE so variables are available in this scope)
source("ui/ui_welcome.R", local = FALSE)
source("ui/ui_download.R", local = FALSE)
source("ui/ui_qc.R", local = FALSE)
source("ui/ui_normalize.R", local = FALSE)
source("ui/ui_groups.R", local = FALSE)
source("ui/ui_batch.R", local = FALSE)
source("ui/ui_results.R", local = FALSE)
source("ui/ui_wgcna.R", local = FALSE)
source("ui/ui_common_genes.R", local = FALSE)
source("ui/ui_ppi.R", local = FALSE)
source("ui/ui_ml.R", local = FALSE)
source("ui/ui_validation.R", local = FALSE)
source("ui/ui_roc.R", local = FALSE)
source("ui/ui_nomogram.R", local = FALSE)
source("ui/ui_gsea.R", local = FALSE)
source("ui/ui_immune.R", local = FALSE)
source("ui/ui_results_summary.R", local = FALSE)

# Main analysis dashboard (shown after user clicks "Go to Analysis" on welcome page)
ui_analysis <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = tags$span(
      icon("dna", class = "fa-spin", style = "margin-right: 10px;"),
      tags$strong("OmniVerse", style = "font-size: 24px; color: #fff;")
    ),
    titleWidth = 300,
    tags$li(class = "dropdown",
            tags$a(href = "#", icon("flask"), "OmniVerse",
                   style = "color: #fff; font-weight: bold; padding: 15px;")),
    tags$li(
      class = "dropdown",
      tags$div(
        id = "net_status_badge",
        class = "net-status online",
        icon("wifi"),
        tags$span(" Online")
      )
    ),
    tags$li(class = "dropdown",
            actionButton("reset_app", 
                        tagList(icon("redo"), " Reset App"),
                        class = "btn-danger",
                        style = "margin: 10px; background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); 
                                 border: none; color: white; font-weight: bold; padding: 8px 15px; 
                                 border-radius: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.3);")),
    tags$li(class = "dropdown",
            actionButton("start_tour", 
                        tagList(icon("question-circle"), " Guided Tour"),
                        class = "btn-info",
                        style = "margin: 10px; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); 
                                 border: none; color: white; font-weight: bold; padding: 8px 15px; 
                                 border-radius: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.3);"))
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("1. Download Data", tabName = "download", 
               icon = icon("download", class = "fa-lg"), 
               badgeLabel = "Start", badgeColor = "green"),
      menuItem("2. QC & Visualization", tabName = "qc", 
               icon = icon("chart-bar", class = "fa-lg"),
               badgeLabel = "View", badgeColor = "blue"),
      menuItem("3. Normalize Data", tabName = "normalize", 
               icon = icon("balance-scale", class = "fa-lg"),
               badgeLabel = "Process", badgeColor = "purple"),
      menuItem("4. Select Groups", tabName = "groups", 
               icon = icon("users", class = "fa-lg"),
               badgeLabel = "Categorize", badgeColor = "orange"),
      menuItem("5. Batch Correction", tabName = "batch", 
               icon = icon("filter", class = "fa-lg"),
               badgeLabel = "Correct", badgeColor = "red"),
      menuItem("6. Differential Expression Analysis", tabName = "results", 
               icon = icon("dna", class = "fa-lg"),
               badgeLabel = "DE Analysis", badgeColor = "yellow"),
      menuItem("7. WGCNA Analysis", tabName = "wgcna", 
               icon = icon("project-diagram", class = "fa-lg"),
               badgeLabel = "Network", badgeColor = "purple"),
      menuItem("8. Common Genes (DEG & WGCNA)", tabName = "common_genes", 
               icon = icon("venus-double", class = "fa-lg"),
               badgeLabel = "GO/KEGG", badgeColor = "green"),
      menuItem("9. PPI Interaction", tabName = "ppi", 
               icon = icon("project-diagram", class = "fa-lg"),
               badgeLabel = "Network", badgeColor = "teal"),
      menuItem("10. Machine Learning Process", tabName = "ml", 
               icon = icon("brain", class = "fa-lg"),
               badgeLabel = "ML", badgeColor = "maroon"),
      menuItem("11. Validation Setup", tabName = "validation", 
               icon = icon("shield-alt", class = "fa-lg"),
               badgeLabel = "Validate", badgeColor = "olive"),
      menuItem("12. ROC Curve Analysis", tabName = "roc", 
               icon = icon("chart-line", class = "fa-lg"),
               badgeLabel = "AUC", badgeColor = "green"),
      menuItem("13. Diagnostic Nomogram", tabName = "nomogram", 
               icon = icon("calculator", class = "fa-lg"),
               badgeLabel = "Nomogram", badgeColor = "maroon"),
      menuItem("14. GSEA Analysis", tabName = "gsea", 
               icon = icon("project-diagram", class = "fa-lg"),
               badgeLabel = "GSEA", badgeColor = "teal"),
      menuItem("15. Immune Cell Deconvolution", tabName = "immune", 
               icon = icon("shield-alt", class = "fa-lg"),
               badgeLabel = "Immune", badgeColor = "purple"),
      menuItem("16. Results Summary", tabName = "results_summary", 
               icon = icon("file-alt", class = "fa-lg"),
               badgeLabel = "PDF", badgeColor = "red")
    ),
    tags$div(
      style = "padding: 15px; text-align: center; border-top: 1px solid #ddd; background: #f8f9fa;",
      tags$label("File name (optional)", style = "font-size: 11px; color: #555; display: block; text-align: left; margin-bottom: 4px;"),
      textInput("workspace_save_filename", NULL, placeholder = "e.g. my_analysis", value = "my_analysis", width = "100%"),
      tags$div(
        actionButton("save_workspace_to_folder", tagList(icon("save"), " Save to folder"),
                     class = "btn-success btn-block", style = "font-size: 13px; margin-top: 8px; margin-bottom: 6px;"),
        downloadButton("download_workspace", tagList(icon("download"), " Save workspace (download)"),
                      class = "btn-info btn-block", style = "font-size: 12px; margin-bottom: 10px;")
      ),
      tags$p(tags$strong("Save to folder:"), " Saves to 'saved_workspaces' in the app directory. Use this if the download button does nothing.", style = "font-size: 11px; color: #555; margin: 4px 0 2px 0;"),
      tags$p(tags$strong("Save (download):"), " Same save + browser may download a copy.", style = "font-size: 11px; color: #555; margin: 0 0 4px 0;"),
      tags$p(tags$strong("Load:"), " Step 1 (Download) → choose file from 'saved_workspaces' or Downloads → Load.", style = "font-size: 11px; color: #555; margin: 0;")
    ),
    tags$div(
      style = "padding: 20px; text-align: center; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; margin-top: auto;",
      tags$h4(icon("info-circle"), " Analysis Pipeline", style = "color: white; font-weight: bold;"),
      tags$p("Professional RNA-seq & Microarray Analysis", 
             style = "font-size: 12px; margin-top: 10px; opacity: 0.9;")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    if (requireNamespace("cicerone", quietly = TRUE)) cicerone::use_cicerone(),
    tags$div(id = "download-toast", icon("download"), " Downloading..."),
    tags$script(HTML("
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
    tags$head(
      # Bootstrap tooltip initializer (activates all data-toggle="tooltip" elements)
      tags$script(HTML("
        $(document).ready(function() {
          // Initialize on page load
          $('[data-toggle=\"tooltip\"]').tooltip({ html: true, container: 'body' });
          // Re-initialize when Shiny re-renders dynamic UI (e.g. after tab switch)
          $(document).on('shiny:value', function() {
            setTimeout(function() {
              $('[data-toggle=\"tooltip\"]').tooltip({ html: true, container: 'body' });
            }, 300);
          });
        });
      ")),
      tags$style(HTML("
        /* ===== HELP ICON / TOOLTIP STYLING ===== */
        .param-help {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 20px;
          height: 20px;
          border-radius: 50%;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: #fff;
          font-size: 11px;
          cursor: pointer;
          margin-left: 6px;
          vertical-align: middle;
          box-shadow: 0 2px 6px rgba(102, 126, 234, 0.3);
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .param-help:hover {
          transform: scale(1.15);
          box-shadow: 0 3px 10px rgba(102, 126, 234, 0.45);
        }
        .tooltip-inner {
          max-width: 320px;
          font-size: 12.5px;
          line-height: 1.5;
          text-align: left;
          padding: 10px 14px;
          background: #2c3e50;
          border-radius: 8px;
        }
        .tooltip.bs-tooltip-right .arrow::before,
        .tooltip.bs-tooltip-auto[x-placement^='right'] .arrow::before {
          border-right-color: #2c3e50;
        }
        .tooltip.bs-tooltip-top .arrow::before,
        .tooltip.bs-tooltip-auto[x-placement^='top'] .arrow::before {
          border-top-color: #2c3e50;
        }
        
        /* ===== DISABLED BUTTON STYLING (Step Guards) ===== */
        .btn[disabled], .btn.disabled {
          opacity: 0.45 !important;
          cursor: not-allowed !important;
          pointer-events: auto !important;
          filter: grayscale(40%);
        }
        
        /* ===== GLOBAL THEME COLORS ===== */
        :root {
          --primary-blue: #3498db;
          --primary-purple: #9b59b6;
          --primary-green: #2ecc71;
          --primary-orange: #f39c12;
          --primary-red: #e74c3c;
          --primary-yellow: #f1c40f;
          --dark-blue: #2980b9;
          --dark-purple: #8e44ad;
          --light-blue: #85c1e9;
          --light-purple: #bb8fce;
        }
        
        /* ===== LOADING INDICATORS ===== */
        .processing-overlay {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(0, 0, 0, 0.5);
          z-index: 9999;
          display: flex;
          justify-content: center;
          align-items: center;
          flex-direction: column;
        }
        
        .processing-spinner {
          border: 8px solid #f3f3f3;
          border-top: 8px solid #3498db;
          border-radius: 50%;
          width: 60px;
          height: 60px;
          animation: spin 1s linear infinite;
          margin-bottom: 20px;
        }
        
        .processing-message {
          color: white;
          font-size: 18px;
          font-weight: bold;
          text-align: center;
          background: rgba(52, 152, 219, 0.9);
          padding: 15px 30px;
          border-radius: 10px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.3);
        }
        
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        
        .btn-processing {
          position: relative;
          pointer-events: none;
          opacity: 0.7;
        }
        
        .btn-processing::after {
          content: '';
          position: absolute;
          width: 16px;
          height: 16px;
          top: 50%;
          left: 50%;
          margin-left: -8px;
          margin-top: -8px;
          border: 2px solid #ffffff;
          border-top-color: transparent;
          border-radius: 50%;
          animation: spin 0.8s linear infinite;
        }
        
        .processing-badge {
          display: inline-block;
          padding: 5px 12px;
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
          color: white;
          border-radius: 20px;
          font-size: 12px;
          font-weight: bold;
          margin-left: 10px;
          animation: pulse 2s ease-in-out infinite;
        }
        
        @keyframes pulse {
          0%, 100% { opacity: 1; transform: scale(1); }
          50% { opacity: 0.7; transform: scale(1.05); }
        }
        
        .status-indicator {
          display: inline-block;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          margin-right: 8px;
          animation: blink 1.5s ease-in-out infinite;
        }
        
        .status-indicator.processing {
          background-color: #f39c12;
          box-shadow: 0 0 10px #f39c12;
        }
        
        .status-indicator.complete {
          background-color: #2ecc71;
          animation: none;
        }
        
        @keyframes blink {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.3; }
        }
        
        /* ===== HEADER STYLING ===== */
        .main-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        }
        
        .main-header .logo {
          background: transparent !important;
          font-weight: bold;
        }
        
        /* ===== SIDEBAR STYLING ===== */
        .main-sidebar {
          background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%) !important;
        }
        
        .sidebar-menu > li > a {
          border-left: 4px solid transparent;
          transition: all 0.3s ease;
        }
        
        .sidebar-menu > li > a:hover {
          background: linear-gradient(90deg, rgba(102, 126, 234, 0.3) 0%, rgba(118, 75, 162, 0.3) 100%) !important;
          border-left-color: #667eea;
          color: #fff !important;
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%) !important;
          border-left-color: #f1c40f;
          color: #fff !important;
          font-weight: bold;
        }
        
        /* ===== BOX STYLING ===== */
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          border: none;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        
        .box:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        }
        
        .box-header {
          border-radius: 10px 10px 0 0;
          padding: 15px;
          font-weight: bold;
          font-size: 16px;
        }
        
        .box.box-primary .box-header {
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%) !important;
          color: white;
        }
        
        .box.box-success .box-header {
          background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%) !important;
          color: white;
        }
        
        .box.box-warning .box-header {
          background: linear-gradient(135deg, #f39c12 0%, #e67e22 100%) !important;
          color: white;
        }
        
        .box.box-info .box-header {
          background: linear-gradient(135deg, #17a2b8 0%, #138496 100%) !important;
          color: white;
        }
        
        .box.box-danger .box-header {
          background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%) !important;
          color: white;
        }
        
        /* ===== BUTTON STYLING ===== */
        .btn {
          border-radius: 25px;
          font-weight: bold;
          padding: 10px 25px;
          transition: all 0.3s ease;
          border: none;
          box-shadow: 0 4px 10px rgba(0,0,0,0.2);
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 15px rgba(0,0,0,0.3);
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%) !important;
          color: white;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #2980b9 0%, #1f6391 100%) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%) !important;
          color: white;
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #27ae60 0%, #229954 100%) !important;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #f39c12 0%, #e67e22 100%) !important;
          color: white;
        }
        
        .btn-info {
          background: linear-gradient(135deg, #17a2b8 0%, #138496 100%) !important;
          color: white;
        }
        
        .btn-danger {
          background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%) !important;
          color: white;
        }
        
        .btn-lg {
          padding: 15px 40px;
          font-size: 18px;
        }
        
        /* ===== INFO BOX STYLING ===== */
        .info-box {
          border-radius: 10px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          transition: transform 0.2s ease;
        }
        
        .info-box:hover {
          transform: scale(1.05);
        }
        
        .info-box-icon {
          border-radius: 10px 0 0 10px;
        }
        
        /* ===== TAB STYLING ===== */
        .nav-tabs-custom .nav-tabs li.active a {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          font-weight: bold;
        }
        
        .nav-tabs > li > a {
          border-radius: 5px 5px 0 0;
        }
        
        /* ===== PLOT AREAS ===== */
        .box .shiny-plot-output, .box img[src*='png'], .box img[src*='jpg'] {
          border-radius: 8px;
          background: #fafafa;
          max-width: 100%;
        }
        .content-wrapper { padding: 20px; }
        
        /* ===== NEXT BUTTON STYLING ===== */
        .next-btn {
          text-align: right;
          margin-top: 20px;
        }
        
        .next-btn .btn {
          background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%);
          animation: pulse 2s infinite;
        }
        
        @keyframes pulse {
          0% { box-shadow: 0 0 0 0 rgba(46, 204, 113, 0.7); }
          70% { box-shadow: 0 0 0 10px rgba(46, 204, 113, 0); }
          100% { box-shadow: 0 0 0 0 rgba(46, 204, 113, 0); }
        }
        
        /* ===== DISEASE-SPECIFIC GUIDE BOX ===== */
        .disease-guide-box {
          animation: fadeIn 0.5s ease;
        }
        .disease-guide-box h4 { border: none; }
        
        /* ===== PAGE HEADERS ===== */
        h2 {
          color: #2c3e50;
          font-weight: bold;
          margin-bottom: 25px;
          padding-bottom: 10px;
          border-bottom: 3px solid #667eea;
        }
        
        /* ===== INPUT STYLING ===== */
        .form-control {
          border-radius: 5px;
          border: 2px solid #ddd;
          transition: border-color 0.3s ease;
        }
        
        .form-control:focus {
          border-color: #667eea;
          box-shadow: 0 0 10px rgba(102, 126, 234, 0.3);
        }
        
        /* ===== TABLE STYLING ===== */
        .table {
          border-radius: 5px;
          overflow: hidden;
        }
        
        .table thead {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
        }
        
        .table tbody tr:hover {
          background-color: #f8f9fa;
        }
        
        /* ===== BADGE STYLING ===== */
        .badge {
          border-radius: 12px;
          padding: 5px 10px;
          font-weight: bold;
        }
        
        /* ===== PROGRESS BAR ===== */
        .progress {
          border-radius: 10px;
          height: 25px;
        }
        
        .progress-bar {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
        }
        
        /* ===== CUSTOM SCROLLBAR ===== */
        ::-webkit-scrollbar {
          width: 10px;
        }
        
        ::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 5px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(180deg, #667eea 0%, #764ba2 100%);
          border-radius: 5px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(180deg, #764ba2 0%, #667eea 100%);
        }
        
        /* ===== ANIMATIONS ===== */
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .box {
          animation: fadeIn 0.5s ease;
        }
        
        /* ===== RESET BUTTON STYLING ===== */
        #reset_app {
          transition: all 0.3s ease;
        }
        
        #reset_app:hover {
          transform: scale(1.05);
          box-shadow: 0 6px 20px rgba(231, 76, 60, 0.4) !important;
        }
        
        /* ===== MODAL DIALOG STYLING ===== */
        .modal-content {
          border-radius: 15px;
          border: none;
          box-shadow: 0 10px 40px rgba(0,0,0,0.3);
        }
        
        .modal-header {
          background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
          color: white;
          border-radius: 15px 15px 0 0;
          border: none;
        }
        
        .modal-header .modal-title {
          font-weight: bold;
          font-size: 20px;
        }
        
        .modal-body {
          padding: 25px;
          font-size: 15px;
        }
        
        .modal-footer {
          border-top: 2px solid #ecf0f1;
          padding: 15px 25px;
        }
        
        .modal-footer .btn {
          margin-left: 10px;
        }
        
        /* ===== NOTIFICATION STYLING ===== */
        .shiny-notification {
          border-radius: 10px;
          box-shadow: 0 6px 20px rgba(0,0,0,0.3);
          font-weight: bold;
        }

        /* ===== ONLINE/OFFLINE BADGE ===== */
        .net-status {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          margin: 10px 0;
          padding: 8px 14px;
          border-radius: 999px;
          font-weight: 800;
          font-size: 13px;
          letter-spacing: 0.2px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.25);
          user-select: none;
        }
        .net-status.online {
          background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%);
          color: #fff;
        }
        .net-status.offline {
          background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
          color: #fff;
        }

        /* ===== STEP TIMER ===== */
        .step-timer {
          margin-top: 10px;
          padding: 10px 12px;
          border-radius: 10px;
          background: linear-gradient(135deg, rgba(102,126,234,0.08) 0%, rgba(118,75,162,0.08) 100%);
          border: 1px solid rgba(102,126,234,0.18);
          color: #2c3e50;
          font-weight: 700;
          display: inline-block;
        }
        .step-timer .label {
          opacity: 0.8;
          font-weight: 800;
          margin-right: 8px;
        }
        
        /* ===== GROUP SELECTION STYLING ===== */
        .group-category-card {
          transition: all 0.3s ease;
          border-radius: 8px;
        }
        
        .group-category-card:hover {
          transform: translateX(5px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }
        
        .group-selector-box {
          margin-bottom: 15px;
        }
        
        .group-selector-box .box-header {
          padding: 12px 15px;
        }
        
        .group-selector-box .box-body {
          padding: 15px;
        }
        
        /* Category dropdown styling */
        select[id^='cat_'] {
          border-radius: 5px;
          border: 2px solid #ddd;
          padding: 8px;
          font-weight: 500;
          transition: border-color 0.3s ease;
        }
        
        select[id^='cat_']:focus {
          border-color: #667eea;
          box-shadow: 0 0 8px rgba(102, 126, 234, 0.3);
        }
        
        /* Sample count badge */
        .sample-count-badge {
          display: inline-flex;
          align-items: center;
          padding: 6px 12px;
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
          color: white;
          border-radius: 20px;
          font-weight: bold;
          box-shadow: 0 2px 8px rgba(52, 152, 219, 0.3);
        }
        
        /* Group name styling */
        .group-name {
          font-size: 16px;
          color: #2c3e50;
          font-weight: 600;
        }
        
        /* Category color indicators */
        .category-normal {
          border-left-color: #2ecc71 !important;
        }
        
        .category-disease {
          border-left-color: #e74c3c !important;
        }
        
        .category-none {
          border-left-color: #95a5a6 !important;
        }
        
        /* Select input styling in group selector */
        select[id^='group_col_'] {
          border-radius: 5px;
          border: 2px solid #ddd;
          padding: 10px;
          font-size: 14px;
          transition: all 0.3s ease;
        }
        
        select[id^='group_col_']:focus {
          border-color: #667eea;
          box-shadow: 0 0 10px rgba(102, 126, 234, 0.3);
        }
        
        /* ===== PROCESSING SUMMARY TOGGLE BUTTON ===== */
        #toggle_download_summary {
          transition: all 0.3s ease;
          border-radius: 20px;
        }
        
        #toggle_download_summary:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        
        /* Collapsible box styling */
        .box[data-collapsed='true'] .box-body {
          display: none;
        }
        
        /* Summary panel animation */
        #download_summary_panel,
        #normalization_summary_panel,
        #batch_summary_panel {
          transition: all 0.3s ease;
        }
        
        /* ===== SELECT INPUT STYLING (Prevent Flickering) ===== */
        select.form-control {
          border-radius: 5px !important;
          border: 2px solid #ddd !important;
          padding: 10px 12px !important;
          font-size: 14px !important;
          font-weight: 500 !important;
          transition: border-color 0.3s ease, box-shadow 0.3s ease !important;
          background-color: #fff !important;
          min-height: 42px !important;
          box-shadow: none !important;
        }
        
        select.form-control:focus {
          border-color: #667eea !important;
          box-shadow: 0 0 10px rgba(102, 126, 234, 0.3) !important;
          outline: none !important;
        }
        
        /* ===== RADIO BUTTONS STYLING ===== */
        .shiny-input-radiogroup {
          padding: 15px;
          background: #f8f9fa;
          border-radius: 8px;
          border: 1px solid #dee2e6;
        }
        
        .shiny-input-radiogroup label {
          display: block;
          padding: 12px 15px;
          margin: 8px 0;
          background: white;
          border: 2px solid #e9ecef;
          border-radius: 6px;
          cursor: pointer;
          transition: all 0.3s ease;
          font-weight: 500;
          font-size: 14px;
        }
        
        .shiny-input-radiogroup label:hover {
          background: #f0f4ff;
          border-color: #667eea;
          transform: translateX(5px);
          box-shadow: 0 2px 8px rgba(102, 126, 234, 0.2);
        }
        
        .shiny-input-radiogroup input[type='radio']:checked + span {
          color: #667eea;
          font-weight: bold;
        }
        
        .shiny-input-radiogroup input[type='radio']:checked ~ label {
          background: linear-gradient(135deg, #e8f0ff 0%, #f0f4ff 100%);
          border-color: #667eea;
          box-shadow: 0 2px 8px rgba(102, 126, 234, 0.3);
        }
        
        .shiny-input-radiogroup input[type='radio'] {
          margin-right: 10px;
          width: 18px;
          height: 18px;
          cursor: pointer;
        }
        
        .shiny-input-radiogroup input[type='radio']:checked {
          accent-color: #667eea;
        }
        
        /* ===== DOWNLOAD INDICATOR TOAST ===== */
        #download-toast {
          position: fixed;
          bottom: 24px;
          right: 24px;
          padding: 14px 22px;
          background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
          color: white;
          border-radius: 12px;
          box-shadow: 0 6px 20px rgba(0,0,0,0.25);
          font-weight: bold;
          font-size: 14px;
          z-index: 9999;
          display: none;
          align-items: center;
          gap: 10px;
          animation: fadeIn 0.25s ease;
        }
        #download-toast.show {
          display: flex;
        }
        #download-toast i {
          font-size: 18px;
        }
        
        /* ===== PIPELINE PROGRESS TRACKER ===== */
        .pipeline-tracker {
          padding: 14px 20px 10px 20px;
          margin-bottom: 18px;
          background: #fff;
          border-radius: 12px;
          box-shadow: 0 2px 12px rgba(0,0,0,0.07);
          border: 1px solid #e9ecef;
        }
        .pipeline-tracker-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 10px;
        }
        .pipeline-tracker-title {
          font-weight: 800;
          font-size: 14px;
          color: #2c3e50;
          letter-spacing: 0.3px;
        }
        .pipeline-tracker-pct {
          font-weight: 700;
          font-size: 13px;
          color: #667eea;
        }
        .pipeline-tracker .progress {
          height: 10px;
          border-radius: 8px;
          background: #e9ecef;
          overflow: hidden;
          margin-bottom: 14px;
        }
        .pipeline-tracker .progress-bar {
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
          border-radius: 8px;
          transition: width 0.6s ease;
        }
        .pipeline-steps-row {
          display: flex;
          flex-wrap: wrap;
          gap: 6px;
          justify-content: center;
        }
        .pipeline-step {
          display: inline-flex;
          align-items: center;
          gap: 5px;
          padding: 5px 12px;
          border-radius: 20px;
          font-size: 11.5px;
          font-weight: 600;
          cursor: pointer;
          transition: all 0.25s ease;
          user-select: none;
          border: 1.5px solid transparent;
        }
        .pipeline-step:hover {
          transform: translateY(-2px);
          box-shadow: 0 3px 10px rgba(0,0,0,0.12);
        }
        .pipeline-step.done {
          background: linear-gradient(135deg, #d5f5e3 0%, #abebc6 100%);
          color: #1e8449;
          border-color: #82e0aa;
        }
        .pipeline-step.running {
          background: linear-gradient(135deg, #fdebd0 0%, #fad7a0 100%);
          color: #b7950b;
          border-color: #f9e79f;
          animation: stepPulse 2s ease-in-out infinite;
        }
        .pipeline-step.pending {
          background: #f4f6f7;
          color: #95a5a6;
          border-color: #d5dbdb;
        }
        .pipeline-step .step-icon {
          font-size: 12px;
        }
        .pipeline-step.done .step-icon { color: #27ae60; }
        .pipeline-step.running .step-icon { color: #f39c12; }
        .pipeline-step.pending .step-icon { color: #bdc3c7; }
        @keyframes stepPulse {
          0%, 100% { box-shadow: 0 0 0 0 rgba(243, 156, 18, 0.3); }
          50% { box-shadow: 0 0 0 6px rgba(243, 156, 18, 0); }
        }
        @media (max-width: 768px) {
          .pipeline-step { font-size: 10px; padding: 4px 8px; }
          .pipeline-tracker { padding: 10px 12px 8px 12px; }
        }
        
        /* ===== RESPONSIVE DESIGN ===== */
        @media (max-width: 768px) {
          .box {
            margin-bottom: 15px;
          }
          
          #reset_app {
            font-size: 12px;
            padding: 6px 12px;
          }
          
          .group-category-card {
            margin-bottom: 10px;
          }
        }
      ")),
      tags$script(HTML("
        (function() {
          function setStatus(isOnline) {
            var el = document.getElementById('net_status_badge');
            if (!el) return;
            el.classList.remove('online','offline');
            el.classList.add(isOnline ? 'online' : 'offline');
            el.innerHTML = (isOnline ? '<i class=\"fa fa-wifi\"></i><span> Online</span>' : '<i class=\"fa fa-exclamation-triangle\"></i><span> Offline</span>');
            if (window.Shiny) Shiny.setInputValue('online_status', !!isOnline, {priority: 'event'});
          }

          // initial
          setStatus(navigator.onLine);

          window.addEventListener('online', function() { setStatus(true); });
          window.addEventListener('offline', function() { setStatus(false); });

          // Lightweight periodic check (helps with flaky wifi that doesn't trigger events)
          setInterval(function() {
            setStatus(navigator.onLine);
          }, 5000);
        })();
      "))
    ),
    
    # ===== PIPELINE PROGRESS TRACKER =====
    uiOutput("pipeline_progress"),
    
    # Click-to-navigate: clicking a step chip jumps to that sidebar tab
    tags$script(HTML("
      $(document).on('click', '.pipeline-step[data-tab]', function() {
        var tab = $(this).data('tab');
        if (tab) {
          // Programmatically click the matching sidebar menu item
          var link = $('a[data-value=\"' + tab + '\"]');
          if (link.length) link.click();
        }
      });
    ")),
    
    tabItems(
      ui_download,
      ui_qc,
      ui_normalize,
      ui_groups,
      ui_batch,
      ui_results,
      ui_wgcna,
      ui_common_genes,
      ui_ppi,
      ui_ml,
      ui_validation,
      ui_roc,
      ui_nomogram,
      ui_gsea,
      ui_immune,
      ui_results_summary
    )
  )
)

# Root UI: show welcome landing page first, then analysis dashboard after "Go to Analysis"
ui <- fluidPage(
  useShinyjs(),
  conditionalPanel(
    condition = "!output.show_analysis",
    ui_welcome
  ),
  conditionalPanel(
    condition = "output.show_analysis",
    ui_analysis
  )
)
