# ==============================================================================
# UI_WELCOME.R - Landing / Welcome Page
# ==============================================================================
# Shown when the app first loads. User clicks "Go to Analysis" to proceed.
# Enhanced with 3D pipeline visualization, animations, and attention-grabbing design.
# ==============================================================================

ui_welcome <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;500;600;700;800&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      @keyframes float { 0%, 100% { transform: translateY(0); } 50% { transform: translateY(-10px); } }
      @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.85; } }
      @keyframes shine { 0% { background-position: -200% center; } 100% { background-position: 200% center; } }
      @keyframes fadeInUp { from { opacity: 0; transform: translateY(30px); } to { opacity: 1; transform: translateY(0); } }
      @keyframes ctaGlow { 0%, 100% { box-shadow: 0 0 30px rgba(167,139,250,0.5); } 50% { box-shadow: 0 0 50px rgba(167,139,250,0.8); } }
      @keyframes badgePop { 0% { transform: scale(0.9); opacity: 0; } 60% { transform: scale(1.05); } 100% { transform: scale(1); opacity: 1; } }
      .welcome-hero-logo { animation: float 4s ease-in-out infinite; }
      .welcome-card { backdrop-filter: blur(16px); transition: transform 0.4s ease, box-shadow 0.4s ease; }
      .welcome-card:hover { transform: translateY(-6px); box-shadow: 0 35px 90px rgba(0,0,0,0.3); }
      .pipeline-3d { perspective: 1000px; }
      .pipeline-3d .step { transform-style: preserve-3d; transform: rotateY(-8deg) rotateX(2deg);
                          transition: transform 0.4s ease; box-shadow: 0 8px 24px rgba(102,126,234,0.3); }
      .pipeline-3d .step:hover { transform: rotateY(-4deg) rotateX(4deg) scale(1.05);
                                box-shadow: 0 14px 36px rgba(102,126,234,0.45); }
      .go-btn { background: linear-gradient(135deg, #667eea 0%, #764ba2 50%, #8b5cf6 100%);
                background-size: 200% auto; animation: shine 4s linear infinite, ctaGlow 2s ease-in-out infinite; }
      .go-btn:hover { background-position: right center; transform: scale(1.05); }
      .hero-animate { animation: fadeInUp 0.8s ease forwards; opacity: 0; }
      .hero-animate:nth-child(1) { animation-delay: 0.1s; }
      .hero-animate:nth-child(2) { animation-delay: 0.25s; }
      .hero-animate:nth-child(3) { animation-delay: 0.4s; }
      .hero-animate:nth-child(4) { animation-delay: 0.55s; }
      .hero-animate:nth-child(5) { animation-delay: 0.7s; }
      .hero-animate:nth-child(6) { animation-delay: 0.85s; }
      .hero-animate:nth-child(7) { animation-delay: 1s; }
      .badge-pop { animation: badgePop 0.6s ease forwards; opacity: 0; }
      .badge-pop:nth-child(1) { animation-delay: 1.1s; }
      .badge-pop:nth-child(2) { animation-delay: 1.2s; }
      .badge-pop:nth-child(3) { animation-delay: 1.3s; }
      .badge-pop:nth-child(4) { animation-delay: 1.4s; }
      .badge-pop:hover { transform: translateY(-3px); box-shadow: 0 8px 25px rgba(0,0,0,0.3); }
    "))
  ),
  style = "min-height: 100vh; background: linear-gradient(135deg, #0f0c29 0%, #302b63 20%, #24243e 40%, #0f3460 60%, #533483 80%, #1a1a2e 100%); 
            background-size: 400% 400%; animation: gradient 14s ease infinite;
            display: flex; flex-direction: column; align-items: center; justify-content: center; 
            padding: 50px 20px; font-family: 'Outfit', 'Segoe UI', sans-serif;
            position: relative; overflow-x: hidden;",
  tags$style(HTML("
    @keyframes gradient { 0%{background-position:0% 50%} 50%{background-position:100% 50%} 100%{background-position:0% 50%} }
    .bg-glow { position: absolute; width: 600px; height: 600px; border-radius: 50%; background: radial-gradient(circle, rgba(139,92,246,0.15) 0%, transparent 70%); 
               top: -200px; right: -200px; pointer-events: none; }
    .bg-glow-2 { position: absolute; width: 400px; height: 400px; border-radius: 50%; background: radial-gradient(circle, rgba(102,126,234,0.12) 0%, transparent 70%); 
                 bottom: -100px; left: -100px; pointer-events: none; }
  ")),
  tags$div(class = "bg-glow"), tags$div(class = "bg-glow-2"),
  tags$div(
    style = "max-width: 960px; width: 100%; position: relative; z-index: 1;",
    # Hero: Logo + Stats Badges + 3D Pipeline
    tags$div(
      style = "text-align: center; margin-bottom: 40px;",
      tags$h1(
        class = "hero-animate welcome-hero-logo",
        style = "color: #fff; font-size: 58px; font-weight: 800; margin-bottom: 12px; text-shadow: 0 4px 30px rgba(0,0,0,0.4);
                  letter-spacing: -0.5px;",
        icon("dna", style = "margin-right: 16px; color: #a78bfa; filter: drop-shadow(0 0 12px rgba(167,139,250,0.8));"),
        "OmniVerse"
      ),
      tags$p(
        class = "hero-animate",
        style = "color: rgba(255,255,255,0.95); font-size: 20px; font-weight: 600; letter-spacing: 0.3px; margin-bottom: 8px;",
        "Your One-Stop RNA-seq & Microarray Analysis Pipeline"
      ),
      tags$p(
        class = "hero-animate",
        style = "color: rgba(255,255,255,0.75); font-size: 15px; font-weight: 500; letter-spacing: 0.5px; margin-bottom: 24px;",
        "From GEO download to publication-ready reports — all in one place"
      ),
      # Stats badges
      tags$div(
        class = "hero-animate",
        style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 12px; margin-bottom: 32px;",
        lapply(list(
          list("16", "Analysis Steps", "#8b5cf6"),
          list("GEO", "Integrated", "#06b6d4"),
          list("RNA-seq", "+ Microarray", "#10b981"),
          list("DESeq2", "limma WGCNA", "#f59e0b")
        ), function(x) {
          tags$div(
            class = "badge-pop",
            style = "background: rgba(255,255,255,0.12); backdrop-filter: blur(10px); border: 1px solid rgba(255,255,255,0.2);
                     padding: 12px 20px; border-radius: 14px; color: #fff; font-weight: 700; font-size: 13px;
                     box-shadow: 0 4px 15px rgba(0,0,0,0.2); transition: transform 0.3s, box-shadow 0.3s;",
            tags$span(style = sprintf("color: %s; font-size: 18px; display: block; margin-bottom: 2px;", x[[3]]), x[[1]]),
            tags$span(style = "opacity: 0.9; font-size: 11px; font-weight: 500;", x[[2]])
          )
        })
      ),
      # 3D Pipeline Flow - Analysis Steps
      tags$div(
        class = "hero-animate pipeline-3d",
        style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 12px; margin-top: 20px; padding: 24px;",
        lapply(list(
          list("1", "Download", "#667eea"),
          list("2", "QC", "#764ba2"),
          list("3-5", "Preprocess", "#9b59b6"),
          list("6", "DE", "#e74c3c"),
          list("7-8", "WGCNA", "#3498db"),
          list("9-10", "PPI & ML", "#2ecc71"),
          list("11-16", "Report", "#f39c12")
        ), function(x) {
          tags$div(
            class = "step",
            style = sprintf(
              "background: linear-gradient(145deg, %s, %s); color: white; padding: 12px 18px; 
               border-radius: 12px; font-weight: bold; font-size: 13px; min-width: 90px;
               border: 1px solid rgba(255,255,255,0.2);",
              x[[3]], paste0(substr(x[[3]], 1, 7), "cc")
            ),
            tags$span(style = "opacity: 0.9; font-size: 11px;", x[[1]]),
            tags$br(),
            tags$span(x[[2]])
          )
        })
      )
    ),
    # Main Card
    tags$div(
      class = "welcome-card",
      style = "background: rgba(255,255,255,0.98); border-radius: 28px; padding: 48px 50px; 
                box-shadow: 0 30px 80px rgba(0,0,0,0.25), 0 0 0 1px rgba(255,255,255,0.15);
                margin-bottom: 30px; animation: fadeInUp 0.9s ease 1.2s forwards; opacity: 0;",
      tags$p(
        style = "color: #333; font-size: 16px; line-height: 1.9; margin-bottom: 30px; text-align: left; font-weight: 500;",
        "OmniVerse guides you through a complete analysis workflow: from GEO download and QC, 
         through normalization and differential expression, to WGCNA, pathway enrichment, PPI networks, 
         machine learning, and publication-ready reports."
      ),
      tags$h3(
        style = "color: #2c3e50; font-size: 20px; margin: 28px 0 14px 0; text-align: left; 
                 border-bottom: 3px solid #9b59b6; padding-bottom: 10px; display: inline-block;",
        icon("sitemap", style = "margin-right: 8px; color: #9b59b6;"),
        "Pipeline Overview"
      ),
      tags$div(
        style = "text-align: left; margin: 0 0 25px 0; font-size: 14px; line-height: 1.9; color: #444;",
        tags$p(tags$strong("1. Download"), " — GEO data (RNA-seq / Microarray / Merged) with gene symbol mapping"),
        tags$p(tags$strong("2. QC & Visualization"), " — Gene overlap, PCA, sample connectivity"),
        tags$p(tags$strong("3–5. Normalize, Groups, Batch"), " — Preprocessing and batch correction"),
        tags$p(tags$strong("6. Differential Expression"), " — limma / DESeq2 / edgeR"),
        tags$p(tags$strong("7–8. WGCNA & Common Genes"), " — Co-expression modules and enrichment"),
        tags$p(tags$strong("9–10. PPI & ML"), " — Protein interaction networks and predictive modeling"),
        tags$p(tags$strong("11–16. Validation, ROC, Nomogram, GSEA, Immune, Summary"), " — Validation and report")
      ),
      tags$h3(
        style = "color: #2c3e50; font-size: 20px; margin: 28px 0 14px 0; text-align: left; 
                 border-bottom: 3px solid #9b59b6; padding-bottom: 10px; display: inline-block;",
        icon("star", style = "margin-right: 8px; color: #f1c40f;"),
        "Why OmniVerse?"
      ),
      tags$ul(
        style = "text-align: left; padding-left: 24px; margin: 0 0 35px 0; color: #444; font-size: 14px; line-height: 2;",
        tags$li("Supports both RNA-seq and microarray in one workflow"),
        tags$li("Multi-dataset merge with common gene mapping across platforms"),
        tags$li("Step-by-step pipeline with guided options and validation"),
        tags$li("Publication-ready figures and downloadable results"),
        tags$li("Optional workspace save/load to resume analyses")
      ),
      tags$div(
        style = "text-align: center; margin-top: 20px;",
        actionButton(
          "go_to_analysis",
          tagList(icon("rocket", style = "margin-right: 10px;"), " Start Analyzing"),
          class = "btn-lg go-btn",
          style = "color: white; border: none; font-weight: 800; padding: 20px 55px; font-size: 19px;
                   border-radius: 40px; letter-spacing: 0.5px;
                   transition: transform 0.3s, box-shadow 0.3s, background-position 0.5s;"
        )
      )
    )
  ),
  tags$script(HTML("
    $(document).on('click', '#go_to_analysis', function() {
      $(this).css({ transform: 'scale(0.96)', boxShadow: '0 4px 16px rgba(102,126,234,0.4)' });
      setTimeout(function() { $('#go_to_analysis').css({ transform: '', boxShadow: '' }); }, 150);
    });
  "))
)
