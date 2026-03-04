# ==============================================================================
# UI_WELCOME.R - Landing / Welcome Page (GExPipe - Gene Expression Pipeline)
# ==============================================================================
# Shown when the app first loads. User clicks "Start Analyzing" to proceed.
# ==============================================================================

ui_welcome <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;500;600;700;800&family=Space+Grotesk:wght@500;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      @keyframes float { 0%, 100% { transform: translateY(0); } 50% { transform: translateY(-12px); } }
      @keyframes shine { 0% { background-position: -200% center; } 100% { background-position: 200% center; } }
      @keyframes fadeInUp { from { opacity: 0; transform: translateY(35px); } to { opacity: 1; transform: translateY(0); } }
      @keyframes ctaGlow { 0%, 100% { box-shadow: 0 0 35px rgba(167,139,250,0.5), 0 8px 32px rgba(102,126,234,0.35); } 50% { box-shadow: 0 0 55px rgba(167,139,250,0.7), 0 12px 40px rgba(102,126,234,0.45); } }
      @keyframes badgePop { 0% { transform: scale(0.88); opacity: 0; } 65% { transform: scale(1.08); } 100% { transform: scale(1); opacity: 1; } }
      @keyframes gradientShift { 0%, 100% { background-position: 0% 50%; } 50% { background-position: 100% 50%; } }
      .hero-title-gradient { background: linear-gradient(120deg, #e0e7ff 0%, #c7d2fe 25%, #a78bfa 50%, #c084fc 75%, #e0e7ff 100%); background-size: 200% auto; -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text; }
      .welcome-hero-logo { animation: float 4.5s ease-in-out infinite; }
      .welcome-card { backdrop-filter: blur(20px); transition: transform 0.4s cubic-bezier(0.34, 1.56, 0.64, 1), box-shadow 0.4s ease; }
      .welcome-card:hover { transform: translateY(-8px); box-shadow: 0 40px 100px rgba(0,0,0,0.22), 0 0 0 1px rgba(255,255,255,0.08); }
      .pipeline-3d { perspective: 1000px; }
      .pipeline-3d .step { transform-style: preserve-3d; transform: rotateY(-8deg) rotateX(2deg); transition: transform 0.35s ease, box-shadow 0.35s ease; box-shadow: 0 10px 28px rgba(102,126,234,0.35); }
      .pipeline-3d .step:hover { transform: rotateY(-2deg) rotateX(4deg) scale(1.06); box-shadow: 0 18px 42px rgba(102,126,234,0.5); }
      .go-btn { background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 40%, #a855f7 70%, #6366f1 100%); background-size: 200% auto;
                animation: shine 5s linear infinite, ctaGlow 2.2s ease-in-out infinite; }
      .go-btn:hover { background-position: right center; transform: scale(1.06); box-shadow: 0 0 60px rgba(139,92,246,0.6); }
      .hero-animate { animation: fadeInUp 0.9s ease forwards; opacity: 0; }
      .hero-animate:nth-child(1) { animation-delay: 0.08s; }
      .hero-animate:nth-child(2) { animation-delay: 0.2s; }
      .hero-animate:nth-child(3) { animation-delay: 0.35s; }
      .hero-animate:nth-child(4) { animation-delay: 0.5s; }
      .hero-animate:nth-child(5) { animation-delay: 0.65s; }
      .hero-animate:nth-child(6) { animation-delay: 0.8s; }
      .hero-animate:nth-child(7) { animation-delay: 0.95s; }
      .badge-pop { animation: badgePop 0.65s cubic-bezier(0.34, 1.56, 0.64, 1) forwards; opacity: 0; }
      .badge-pop:nth-child(1) { animation-delay: 1.05s; }
      .badge-pop:nth-child(2) { animation-delay: 1.18s; }
      .badge-pop:nth-child(3) { animation-delay: 1.31s; }
      .badge-pop:nth-child(4) { animation-delay: 1.44s; }
      .badge-pop:hover { transform: translateY(-4px); box-shadow: 0 12px 28px rgba(0,0,0,0.25); }
      .pipeline-item { padding: 6px 0; border-left: 3px solid transparent; padding-left: 14px; margin: 4px 0; transition: all 0.25s ease; }
      .pipeline-item:hover { border-left-color: #8b5cf6; padding-left: 18px; }
      .watermark-welcome { position: fixed; bottom: 24px; right: 28px; font-size: 13px; font-weight: 700; letter-spacing: 1.5px; color: rgba(255,255,255,0.12); pointer-events: none; z-index: 0; font-family: 'Outfit', sans-serif; }
    "))
  ),
  style = "min-height: 100vh; background: linear-gradient(135deg, #0e0a1f 0%, #1e1b4b 18%, #312e81 35%, #1e3a5f 52%, #4c1d95 70%, #0f172a 100%); 
            background-size: 400% 400%; animation: gradientShift 18s ease infinite;
            display: flex; flex-direction: column; align-items: center; justify-content: center; 
            padding: 56px 24px; font-family: 'Outfit', 'Space Grotesk', 'Segoe UI', sans-serif;
            position: relative; overflow-x: hidden;",
  tags$style(HTML("
    @keyframes gradientShift { 0%{background-position:0% 50%} 50%{background-position:100% 50%} 100%{background-position:0% 50%} }
    .bg-glow { position: absolute; width: 680px; height: 680px; border-radius: 50%; background: radial-gradient(circle, rgba(139,92,246,0.18) 0%, transparent 65%); top: -240px; right: -220px; pointer-events: none; }
    .bg-glow-2 { position: absolute; width: 480px; height: 480px; border-radius: 50%; background: radial-gradient(circle, rgba(99,102,241,0.14) 0%, transparent 65%); bottom: -120px; left: -120px; pointer-events: none; }
    .bg-mesh { position: absolute; inset: 0; background-image: radial-gradient(rgba(255,255,255,0.03) 1px, transparent 1px); background-size: 32px 32px; pointer-events: none; }
  ")),
  tags$div(class = "bg-glow"), tags$div(class = "bg-glow-2"), tags$div(class = "bg-mesh"),
  tags$div(class = "watermark-welcome", "GExPipe · Gene Expression Pipeline"),
  tags$div(
    style = "max-width: 980px; width: 100%; position: relative; z-index: 1;",
    # Hero: Logo + Stats Badges + 3D Pipeline
    tags$div(
      style = "text-align: center; margin-bottom: 48px;",
      tags$h1(
        class = "hero-animate welcome-hero-logo",
        style = "font-size: 58px; font-weight: 800; margin-bottom: 10px; letter-spacing: -0.5px; line-height: 1.15;",
        icon("dna", style = "margin-right: 16px; color: #a78bfa; filter: drop-shadow(0 0 16px rgba(167,139,250,0.9)); vertical-align: middle;"),
        tags$span(class = "hero-title-gradient", "GExPipe")
      ),
      tags$p(
        class = "hero-animate",
        style = "color: rgba(255,255,255,0.98); font-size: 20px; font-weight: 600; letter-spacing: 0.3px; margin-bottom: 6px; text-shadow: 0 2px 20px rgba(0,0,0,0.3);",
        "Gene Expression Pipeline"
      ),
      tags$p(
        class = "hero-animate",
        style = "color: rgba(255,255,255,0.85); font-size: 16px; font-weight: 500; letter-spacing: 0.4px; margin-bottom: 28px;",
        "One workflow: GEO → QC → DE → WGCNA → PPI → ML → reports. No coding required."
      ),
      # Stats badges
      tags$div(
        class = "hero-animate",
        style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 14px; margin-bottom: 36px;",
        lapply(list(
          list("16", "Analysis Steps", "#a78bfa"),
          list("GEO", "Integrated", "#22d3ee"),
          list("RNA-seq", "+ Microarray", "#34d399"),
          list("DESeq2", "limma WGCNA", "#fbbf24")
        ), function(x) {
          tags$div(
            class = "badge-pop",
            style = sprintf(
              "background: linear-gradient(145deg, rgba(255,255,255,0.14), rgba(255,255,255,0.06)); backdrop-filter: blur(12px);
               border: 1px solid rgba(255,255,255,0.25); padding: 14px 22px; border-radius: 16px; color: #fff; font-weight: 700; font-size: 13px;
               box-shadow: 0 6px 20px rgba(0,0,0,0.25); transition: transform 0.3s, box-shadow 0.3s;",
              x[[3]]
            ),
            tags$span(style = sprintf("color: %s; font-size: 20px; display: block; margin-bottom: 3px; font-weight: 800; text-shadow: 0 0 20px %s40;", x[[3]], x[[3]]), x[[1]]),
            tags$span(style = "opacity: 0.92; font-size: 11px; font-weight: 600; letter-spacing: 0.3px;", x[[2]])
          )
        })
      ),
      # 3D Pipeline Flow - Analysis Steps
      tags$div(
        class = "hero-animate pipeline-3d",
        style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 14px; margin-top: 24px; padding: 28px;",
        lapply(list(
          list("1", "Download", "#6366f1"),
          list("2", "QC", "#7c3aed"),
          list("3-5", "Preprocess", "#a855f7"),
          list("6", "DE", "#ec4899"),
          list("7-8", "WGCNA", "#0ea5e9"),
          list("9-10", "PPI & ML", "#10b981"),
          list("11-16", "Report", "#f59e0b")
        ), function(x) {
          tags$div(
            class = "step",
            style = sprintf(
              "background: linear-gradient(145deg, %s, rgba(0,0,0,0.25)); color: white; padding: 14px 20px;
               border-radius: 14px; font-weight: bold; font-size: 13px; min-width: 92px;
               border: 1px solid rgba(255,255,255,0.25);",
              x[[3]]
            ),
            tags$span(style = "opacity: 0.95; font-size: 11px; letter-spacing: 0.5px;", x[[1]]),
            tags$br(),
            tags$span(style = "font-size: 13px;", x[[2]])
          )
        })
      )
    ),
    # Main Card
    tags$div(
      class = "welcome-card",
      style = "background: linear-gradient(180deg, rgba(255,255,255,0.99) 0%, rgba(248,250,252,0.98) 100%);
                border-radius: 32px; padding: 52px 56px;
                box-shadow: 0 32px 88px rgba(0,0,0,0.2), 0 0 0 1px rgba(255,255,255,0.5), inset 0 1px 0 rgba(255,255,255,0.8);
                margin-bottom: 36px; animation: fadeInUp 0.9s ease 1.2s forwards; opacity: 0;",
      tags$p(
        style = "color: #334155; font-size: 16px; line-height: 1.9; margin-bottom: 28px; text-align: left; font-weight: 500;
                 letter-spacing: 0.15px;",
        "GExPipe walks you through a complete gene expression analysis: download data from GEO, run quality checks,
         normalize and correct for batch effects, find differentially expressed genes, build co-expression networks (WGCNA),
         enrich pathways, analyze protein interactions (PPI), run machine learning, and export publication-ready figures and reports."
      ),
      tags$h3(
        style = "color: #1e293b; font-size: 20px; font-weight: 700; margin: 26px 0 14px 0; text-align: left;
                 border-bottom: 3px solid #8b5cf6; padding-bottom: 8px; display: inline-block; letter-spacing: -0.2px;",
        icon("sitemap", style = "margin-right: 10px; color: #8b5cf6;"),
        "What you can do"
      ),
      tags$div(
        style = "text-align: left; margin: 0 0 28px 0; font-size: 14px; line-height: 1.95; color: #475569;",
        tags$div(class = "pipeline-item", tags$strong("1. Download"), " — GEO data (RNA-seq / Microarray / Merged) with gene symbol mapping"),
        tags$div(class = "pipeline-item", tags$strong("2. QC & Visualization"), " — Gene overlap, PCA, sample connectivity"),
        tags$div(class = "pipeline-item", tags$strong("3–5. Normalize, Groups, Batch"), " — Preprocessing and batch correction"),
        tags$div(class = "pipeline-item", tags$strong("6. Differential Expression"), " — limma / DESeq2 / edgeR"),
        tags$div(class = "pipeline-item", tags$strong("7–8. WGCNA & Common Genes"), " — Co-expression modules and enrichment"),
        tags$div(class = "pipeline-item", tags$strong("9–10. PPI & ML"), " — Protein interaction networks and predictive modeling"),
        tags$div(class = "pipeline-item", tags$strong("11–16. Validation, ROC, Nomogram, GSEA, Immune, Summary"), " — Validation and report")
      ),
      tags$h3(
        style = "color: #1e293b; font-size: 20px; font-weight: 700; margin: 26px 0 14px 0; text-align: left;
                 border-bottom: 3px solid #8b5cf6; padding-bottom: 8px; display: inline-block; letter-spacing: -0.2px;",
        icon("star", style = "margin-right: 10px; color: #f59e0b;"),
        "Why GExPipe?"
      ),
      tags$ul(
        style = "text-align: left; padding-left: 26px; margin: 0 0 38px 0; color: #475569; font-size: 14px; line-height: 2.1;
                 list-style: none;",
        tags$li(style = "position: relative; padding-left: 8px; margin: 6px 0;",
                icon("check-circle", style = "position: absolute; left: -22px; top: 4px; color: #10b981; font-size: 14px;"),
                "Supports both RNA-seq and microarray in one workflow"),
        tags$li(style = "position: relative; padding-left: 8px; margin: 6px 0;",
                icon("check-circle", style = "position: absolute; left: -22px; top: 4px; color: #10b981; font-size: 14px;"),
                "Multi-dataset merge with common gene mapping across platforms"),
        tags$li(style = "position: relative; padding-left: 8px; margin: 6px 0;",
                icon("check-circle", style = "position: absolute; left: -22px; top: 4px; color: #10b981; font-size: 14px;"),
                "Step-by-step pipeline with guided options and validation"),
        tags$li(style = "position: relative; padding-left: 8px; margin: 6px 0;",
                icon("check-circle", style = "position: absolute; left: -22px; top: 4px; color: #10b981; font-size: 14px;"),
                "Publication-ready figures and downloadable results"),
        tags$li(style = "position: relative; padding-left: 8px; margin: 6px 0;",
                icon("check-circle", style = "position: absolute; left: -22px; top: 4px; color: #10b981; font-size: 14px;"),
                "Optional workspace save/load to resume analyses")
      ),
      tags$p(
        style = "color: #64748b; font-size: 14px; font-weight: 600; margin-bottom: 8px; text-align: center;",
        "Ready? Click below to open the pipeline."
      ),
      tags$div(
        style = "text-align: center; margin-top: 16px; padding-top: 4px;",
        actionButton(
          "go_to_analysis",
          tagList(icon("rocket", style = "margin-right: 12px; font-size: 18px;"), " Start Analyzing"),
          class = "btn-lg go-btn",
          style = "color: white; border: none; font-weight: 800; padding: 22px 58px; font-size: 20px;
                   border-radius: 50px; letter-spacing: 0.6px;
                   transition: transform 0.3s, box-shadow 0.3s, background-position 0.5s;"
        ),
        tags$p(
          style = "color: #94a3b8; font-size: 12px; margin-top: 14px; font-weight: 500;",
          "GExPipe — Gene Expression Pipeline"
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
