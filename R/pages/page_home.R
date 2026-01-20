# R/pages/page_home.R
page_home_ui <- function() {
  tags$div(
    id = "home-landing",
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Fraunces:wght@600;700;800&family=Manrope:wght@400;500;600;700&display=swap"
      ),
      tags$style(htmltools::HTML("
        :root {
          --home-red: var(--primary, #c9414d);
          --home-red-dark: var(--primary-dark, #a33540);
          --home-ink: var(--text-primary, #1a1a1a);
          --home-muted: var(--text-secondary, #5a5a5a);
          --home-line: var(--border-light, #e8e4df);
        }

        #home-landing {
          min-height: 100vh;
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          overflow: auto;
          background: linear-gradient(180deg, #e8b4ba 0%, #ffffff 30%, #ffffff 70%, #e8b4ba 100%) !important;
          font-family: 'Manrope', 'Segoe UI', sans-serif;
          color: var(--home-ink);
          display: flex;
          flex-direction: column;
          z-index: 100;
        }

        #home-landing .home-container {
          position: relative;
          z-index: 1;
          max-width: 1280px;
          margin: 0 auto;
          padding: 48px 48px 28px;
          width: 100%;
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
        }

        #home-landing .main-stack {
          flex: 1 1 auto;
          display: flex;
          flex-direction: column;
          justify-content: center;
          gap: 28px;
        }

        #home-landing .hero {
          display: grid;
          grid-template-columns: 1fr;
          gap: 24px;
          align-items: center;
          justify-items: center;
          text-align: center;
        }

        #home-landing .hero-title {
          font-family: 'Fraunces', 'Georgia', serif;
          font-size: 64px;
          line-height: 0.95;
          margin: 0 0 36px 0;
          letter-spacing: -1px;
        }

        #home-landing .cta-row {
          display: flex;
          gap: 12px;
          flex-wrap: wrap;
          align-items: center;
          justify-content: center;
          margin-top: 6px;
        }

        #home-landing .btn {
          border-radius: 10px;
          font-weight: 700;
          padding: 14px 22px;
          border: 1px solid transparent;
          cursor: pointer;
          text-decoration: none;
          display: inline-block;
        }

        #home-landing .btn-primary {
          background: var(--home-red);
          color: #ffffff;
          border-color: var(--home-red-dark);
          box-shadow: 0 10px 24px rgba(201, 65, 77, 0.22);
          transition: all var(--duration-fast, 150ms) ease;
        }

        #home-landing .btn-primary:hover { background: var(--home-red-dark); }

        #home-landing .btn-ghost {
          background: transparent;
          border-color: var(--home-line);
          color: var(--home-ink);
          transition: all var(--duration-fast, 150ms) ease;
        }

        #home-landing .btn-ghost:hover {
          border-color: var(--home-red);
          color: var(--home-red);
        }

        #home-landing .section {
          margin-top: 0;
        }

        #home-landing .section-head {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
        }

        #home-landing .section-title {
          font-size: 22px;
          margin: 0;
        }

        #home-landing .workflow {
          display: grid;
          grid-template-columns: repeat(4, minmax(180px, 1fr));
          gap: 14px;
          margin-top: 18px;
        }

        #home-landing .wf-card {
          background: var(--bg-card, #ffffff);
          border: 1px solid rgba(201, 65, 77, 0.35);
          border-radius: 16px;
          padding: 16px;
          min-height: 170px;
          display: flex;
          flex-direction: column;
          gap: 8px;
          box-shadow: var(--shadow-md, 0 4px 12px rgba(26, 26, 26, 0.06));
          transition: transform var(--duration-normal, 250ms) var(--ease-out, cubic-bezier(0.16, 1, 0.3, 1)), box-shadow var(--duration-normal, 250ms) ease, border-color var(--duration-fast, 150ms) ease;
        }
        #home-landing .wf-card:hover {
          transform: translateY(-4px);
          box-shadow: var(--shadow-lg, 0 8px 24px rgba(26, 26, 26, 0.08));
          border-color: rgba(201, 65, 77, 0.6);
        }

        #home-landing .wf-step {
          font-weight: 800;
          color: var(--home-red);
          font-size: 14px;
        }
        #home-landing .wf-title { font-weight: 700; font-size: 16px; }
        #home-landing .wf-desc { color: var(--home-muted); font-size: 13px; }
        #home-landing .wf-link {
          margin-top: auto;
          color: var(--home-ink);
          font-weight: 700;
          text-decoration: none;
        }
        #home-landing .wf-button {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          padding: 10px 14px;
          border: 1px solid var(--home-line);
          border-radius: 10px;
          background: var(--bg-card, #ffffff);
          font-weight: 700;
          transition: all var(--duration-fast, 150ms) ease;
        }
        #home-landing .wf-button:hover {
          border-color: var(--home-red);
          color: #ffffff;
          background: var(--home-red);
        }

        #home-landing .footer {
          margin-top: 46px;
          padding-top: 18px;
          border-top: 1px solid var(--home-line);
          color: var(--home-muted);
          font-size: 13px;
          display: flex;
          flex-direction: column;
          gap: 10px;
          flex-wrap: wrap;
        }
        #home-landing .footer-row {
          display: flex;
          justify-content: space-between;
          align-items: center;
          gap: 12px;
          width: 100%;
        }
        #home-landing .footer-links {
          display: flex;
          gap: 14px;
          flex-wrap: wrap;
          justify-content: center;
        }
        #home-landing .footer-links a {
          color: var(--home-ink);
          text-decoration: none;
          font-weight: 700;
        }
        #home-landing .footer-links a:hover {
          color: var(--home-red);
        }

        #home-landing .fade-in {
          animation: rise var(--duration-slow, 400ms) var(--ease-out, cubic-bezier(0.16, 1, 0.3, 1)) both;
        }
        #home-landing .delay-1 { animation-delay: 80ms; }
        #home-landing .delay-2 { animation-delay: 160ms; }
        #home-landing .delay-3 { animation-delay: 240ms; }
        #home-landing .delay-4 { animation-delay: 320ms; }

        @keyframes rise {
          0% { opacity: 0; transform: translateY(16px); }
          100% { opacity: 1; transform: translateY(0); }
        }

        @media (max-width: 980px) {
          #home-landing .workflow { grid-template-columns: repeat(2, minmax(160px, 1fr)); }
          #home-landing .home-container { padding: 36px 28px 22px; }
        }

        @media (max-width: 640px) {
          #home-landing .hero-title { font-size: 44px; }
          #home-landing .workflow { grid-template-columns: 1fr; }
          #home-landing .footer-row { flex-direction: column; align-items: flex-start; }
          #home-landing .footer-links { justify-content: flex-start; }
        }
      "))
    ),
    div(
      class = "home-container",
      div(
        class = "main-stack",
        div(
          class = "hero fade-in",
          div(
            class = "hero-left",
            div(class = "hero-title", "MS Terp"),
            div(
              class = "cta-row",
              actionButton("home_go_newrun", "Start new run", class = "btn btn-primary"),
              actionButton("home_go_results", "Open results", class = "btn btn-ghost"),
              actionButton("home_go_tutorial", "Walkthrough", class = "btn btn-ghost")
            )
          )
        ),
        div(
          class = "section delay-2 fade-in",
          div(
            class = "section-head",
            tags$h3(class = "section-title", "")
          ),
          div(
            class = "workflow",
            div(
              class = "wf-card",
              div(class = "wf-step", "01"),
              div(class = "wf-title", "Prepare data"),
              div(class = "wf-desc", "Format raw files with experimental details."),
              actionButton("home_go_format", "Format data", class = "wf-link wf-button")
            ),
            div(
              class = "wf-card",
              div(class = "wf-step", "02"),
              div(class = "wf-title", "Build TerpBase"),
              div(class = "wf-desc", "Build custom libraries"),
              actionButton("home_go_db", "Open TerpBase", class = "wf-link wf-button")
            ),
            div(
              class = "wf-card",
              div(class = "wf-step", "03"),
              div(class = "wf-title", "Design pipeline"),
              div(class = "wf-desc", "Compose analysis steps"),
              actionButton("home_go_pipeline", "Open TerpFlow", class = "wf-link wf-button")
            ),
            div(
              class = "wf-card",
              div(class = "wf-step", "04"),
              div(class = "wf-title", "Run + export"),
              div(class = "wf-desc", "Generate tables and plots."),
              actionButton("home_go_newrun2", "Start run", class = "wf-link wf-button")
            )
          )
        )
      ),
      div(
        class = "footer delay-4 fade-in",
        div(
          class = "footer-row",
          tags$span("The Hao Research Group @ UMD"),
          tags$span("Version 1.0")
        ),
        div(
          class = "footer-links",
          actionLink("home_go_tools", "Tools"),
          actionLink("home_go_about", "About us")
        )
      )
    )
  )
}

page_home_server <- function(input, output, session, app_state) {
  invisible(NULL)
}
