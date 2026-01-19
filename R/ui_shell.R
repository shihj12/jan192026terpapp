# R/ui_shell.R
msterp_theme_head <- function() {
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600;700;800;900&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fraunces:wght@600;700;800&display=swap"
    ),
    tags$style(HTML("
      /* === Global CSS Variables (Spec) === */
      :root {
        /* Softer color palette */
        --primary: #c9414d;           /* Muted burgundy (was #d50032) */
        --primary-dark: #a33540;
        --primary-light: #f8e8ea;

        --accent-gold: #d4a84b;       /* Muted amber (was #FFD200) */
        --accent-gold-light: #faf3e3;

        --charcoal: #3d3d3d;          /* Soft black (was #000000) */
        --charcoal-light: #5a5a5a;

        /* Neutral backgrounds - warm cream tones */
        --bg-page: #faf9f7;
        --bg-card: #ffffff;
        --bg-muted: #f5f3f0;
        --bg-hover: #f0eeeb;

        /* Text */
        --text-primary: #1a1a1a;
        --text-secondary: #5a5a5a;
        --text-muted: #8a8a8a;

        /* Borders */
        --border-light: #e8e4df;
        --border-medium: #d4cfc7;

        /* Shadows */
        --shadow-sm: 0 1px 3px rgba(26, 26, 26, 0.04);
        --shadow-md: 0 4px 12px rgba(26, 26, 26, 0.06);
        --shadow-lg: 0 8px 24px rgba(26, 26, 26, 0.08);

        /* Animation tokens */
        --ease-out: cubic-bezier(0.16, 1, 0.3, 1);
        --ease-in-out: cubic-bezier(0.65, 0, 0.35, 1);
        --duration-fast: 150ms;
        --duration-normal: 250ms;
        --duration-slow: 400ms;

        /* Legacy aliases for backward compatibility */
        --md-red: var(--primary);
        --md-gold: var(--accent-gold);
        --md-gold-light: var(--accent-gold-light);
        --md-green: #0f8a3a;
        --md-bg: var(--bg-page);
        --md-text: var(--text-primary);
        --md-border: var(--border-medium);
        --md-card-bg: var(--bg-card);
        --md-card-border: var(--border-medium);
        --md-card-shadow: var(--shadow-md);
        --md-nav-bg: #1b1b1b;
        --md-nav-item-bg: #292929;
        --md-nav-item-border: #383838;
        --md-nav-hover: #2f2f2f;

        /* Tabbed cardset variables */
        --tab-bg: var(--bg-card);
        --tab-border: var(--border-medium);
        --tab-hover-bg: var(--bg-muted);
        --tab-active-bg: var(--bg-muted);
        --tab-accent: var(--primary);
        --tab-accent-strong: var(--primary-dark);
        --tab-text: var(--text-primary);
        --tab-text-hover: var(--primary);
        --tab-text-active: var(--primary);
        --tab-text-muted: var(--text-secondary);
        --tab-radius: 12px;
        --tab-pill-pad: 4px;
      }

      /* === Animation Keyframes === */
      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(16px); }
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      @keyframes shimmer {
        0% { background-position: -200% 0; }
        100% { background-position: 200% 0; }
      }
      @keyframes spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      @keyframes indeterminate {
        0% { transform: translateX(-100%); }
        100% { transform: translateX(200%); }
      }

      /* === Animation Utility Classes === */
      .animate-fade-in {
        animation: fadeInUp var(--duration-slow) var(--ease-out) both;
      }
      .animate-delay-1 { animation-delay: 50ms; }
      .animate-delay-2 { animation-delay: 100ms; }
      .animate-delay-3 { animation-delay: 150ms; }
      .animate-delay-4 { animation-delay: 200ms; }
      .animate-delay-5 { animation-delay: 250ms; }
      .stagger-item {
        animation: fadeInUp var(--duration-normal) var(--ease-out) both;
      }
      .stagger-item:nth-child(1) { animation-delay: 0ms; }
      .stagger-item:nth-child(2) { animation-delay: 50ms; }
      .stagger-item:nth-child(3) { animation-delay: 100ms; }
      .stagger-item:nth-child(4) { animation-delay: 150ms; }
      .stagger-item:nth-child(5) { animation-delay: 200ms; }

      /* === Skeleton Loaders === */
      .skeleton {
        background: linear-gradient(90deg, var(--bg-muted) 25%, var(--bg-hover) 50%, var(--bg-muted) 75%);
        background-size: 200% 100%;
        animation: shimmer 1.5s infinite;
        border-radius: 4px;
      }
      .skeleton-text {
        height: 1em;
        margin-bottom: 0.5em;
        border-radius: 4px;
      }
      .skeleton-text:last-child { width: 60%; }
      .skeleton-circle {
        width: 40px;
        height: 40px;
        border-radius: 50%;
      }
      .skeleton-rect {
        height: 100px;
        border-radius: 8px;
      }

      /* === Spinner === */
      .spinner-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 12px;
        padding: 24px;
      }
      .spinner {
        width: 32px;
        height: 32px;
        border: 3px solid var(--border-light);
        border-top-color: var(--primary);
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
      }
      .spinner-text {
        font-size: 14px;
        color: var(--text-secondary);
      }

      /* === Progress Bar === */
      .progress-bar-container {
        width: 100%;
        height: 6px;
        background: var(--bg-muted);
        border-radius: 999px;
        overflow: hidden;
      }
      .progress-bar-fill {
        height: 100%;
        background: var(--primary);
        border-radius: 999px;
        transition: width var(--duration-normal) var(--ease-out);
      }
      .progress-bar-fill.indeterminate {
        width: 40%;
        animation: indeterminate 1.2s var(--ease-in-out) infinite;
      }

      html, body {
        height: 100%;
        font-family: 'Source Sans Pro', 'Segoe UI', sans-serif;
        background: var(--md-bg);
        color: var(--md-text);
      }

      /* Important for grid/scroll correctness */
      *, *::before, *::after { box-sizing: border-box; }

      /* === Shell wrapper (.shell alias) === */
      .msterp-wrap, .shell {
        --topbar-h: 56px;
        --bg: var(--md-bg);
        --banner: var(--md-red);
        --panel: #ffffff;
        --panel2: var(--bg-muted);
        --border: var(--md-border);
        --gap: 0px;

        background: var(--bg);
        min-height: 100vh;
        padding: 0;
        border-radius: 0;
      }

      /* Top banner */
      .msterp-topbar {
        height: var(--topbar-h);
        background: var(--banner);
        color: #fff;
        border-radius: 0;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 16px;
        margin-bottom: 0;
      }
      .msterp-topbar img { height: 44px; width: auto; display: block; }
      .msterp-topbar-center {
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 1 1 auto;
        min-width: 0;
        padding: 0 12px;
      }
      .msterp-topbar-title {
        font-weight: 800;
        font-size: 16px;
        letter-spacing: 0.3px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        font-family: 'Source Sans Pro', 'Segoe UI', sans-serif;
      }

      /* === Shell layout: sidebar + content (.shell > .nav + .main) === */
      .msterp-shell {
        display: flex;
        flex-direction: row;
        gap: 0;
        height: calc(100vh - var(--topbar-h));
        min-height: 0;
      }

      /* === Sidebar (.nav alias) === */
      .msterp-sidebar, .nav {
        width: 220px;
        flex: 0 0 220px;
        background: var(--md-nav-bg);
        border: none;
        border-radius: 0;
        padding: 12px;
        overflow: auto;
        position: relative;
        min-height: 0;
        display: flex;
        flex-direction: column;
        transition: width var(--duration-normal) var(--ease-out), flex var(--duration-normal) var(--ease-out);
      }

      /* === Main content (.main alias) === */
      .msterp-content, .main {
        flex: 1;
        background: var(--panel);
        border: none;
        border-radius: 0;
        padding: 16px;
        overflow: auto;
        min-height: 0;
        min-width: 0;
        display: flex;
        flex-direction: column;
        gap: 16px;
        height: 100%;
        width: 100%;
      }
      .msterp-content > * {
        flex-shrink: 0;
      }
      .msterp-content > .two-panel,
      .msterp-content > .bslib-card {
        flex: 1 1 auto;
        min-height: 0;
      }

      /* === Page wrapper === */
      .msterp-page {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
        gap: 12px;
        width: 100%;
        max-width: 1200px;
        margin: 0 auto;
      }
      .msterp-page.full-bleed {
        max-width: none;
        margin: 0;
      }
      .msterp-page-header {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
        padding: 6px 0 8px;
        border-bottom: 1px solid var(--md-border);
      }
      .msterp-page-title {
        font-size: 18px;
        font-weight: 800;
        margin: 0;
      }
      .msterp-page-actions {
        display: flex;
        align-items: center;
        gap: 8px;
        flex-wrap: wrap;
      }
      .msterp-page-body {
        flex: 1 1 auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
        gap: 16px;
      }
      .msterp-page-body > .two-panel,
      .msterp-page-body > .bslib-card {
        flex: 1 1 auto;
        min-height: 0;
      }

      /* === Top row pattern for pages === */
      .top {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 12px;
        margin-bottom: 8px;
      }

      /* Typography hierarchy (shell pages only) */
      .msterp-wrap h1,
      .msterp-wrap h2 {
        font-weight: 800;
      }
      .msterp-wrap h3,
      .msterp-wrap h4 {
        font-weight: 700;
      }
      .msterp-wrap h5,
      .msterp-wrap h6 {
        font-weight: 600;
      }
      .msterp-wrap p,
      .msterp-wrap li,
      .msterp-wrap small {
        font-weight: 400;
      }
      .msterp-wrap label,
      .msterp-wrap .control-label {
        font-weight: 600;
      }
      .msterp-wrap strong {
        font-weight: 700;
      }

      .msterp-nav-title { font-weight: 700; margin: 4px 0 10px; color: #fff; }

      /* === Nav items (dark theme per spec) === */
      .msterp-nav {
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        gap: 8px;
        padding-top: 4px;
        padding-bottom: 8px;
      }
      .msterp-nav a.action-button {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: flex-start;
        gap: 10px;
        text-align: left;
        padding: 10px 12px;
        text-decoration: none;
        color: #fff;
        background: var(--md-nav-item-bg);
        border: 1px solid var(--md-nav-item-border);
        border-radius: 8px;
        margin: 0;
        transition: background 0.15s ease;
      }
      .msterp-nav .nav-row {
        display: flex;
        align-items: center;
        justify-content: flex-start;
        gap: 10px;
        width: 100%;
        min-width: 0;
      }
      .msterp-nav a:hover {
        background: var(--md-nav-hover);
      }
      .msterp-nav a.active, .msterp-nav a[aria-current='page'] {
        background: var(--primary);
        color: #fefefe;
        border-color: var(--primary-dark);
      }

      .msterp-nav .nav-icon {
        width: 22px;
        height: 22px;
        min-width: 22px;
        min-height: 22px;
        object-fit: contain;
        flex: 0 0 22px;
        display: block;
        border-radius: 4px;
        background: transparent;
        padding: 0;
        filter: invert(1);
      }
      .msterp-nav .nav-text, .msterp-nav .label {
        flex: 1;
        font-weight: 600;
        font-size: 14px;
        line-height: 1.2;
        white-space: nowrap;
        overflow: hidden;
        transition: opacity var(--duration-fast) ease, width var(--duration-fast) ease;
      }

      .msterp-nav .hint { font-size: 12px; color: #aaa; margin-top: 10px; }

      /* === Collapsible sidebar (56px when collapsed) === */
      .msterp-shell.sidebar-collapsed .msterp-sidebar,
      .msterp-shell.sidebar-collapsed .nav {
        width: 56px;
        flex: 0 0 56px;
        padding: 12px 6px;
      }
      .msterp-shell.sidebar-collapsed .msterp-nav .nav-text,
      .msterp-shell.sidebar-collapsed .msterp-nav .label,
      .nav.collapsed .label {
        display: none;
      }
      .msterp-shell.sidebar-collapsed .msterp-nav a {
        justify-content: center;
        padding: 10px;
        width: 100%;
      }
      .msterp-shell.sidebar-collapsed .msterp-nav .nav-row {
        justify-content: center;
        gap: 0;
        width: auto;
      }
      .msterp-shell.sidebar-collapsed .msterp-nav .nav-icon {
        margin: 0 auto;
      }
      .msterp-shell.sidebar-collapsed .msterp-nav .hint { display: none; }

      /* === Nav header with toggle button === */
      .nav-header {
        display: flex;
        justify-content: flex-end;
        margin-bottom: 10px;
      }
      .sidebar-toggle {
        width: 100%;
        border: 1px solid var(--md-nav-item-border);
        background: var(--md-nav-item-bg);
        color: #fff;
        border-radius: 8px;
        padding: 8px 10px;
        cursor: pointer;
        margin-bottom: 10px;
        font-weight: 600;
        transition: background 0.15s ease;
      }
      .sidebar-toggle:hover {
        background: var(--md-nav-hover);
      }

      /* Two-panel inner layout (for embedded tools later) */
      .two-panel {
        display: grid;
        grid-template-columns: minmax(320px, 36%) minmax(0, 1fr);
        gap: 16px;
        flex: 1;
        min-height: 0;
      }
      .two-panel > .panel-left  {
        background: var(--panel2);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 16px;
        padding-bottom: 16px;
        overflow: auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      .two-panel > .panel-right {
        background: var(--panel);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 16px;
        padding-bottom: 16px;
        overflow: auto;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }

      /* Busy overlay */
      .msterp-busy {
        display: none;
        position: fixed;
        inset: 0;
        background: rgba(0,0,0,0.55);
        z-index: 9999;
        align-items: center;
        justify-content: center;
        padding: 24px;
      }
      .msterp-busy-card {
        width: min(520px, 95vw);
        background: #fff;
        border-radius: 16px;
        border: 1px solid var(--border);
        padding: 18px;
      }
      .msterp-busy-row {
        display: flex;
        gap: 14px;
        align-items: center;
      }
      .msterp-busy-media {
        width: 96px;
        height: 96px;
        border-radius: 12px;
        border: 1px solid var(--border);
        background: #f7f7f9;
        display: flex;
        align-items: center;
        justify-content: center;
        overflow: hidden;
        flex: 0 0 96px;
      }
      .msterp-busy-media img { width: 100%; height: 100%; object-fit: cover; }
      .msterp-busy-text { flex: 1; }
      .msterp-busy-text .msg { font-weight: 700; margin-bottom: 6px; }
      .msterp-progress {
        width: 100%;
        height: 12px;
        background: #f0f0f3;
        border-radius: 999px;
        border: 1px solid var(--border);
        overflow: hidden;
      }
      .msterp-progress > .bar {
        height: 100%;
        width: 0%;
        background: var(--banner);
        border-radius: 999px;
        transition: width 160ms linear;
      }
      .msterp-percent { margin-top: 8px; font-size: 12px; color: #333; }

      /* === Shared Components (Spec) === */

      /* Buttons (brand-aligned) */
      .btn.btn-primary,
      .btn-primary,
      .action-button.btn-primary {
        background: var(--md-red);
        border-color: #b30028;
        color: #fff;
        font-weight: 700;
      }
      .btn.btn-primary:hover,
      .btn-primary:hover,
      .action-button.btn-primary:hover {
        background: #b30028;
        border-color: #9a0022;
      }
      .btn.btn-default,
      .btn-default {
        background: #fff;
        border: 1px solid var(--md-border);
        color: #1a1a1a;
        font-weight: 600;
      }
      .btn.btn-default:hover,
      .btn-default:hover {
        background: #f7f4f0;
        border-color: #d3c8b8;
      }
      .btn.btn-outline-primary,
      .btn-outline-primary {
        color: var(--md-red);
        border-color: var(--md-red);
        background: transparent;
        font-weight: 600;
      }
      .btn.btn-outline-primary:hover,
      .btn-outline-primary:hover {
        background: var(--md-red);
        color: #fff;
      }

      /* === Refined UI Components === */

      /* Soft buttons */
      .btn-soft {
        background: var(--bg-muted);
        border: 1px solid var(--border-light);
        color: var(--text-primary);
        font-weight: 600;
        border-radius: 8px;
        padding: 8px 14px;
        transition: all var(--duration-fast) ease;
      }
      .btn-soft:hover {
        background: var(--bg-hover);
        border-color: var(--border-medium);
      }
      .btn-primary-soft {
        background: var(--primary-light);
        border: 1px solid transparent;
        color: var(--primary);
        font-weight: 600;
        border-radius: 8px;
        padding: 8px 14px;
        transition: all var(--duration-fast) ease;
      }
      .btn-primary-soft:hover {
        background: var(--primary);
        color: #fff;
      }
      .btn-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 32px;
        height: 32px;
        padding: 0;
        border-radius: 8px;
        background: var(--bg-muted);
        border: 1px solid var(--border-light);
        color: var(--text-secondary);
        transition: all var(--duration-fast) ease;
      }
      .btn-icon:hover {
        background: var(--bg-hover);
        color: var(--text-primary);
      }

      /* Status badges */
      .status-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
      }
      .status-badge::before {
        content: '';
        width: 8px;
        height: 8px;
        border-radius: 50%;
      }
      .status-saved {
        background: #e8f5e9;
        color: #2e7d32;
      }
      .status-saved::before {
        background: #4caf50;
      }
      .status-unsaved {
        background: var(--accent-gold-light);
        color: #8b6914;
      }
      .status-unsaved::before {
        background: var(--accent-gold);
        animation: pulse 1.5s infinite;
      }
      .status-error {
        background: #ffebee;
        color: #c62828;
      }
      .status-error::before {
        background: #ef5350;
        animation: pulse 1s infinite;
      }

      /* Toggle switches (refined) */
      .toggle-switch-track {
        width: 44px;
        height: 24px;
        background: var(--bg-muted);
        border: 1px solid var(--border-medium);
        border-radius: 12px;
        position: relative;
        cursor: pointer;
        transition: all var(--duration-fast) ease;
      }
      .toggle-switch-track.active {
        background: var(--primary);
        border-color: var(--primary-dark);
      }
      .toggle-switch-knob {
        width: 18px;
        height: 18px;
        background: #fff;
        border-radius: 50%;
        position: absolute;
        top: 2px;
        left: 2px;
        box-shadow: var(--shadow-sm);
        transition: transform var(--duration-fast) var(--ease-out);
      }
      .toggle-switch-track.active .toggle-switch-knob {
        transform: translateX(20px);
      }

      /* Card variants */
      .card-minimal {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-radius: 10px;
        padding: 14px;
      }
      .card-accent {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-left: 3px solid var(--primary);
        border-radius: 10px;
        padding: 14px;
      }
      .card-accent-gold {
        background: var(--bg-card);
        border: 1px solid var(--border-light);
        border-left: 3px solid var(--accent-gold);
        border-radius: 10px;
        padding: 14px;
      }

      /* Form controls (minimal style) */
      .form-group-minimal {
        margin-bottom: 12px;
      }
      .form-label-minimal {
        display: block;
        font-size: 12px;
        font-weight: 600;
        color: var(--text-secondary);
        margin-bottom: 4px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .form-input-minimal {
        width: 100%;
        padding: 8px 12px;
        border: 1px solid var(--border-light);
        border-radius: 8px;
        font-size: 14px;
        background: var(--bg-card);
        color: var(--text-primary);
        transition: border-color var(--duration-fast) ease, box-shadow var(--duration-fast) ease;
      }
      .form-input-minimal:focus {
        outline: none;
        border-color: var(--primary);
        box-shadow: 0 0 0 3px var(--primary-light);
      }

      /* File input alignment */
      .msterp-wrap .shiny-input-container .input-group .input-group-btn .btn-file {
        height: 38px;
        padding: 8px 12px;
        line-height: 20px;
      }
      .msterp-wrap .shiny-input-container .input-group .input-group-btn + .form-control {
        height: 38px;
        padding: 8px 12px;
      }

      /* Card component */
      .card {
        background: var(--md-card-bg);
        border: 1px solid var(--md-card-border);
        border-radius: 12px;
        padding: 16px;
        box-shadow: var(--md-card-shadow);
      }

      /* Pill component (gold) */
      .pill {
        display: inline-block;
        background: var(--md-gold-light);
        border: 1px solid var(--md-gold);
        color: #9c6a00;
        font-weight: 700;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
      }

      /* Validation lights */
      .light {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 6px 10px;
        border: 1px solid var(--md-border);
        border-radius: 8px;
        background: #fff;
        font-weight: 600;
      }
      .dot {
        width: 10px;
        height: 10px;
        border-radius: 999px;
        display: inline-block;
      }
      .dot.red { background: var(--md-red); }
      .dot.green { background: var(--md-green); }
      .dot.gray { background: #999; }

      /* Progress bar (spec) */
      .bar {
        width: 100%;
        height: 10px;
        background: #e9e2d7;
        border-radius: 999px;
        border: 1px solid var(--md-border);
        overflow: hidden;
      }
      .bar .fill {
        height: 100%;
        background: var(--md-red);
        border-radius: 999px;
        transition: width 160ms linear;
      }

      /* Logs container (dark) */
      .logs {
        background: #1f1f1f;
        border: 1px solid #383838;
        border-radius: 12px;
        padding: 12px;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-size: 12px;
        color: #e0e0e0;
        overflow-x: auto;
        overflow-y: auto;
        white-space: pre-wrap;
        min-height: 100px;
        max-height: 400px;
      }

      /* Gate pattern (shared) */
      .msterp-gate {
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 16px;
      }
      .msterp-gate-card {
        width: min(520px, 92vw);
        background: var(--md-card-bg);
        border: 1px solid var(--md-card-border);
        border-radius: 14px;
        box-shadow: var(--md-card-shadow);
        overflow: hidden;
      }
      .msterp-gate-body {
        padding: 16px;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      .msterp-gate-title {
        font-weight: 800;
        font-size: 18px;
      }
      .msterp-gate-help {
        font-size: 12px;
        color: #555;
      }
      .msterp-gate-actions {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      /* Responsive grid */
      .grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 14px;
      }

      /* === bslib Card Tab Styling (Centered, shrink-wrapped, spec-matching) === */
      .bslib-card {
        border: 1px solid var(--md-card-border);
        border-radius: 12px;
        background: var(--md-card-bg);
        box-shadow: var(--md-card-shadow);
        overflow: hidden;
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-height: 0;
        width: 100%;
      }

      /* Center the whole tab group in the card header */
      .bslib-card > .card-header {
        background: var(--tab-bg);
        border-bottom: 1px solid var(--tab-border);
        padding: 12px 8px;
        display: flex;
        justify-content: center;
        flex-shrink: 0;
      }

      /* Shrink-wrap the nav so it doesn't span full width (and add capsule border) */
      .bslib-card .nav-tabs,
      .bslib-card .nav.nav-tabs {
        background: transparent;
        border-bottom: 0;
        gap: 0;
        display: inline-flex !important;
        flex-direction: row !important;
        flex-wrap: nowrap !important;
        width: auto;
        margin: 0 auto;
        padding: var(--tab-pill-pad);
        border: 1px solid var(--tab-border);
        border-radius: calc(var(--tab-radius) + 2px);
      }

      .bslib-card .nav-tabs .nav-item {
        flex: 0 0 auto;
        display: inline-block;
      }

      /* Typography + dividers + hover/active */
      .bslib-card .nav-tabs .nav-link {
        background: transparent;
        color: var(--tab-text);
        font-family: 'Source Sans Pro', 'Segoe UI', sans-serif;
        font-weight: 700;
        font-size: 15px;
        line-height: 1.15;
        padding: 12px 24px;
        border: 0;
        border-right: 1px solid var(--tab-border);
        border-radius: var(--tab-radius);
        white-space: nowrap;
        text-align: center;
      }

      .bslib-card .nav-tabs .nav-item:last-child .nav-link {
        border-right: 0;
      }

      .bslib-card .nav-tabs .nav-link:hover {
        background: var(--tab-hover-bg);
        color: var(--tab-text-hover);
      }

      .bslib-card .nav-tabs .nav-link.active,
      .bslib-card .nav-tabs .nav-item.show .nav-link {
        background: var(--tab-active-bg);
        color: var(--tab-text-active);
        position: relative;
      }

      /* Active underline accent */
      .bslib-card .nav-tabs .nav-link.active::after {
        content: '';
        position: absolute;
        left: 12px;
        right: 12px;
        bottom: 0;
        height: 3px;
        background: var(--tab-accent);
        border-radius: 2px 2px 0 0;
      }

      .bslib-card > .card-body,
      .bslib-card > .tab-content {
        padding: 16px;
        flex: 1 1 auto;
        min-height: 0;
        overflow: auto;
        width: 100%;
      }
      .bslib-card .tab-pane {
        padding: 0;
        width: 100%;
        min-height: 0;
      }
      .bslib-card .tab-pane.active {
        display: block;
        width: 100%;
      }

      /* Narrow sizing for tabs */
      @media (max-width: 700px) {
        .bslib-card .nav-tabs .nav-link {
          font-size: 14px;
          padding: 10px 16px;
        }
      }

      /* === Responsive Behavior === */
      @media (max-width: 1200px) {
        .msterp-shell.sidebar-collapsed .msterp-sidebar,
        .msterp-shell.sidebar-collapsed .nav {
          width: 72px;
          flex: 0 0 72px;
        }
      }

      @media (max-width: 900px) {
        .msterp-shell {
          flex-direction: column;
        }
        .msterp-sidebar, .nav {
          width: 100%;
          flex: 0 0 auto;
          max-height: 200px;
        }
        .msterp-nav {
          flex-direction: row;
          flex-wrap: wrap;
          gap: 6px;
          padding-top: 0;
          padding-bottom: 0;
        }
        .msterp-nav a {
          flex: 0 0 auto;
        }
        .grid {
          grid-template-columns: 1fr;
        }
        .logs {
          overflow-x: scroll;
        }
      }
    ")),
    tags$style(HTML("
      /* Pipeline editor layout */
      .pipe-layout {
        display: grid;
        grid-template-columns: 280px minmax(0, 1fr) 320px;
        gap: var(--gap);
        height: 100%;
        min-height: 0; /* allow inner panels to scroll */
      }
      .pipe-left, .pipe-center, .pipe-right {
        border-radius: 12px;
        border: 1px solid var(--border);
        overflow: auto;
        min-height: 0; /* grid + overflow fix */
      }
      .pipe-left  { background: var(--panel2); padding: 12px; }
      .pipe-center{ background: #fff;       padding: 12px; }
      .pipe-right { background: var(--panel2); padding: 12px; }

      .pipe-section {
        border: 1px solid var(--border);
        border-radius: 12px;
        background: var(--panel2);
        padding: 10px;
        margin-bottom: 12px;
      }
      .pipe-section-title {
        font-weight: 800;
        margin-bottom: 8px;
      }
      .pipe-step-card {
        border: 1px solid var(--md-card-border);
        border-radius: 10px;
        background: var(--md-card-bg);
        padding: 8px 10px;
        margin-bottom: 8px;
        display: flex;
        justify-content: space-between;
        gap: 10px;
        align-items: center;
        box-shadow: var(--md-card-shadow);
      }
      .pipe-step-meta {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      .pipe-step-meta .small {
        font-size: 12px;
        color: #444;
      }
    ")),
    tags$script(HTML("
      // Keep sidebar toggle button label in sync with collapsed state
      function msterpSyncSidebarToggle() {
        var $shell = $('.msterp-shell');
        var collapsed = $shell.hasClass('sidebar-collapsed');
        $('#sidebar_toggle').text(collapsed ? '>' : '<');
      }

      // Sidebar collapse without server roundtrip
      $(document).on('click', '#sidebar_toggle', function() {
        $('.msterp-shell').toggleClass('sidebar-collapsed');
        msterpSyncSidebarToggle();
      });

      // Initial sync on load
      $(function() {
        msterpSyncSidebarToggle();
      });

      // Busy overlay handler
      Shiny.addCustomMessageHandler('msterp_busy', function(payload) {
        var active  = !!payload.active;
        var msg     = payload.message || '';
        var percent = payload.percent;

        var $overlay = $('#msterp_busy');
        var $msg     = $('#msterp_busy_msg');
        var $bar     = $('#msterp_busy_bar');
        var $pct     = $('#msterp_busy_pct');

        $msg.text(msg);

        if (percent === null || typeof percent === 'undefined' || isNaN(percent)) {
          $bar.css('width', '0%');
          $pct.text('');
        } else {
          var p = Math.max(0, Math.min(100, Number(percent)));
          $bar.css('width', p + '%');
          $pct.text(p.toFixed(0) + '% complete');
        }

        if (active) $overlay.css('display', 'flex');
        else $overlay.css('display', 'none');
      });

      // Sidebar collapse handler (auto-collapse on Results page)
      Shiny.addCustomMessageHandler('msterp_sidebar_collapse', function(payload) {
        var $shell = $('.msterp-shell');
        if (payload.collapsed) {
          $shell.addClass('sidebar-collapsed');
        } else {
          $shell.removeClass('sidebar-collapsed');
        }
        msterpSyncSidebarToggle();
      });

      // Reset file inputs (used by Format/New Run reset actions)
      Shiny.addCustomMessageHandler('msterp_reset_file_input', function(payload) {
        if (!payload || !payload.id) return;
        var id = payload.id;
        var el = document.getElementById(id);
        var container = null;
        var input = null;

        // Find the container and input element
        if (el && el.tagName === 'INPUT' && el.type === 'file') {
          input = el;
          container = el.closest('.shiny-input-container');
        } else if (el && el.querySelector) {
          container = el;
          input = el.querySelector('input[type=\"file\"]');
        } else {
          container = document.querySelector('#' + id);
          if (container) {
            input = container.querySelector('input[type=\"file\"]');
          }
        }

        if (!input) return;

        // Clear the actual file input
        input.value = '';

        // Reset the displayed filename text (Shiny fileInput shows filename in a text input)
        if (container) {
          var textInput = container.querySelector('input[type=\"text\"].form-control');
          if (textInput) {
            textInput.value = '';
            textInput.placeholder = 'No file selected';
          }
        }

        // Notify Shiny that the input has been cleared
        try { Shiny.setInputValue(id, null, {priority: 'event'}); } catch (e) {}
      });

      // Open URL in a new tab (used by Results viewer link-outs)
      Shiny.addCustomMessageHandler('msterp_open_url', function(payload) {
        if (!payload || !payload.url) return;
        var url = payload.url || payload;
         window.open(payload.url, '_blank', 'noopener');
      });

      // Set active nav item based on current page
      var pendingNavUpdate = null;
      var navObserver = null;

      var navIdMap = {
        'home': 'nav_home',
        'results': 'nav_results',
        'format': 'nav_format',
        'new_run': 'nav_newrun',
        'pipeline': 'nav_pipeline',
        'tools': 'nav_tools',
        'database': 'nav_db',
        'tutorial': 'nav_tutorial',
        'about': 'nav_about'
      };

      function applyNavUpdate(pageId) {
        var navId = navIdMap[pageId];
        if (!navId) return false;

        var $navItem = $('#' + navId);
        if ($navItem.length === 0) return false;

        // Remove active class from all nav items
        $('.msterp-nav a').removeClass('active');
        // Add active class to the current nav item
        $navItem.addClass('active');
        return true;
      }

      function stopNavObserver() {
        if (navObserver) {
          navObserver.disconnect();
          navObserver = null;
        }
      }

      function startNavObserver(pageId) {
        stopNavObserver();

        navObserver = new MutationObserver(function(mutations) {
          if (applyNavUpdate(pageId)) {
            stopNavObserver();
            pendingNavUpdate = null;
          }
        });

        navObserver.observe(document.body, {
          childList: true,
          subtree: true
        });
      }

      // Immediately highlight nav item on click (before server roundtrip)
      $(document).on('click', '.msterp-nav a', function(e) {
        $('.msterp-nav a').removeClass('active');
        $(this).addClass('active');
      });

      Shiny.addCustomMessageHandler('msterp_set_active_nav', function(payload) {
        var pageId = payload.page_id;

        // Try to apply immediately
        if (applyNavUpdate(pageId)) {
          pendingNavUpdate = null;
          stopNavObserver();
          return;
        }

        // If nav doesn't exist yet, use MutationObserver to watch for it
        pendingNavUpdate = pageId;
        startNavObserver(pageId);
      });

      // Scroll to element handler
      Shiny.addCustomMessageHandler('msterp_scroll_to', function(payload) {
        if (!payload || !payload.selector) return;
        var el = document.querySelector(payload.selector);
        if (el) {
          el.scrollIntoView({ behavior: 'smooth', block: 'center' });
          el.focus();
        }
      });

    "))
  )
}

topbar_ui <- function(left_logo  = "static/img/logo_left.png",
                      right_logo = "static/img/logo_right.png",
                      left_alt   = "Left logo",
                      right_alt  = "Right logo") {
  div(
    class = "msterp-topbar",
    tags$img(src = left_logo,  alt = left_alt),
    div(class = "msterp-topbar-center", uiOutput("topbar_title")),
    tags$img(src = right_logo, alt = right_alt)
  )
}

msterp_sidebar_ui <- function(about_logo = "static/img/about.svg",
                              flow_logo = "static/img/flow.svg",
                              format_logo = "static/img/format.svg",
                              home_logo = "static/img/home.svg",
                              new_run_logo = "static/img/newrun.svg",
                              results_logo = "static/img/results.svg",
                              terpbase_logo = "static/img/terpbase.svg",
                              tools_logo = "static/img/tools.svg",
                              tutorial_logo = "static/img/tutorial.svg"
) {
  nav_item <- function(id, text, icon_src) {
    actionLink(
      id,
      label = tags$span(
        class = "nav-row",
        tags$img(src = icon_src, class = "nav-icon", alt = text),
        span(class = "nav-text label", text)
      ),
      title = text
    )
  }
  
  div(
    class = "msterp-sidebar",
    tags$button(id = "sidebar_toggle", class = "sidebar-toggle", type = "button", ">"),
    div(
      class = "msterp-nav",
      nav_item("nav_home",     "Home",     home_logo),
      nav_item("nav_results",  "TerpBook",  results_logo),
      nav_item("nav_format",   "Format",   format_logo),
      nav_item("nav_newrun",   "New run",  new_run_logo),
      nav_item("nav_pipeline", "TerpFlow", flow_logo),
      nav_item("nav_tools",    "Tools",    tools_logo),
      nav_item("nav_db",       "TerpBase", terpbase_logo),
      nav_item("nav_tutorial", "Tutorial", tutorial_logo),
      nav_item("nav_about",    "About",    about_logo)
    )
  )
}


msterp_busy_overlay_ui <- function() {
  div(
    id = "msterp_busy",
    class = "msterp-busy",
    div(
      class = "msterp-busy-card",
      div(
        class = "msterp-busy-row",
        div(
          class = "msterp-busy-media",
          tags$img(src = "static/media/loading.gif", alt = "Loading")
        ),
        div(
          class = "msterp-busy-text",
          div(id = "msterp_busy_msg", class = "msg", "Working…"),
          div(class = "msterp-progress", div(id = "msterp_busy_bar", class = "bar")),
          div(id = "msterp_busy_pct", class = "msterp-percent", "")
        )
      )
    )
  )
}

single_panel_ui <- function(...) {
  tagList(...)
}

msterp_page <- function(title = NULL, actions = NULL, ..., full_bleed = FALSE, class = NULL) {
  header <- NULL
  if (!is.null(title) || !is.null(actions)) {
    header <- div(
      class = "msterp-page-header",
      if (!is.null(title)) tags$h2(class = "msterp-page-title", title) else NULL,
      if (!is.null(actions)) div(class = "msterp-page-actions", actions) else NULL
    )
  }

  div(
    class = paste(c("msterp-page", if (isTRUE(full_bleed)) "full-bleed", class), collapse = " "),
    header,
    div(class = "msterp-page-body", ...)
  )
}

two_panel_ui <- function(left_ui, right_ui) {
  div(
    class = "two-panel",
    div(class = "panel-left",  left_ui),
    div(class = "panel-right", right_ui)
  )
}

