# Porównanie fontów dla projektu Interaktywne Wykłady
# Uruchom: shiny::runApp("sandbox/fonts-comparison")
#
# Cztery kandydaci obok siebie, ten sam polski paragraf z wzorem i kodem.
# Wszystkie obsługują Latin Extended (polskie znaki).

library(shiny)
library(bslib)

# ============================================================================
# PRZYKŁADOWY TEKST (polski, z diakrytyką, wzorem, kodem)
# ============================================================================

sample_paragraph <- HTML(
  '<h1 class="demo-h1">Rozdział 3: Przedziały ufności</h1>
   <h2 class="demo-h2">Idea przedziału</h2>
   <p class="demo-p">
     Przedział ufności to zakres wartości, w którym — z określonym
     prawdopodobieństwem — spodziewamy się znaleźć <em>prawdziwy</em>
     parametr populacji. Najczęściej używamy poziomu ufności 95%,
     co oznacza, że gdybyśmy powtórzyli badanie wielokrotnie, około 95 ze 100
     przedziałów zawierałoby prawdziwą średnią.
   </p>
   <h3 class="demo-h3">Wzór na CI dla średniej</h3>
   <p class="demo-p">
     Dla dużej próby: \\( \\bar{x} \\pm z_{\\alpha/2} \\cdot \\frac{s}{\\sqrt{n}} \\),
     gdzie <strong>s</strong> to odchylenie standardowe próby.
   </p>
   <pre class="demo-code">t.test(x, conf.level = 0.95)$conf.int
# [1] 12.34 18.76</pre>
   <p class="demo-p small">
     Źródło cyfr: 0123456789 · Ligatury: fi fl · Znaki specjalne: ≤ ≥ ≠ → α β μ σ
   </p>'
)

# ============================================================================
# FONT CARDS - każdy kafelek ładuje swój font
# ============================================================================

font_card <- function(id, label, family_css, google_link) {
  tags$div(
    class = "font-card",
    style = paste0("font-family: ", family_css, ";"),
    tags$head(tags$link(rel = "stylesheet", href = google_link)),
    tags$div(class = "font-label", label),
    sample_paragraph
  )
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  theme = bs_theme(bootswatch = "sandstone"),
  withMathJax(),

  tags$head(tags$style(HTML("
    body { background: #ffffff; }
    .header { padding: 20px 30px; border-bottom: 1px solid #dee2e6; margin-bottom: 0; }
    .header h1 { margin: 0; font-size: 22px; color: #2c3e50; }
    .header .hint { color: #7f8c8d; font-size: 14px; margin-top: 6px; }

    .grid { display: grid; grid-template-columns: 1fr 1fr;
            gap: 0; border-top: 1px solid #dee2e6; }
    .font-card { padding: 25px 30px; border-right: 1px solid #dee2e6;
                 border-bottom: 1px solid #dee2e6; min-height: 420px; }
    .font-card:nth-child(even) { border-right: none; }

    .font-label { font-family: 'JetBrains Mono', monospace;
                  font-size: 11px; letter-spacing: 1px; text-transform: uppercase;
                  color: #3498db; margin-bottom: 12px; }

    .demo-h1 { font-size: 28px; font-weight: 700; color: #2c3e50;
               border-bottom: 3px solid #3498db; padding-bottom: 6px;
               margin: 0 0 12px 0; }
    .demo-h2 { font-size: 20px; font-weight: 600; color: #34495e;
               margin: 18px 0 8px 0; }
    .demo-h3 { font-size: 16px; font-weight: 600; color: #34495e;
               margin: 14px 0 6px 0; }
    .demo-p  { font-size: 15px; line-height: 1.65; color: #2c3e50;
               margin: 0 0 10px 0; max-width: 62ch; }
    .demo-p.small { font-size: 13px; color: #7f8c8d; }
    .demo-code { font-family: 'JetBrains Mono', 'Fira Code', monospace;
                 font-size: 13px; background: #f8f9fa; border: 1px solid #e9ecef;
                 border-radius: 4px; padding: 10px 12px; margin: 10px 0; }
  "))),

  div(class = "header",
      h1("Porównanie fontów — Interaktywne Wykłady"),
      div(class = "hint",
          "Ten sam paragraf w 4 fontach. Wszystkie wspierają polskie znaki. ",
          "Porównaj czytelność, wygląd cyfr, ciężar nagłówków.")
  ),

  div(class = "grid",
      font_card(
        "inter", "Inter (sans, neutralny, UI-first)",
        "'Inter', -apple-system, sans-serif",
        "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap&subset=latin-ext"
      ),
      font_card(
        "source", "Source Sans 3 (nagłówki) + Source Serif 4 (narracja)",
        "'Source Serif 4', Georgia, serif",
        "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&family=Source+Serif+4:wght@400;600;700&display=swap&subset=latin-ext"
      ),
      font_card(
        "atkinson", "Atkinson Hyperlegible (dostępność, dysleksja)",
        "'Atkinson Hyperlegible', sans-serif",
        "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:wght@400;700&display=swap&subset=latin-ext"
      ),
      font_card(
        "lato", "Lato (polski autor, sprawdzony w PL)",
        "'Lato', sans-serif",
        "https://fonts.googleapis.com/css2?family=Lato:wght@400;700;900&display=swap&subset=latin-ext"
      )
  ),

  # Dodatkowy specimen: tylko nagłówki jednego fontu w 3 rozmiarach
  tags$div(style = "padding: 30px; border-top: 2px solid #3498db; margin-top: 0;",
    tags$h2("Notatki dla osoby porównującej",
            style = "color: #2c3e50; margin-bottom: 10px;"),
    tags$ul(
      tags$li("Zwróć uwagę na ", tags$strong("cyfry"), " — czy są proporcjonalne (tabularne) czy staromodne."),
      tags$li("Porównaj ", tags$strong("polskie znaki"), " (ą, ł, ź) — czy są spójne z resztą tekstu."),
      tags$li("Kursywa (", tags$em("prawdziwy"), ") — czy wyraźnie odróżnia się od zwykłego."),
      tags$li("Kod (", tags$code("t.test()"), ") — wszystkie używają JetBrains Mono."),
      tags$li("Wzór matematyczny — MathJax ma własny font, ale powinien współgrać z tekstem.")
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
