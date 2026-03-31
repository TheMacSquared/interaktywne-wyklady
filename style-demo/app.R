# Style Demo - katalog wariantow stylizacji Shiny
# Jedna scrollowalna aplikacja z sekcjami pokazujacymi rozne podejscia do CSS

library(shiny)

# ============================================================================
# CSS
# ============================================================================

demo_css <- HTML("
/* === GLOBAL === */
body { background: #f0f2f5; }

.demo-section {
  background: white;
  border-radius: 12px;
  padding: 30px;
  margin: 20px auto;
  max-width: 1100px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
}
.demo-section h2 {

  font-size: 24px;
  font-weight: 700;
  color: #2c3e50;
  margin-bottom: 8px;
}
.demo-section .demo-desc {
  color: #7f8c8d;
  font-size: 14px;
  margin-bottom: 20px;
}
.variant-label {
  font-size: 12px;
  color: #95a5a6;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-bottom: 8px;
  font-weight: 600;
}
.variant-box {
  border: 1px dashed #dee2e6;
  border-radius: 8px;
  padding: 16px;
  margin-bottom: 16px;
  min-height: 80px;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  gap: 8px;
}

/* === SEKCJA 1: PRZYCISKI === */

/* 1. Gradient */
.btn-gradient {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  border: none;
  color: white;
  padding: 10px 24px;
  border-radius: 6px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
  transition: all 0.3s ease;
}
.btn-gradient:hover {
  box-shadow: 0 6px 20px rgba(102, 126, 234, 0.6);
  transform: translateY(-2px);
}

/* 2. Pill */
.btn-pill {
  background: #3498db;
  border: none;
  color: white;
  padding: 10px 28px;
  border-radius: 50px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}
.btn-pill:hover {
  background: #2980b9;
  transform: scale(1.05);
}

/* 3. Outlined hover fill */
.btn-outline-fill {
  background: transparent;
  border: 2px solid #3498db;
  color: #3498db;
  padding: 9px 24px;
  border-radius: 6px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}
.btn-outline-fill:hover {
  background: #3498db;
  color: white;
}

/* 4. Neomorphism */
.btn-neo {
  background: #e0e5ec;
  border: none;
  color: #2c3e50;
  padding: 10px 24px;
  border-radius: 10px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  box-shadow: 6px 6px 12px #b8bec7, -6px -6px 12px #ffffff;
  transition: all 0.2s ease;
}
.btn-neo:hover {
  box-shadow: 4px 4px 8px #b8bec7, -4px -4px 8px #ffffff;
}
.btn-neo:active {
  box-shadow: inset 4px 4px 8px #b8bec7, inset -4px -4px 8px #ffffff;
}

/* 5. Animated ripple */
.btn-ripple {
  position: relative;
  overflow: hidden;
  background: #27ae60;
  border: none;
  color: white;
  padding: 10px 24px;
  border-radius: 6px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  transition: background 0.3s;
}
.btn-ripple:hover { background: #219a52; }
.btn-ripple .ripple-effect {
  position: absolute;
  border-radius: 50%;
  background: rgba(255,255,255,0.5);
  transform: scale(0);
  animation: ripple-anim 0.6s linear;
  pointer-events: none;
}
@keyframes ripple-anim {
  to { transform: scale(4); opacity: 0; }
}

/* 6. Flat minimal */
.btn-flat {
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  color: #2c3e50;
  padding: 10px 24px;
  border-radius: 4px;
  font-size: 14px;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.2s ease;
}
.btn-flat:hover {
  background: #e9ecef;
  border-color: #adb5bd;
}

/* 7. Gradient outline */
.btn-gradient-outline {
  position: relative;
  background: white;
  border: none;
  color: #2c3e50;
  padding: 10px 24px;
  border-radius: 6px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  z-index: 1;
}
.btn-gradient-outline::before {
  content: '';
  position: absolute;
  top: -2px; left: -2px; right: -2px; bottom: -2px;
  background: linear-gradient(135deg, #667eea, #764ba2, #f093fb);
  border-radius: 8px;
  z-index: -1;
}
.btn-gradient-outline::after {
  content: '';
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  background: white;
  border-radius: 6px;
  z-index: -1;
  transition: opacity 0.3s;
}
.btn-gradient-outline:hover::after {
  opacity: 0;
}
.btn-gradient-outline:hover {
  color: white;
}

/* 8. Bounce */
.btn-bounce {
  background: #e74c3c;
  border: none;
  color: white;
  padding: 10px 24px;
  border-radius: 6px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  transition: transform 0.2s;
}
.btn-bounce:hover {
  animation: bounce-anim 0.4s ease;
}
@keyframes bounce-anim {
  0% { transform: scale(1); }
  30% { transform: scale(1.15); }
  50% { transform: scale(0.95); }
  70% { transform: scale(1.05); }
  100% { transform: scale(1); }
}

/* === SEKCJA 2: KARTY === */

.card-demo {
  padding: 20px;
  border-radius: 10px;
  margin-bottom: 12px;
  min-height: 120px;
}
.card-demo h4 { margin-top: 0; font-size: 16px; }
.card-demo p { font-size: 13px; color: #666; margin: 0; }

/* Obecny widget-block */
.card-baseline {
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
}

/* Glass-morphism */
.card-glass {
  background: rgba(255, 255, 255, 0.25);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  box-shadow: 0 8px 32px rgba(31, 38, 135, 0.15);
}
.card-glass-bg {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  border-radius: 10px;
  padding: 4px;
}

/* Gradient border */
.card-gradient-border {
  background: white;
  border: 2px solid transparent;
  background-clip: padding-box;
  position: relative;
}
.card-gradient-border::before {
  content: '';
  position: absolute;
  top: -2px; left: -2px; right: -2px; bottom: -2px;
  background: linear-gradient(135deg, #3498db, #27ae60, #f39c12);
  border-radius: 12px;
  z-index: -1;
}

/* Top bar */
.card-topbar {
  background: white;
  border: 1px solid #dee2e6;
  border-top: 4px solid #3498db;
}

/* Dark mode */
.card-dark {
  background: #1a1a2e;
  border: 1px solid #16213e;
  color: #e0e0e0;
}
.card-dark h4 { color: #eaf4fc; }
.card-dark p { color: #a0a0b0; }

/* Neomorphism card */
.card-neo {
  background: #e0e5ec;
  border: none;
  box-shadow: 8px 8px 16px #b8bec7, -8px -8px 16px #ffffff;
}

/* Accent left border */
.card-accent {
  background: white;
  border: 1px solid #dee2e6;
  border-left: 5px solid #9b59b6;
}

/* === SEKCJA 3: QUIZ ANSWERS === */

.quiz-demo { margin-bottom: 24px; }
.quiz-demo .quiz-question {
  font-size: 15px;
  font-weight: 600;
  color: #2c3e50;
  margin-bottom: 12px;
}

/* Wariant A: lista (obecny) */
.quiz-list .quiz-btn {
  display: block;
  width: 100%;
  text-align: left;
  padding: 12px 16px;
  margin-bottom: 8px;
  background: white;
  border: 2px solid #dee2e6;
  border-radius: 8px;
  font-size: 14px;
  cursor: pointer;
  transition: all 0.2s;
}
.quiz-list .quiz-btn:hover {
  border-color: #3498db;
  background: #eaf4fc;
}

/* Wariant B: tile/grid */
.quiz-tiles {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 12px;
}
.quiz-tiles .quiz-tile {
  background: white;
  border: 2px solid #dee2e6;
  border-radius: 12px;
  padding: 20px 12px;
  text-align: center;
  cursor: pointer;
  transition: all 0.3s;
}
.quiz-tiles .quiz-tile:hover {
  border-color: #3498db;
  transform: translateY(-4px);
  box-shadow: 0 8px 25px rgba(52, 152, 219, 0.2);
}
.quiz-tiles .quiz-tile .tile-letter {
  display: inline-block;
  width: 36px;
  height: 36px;
  line-height: 36px;
  border-radius: 50%;
  background: #3498db;
  color: white;
  font-weight: 700;
  font-size: 16px;
  margin-bottom: 8px;
}
.quiz-tiles .quiz-tile .tile-text {
  font-size: 13px;
  color: #2c3e50;
}

/* Wariant C: karty hover-lift */
.quiz-lift .quiz-card {
  background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
  border: 1px solid #dee2e6;
  border-radius: 10px;
  padding: 14px 18px;
  margin-bottom: 10px;
  cursor: pointer;
  transition: all 0.3s cubic-bezier(0.25, 0.8, 0.25, 1);
  display: flex;
  align-items: center;
  gap: 14px;
}
.quiz-lift .quiz-card:hover {
  box-shadow: 0 14px 28px rgba(0,0,0,0.12), 0 10px 10px rgba(0,0,0,0.08);
  transform: translateY(-4px);
}
.quiz-lift .quiz-card .card-letter {
  min-width: 40px;
  height: 40px;
  line-height: 40px;
  text-align: center;
  border-radius: 8px;
  background: linear-gradient(135deg, #667eea, #764ba2);
  color: white;
  font-weight: 700;
  font-size: 16px;
}
.quiz-lift .quiz-card .card-text {
  font-size: 14px;
  color: #2c3e50;
}

/* Wariant D: animowany wjazd */
.quiz-slide .quiz-slide-btn {
  display: block;
  width: 100%;
  text-align: left;
  padding: 12px 18px;
  margin-bottom: 8px;
  background: white;
  border: 2px solid #27ae60;
  border-radius: 8px;
  font-size: 14px;
  cursor: pointer;
  opacity: 0;
  transform: translateX(-40px);
  animation: slide-in 0.4s ease forwards;
}
.quiz-slide .quiz-slide-btn:nth-child(1) { animation-delay: 0.1s; }
.quiz-slide .quiz-slide-btn:nth-child(2) { animation-delay: 0.25s; }
.quiz-slide .quiz-slide-btn:nth-child(3) { animation-delay: 0.4s; }
.quiz-slide .quiz-slide-btn:hover {
  background: #27ae60;
  color: white;
}
@keyframes slide-in {
  to { opacity: 1; transform: translateX(0); }
}

/* === SEKCJA 4: CALLOUTS === */

.callout-demo {
  padding: 14px 18px;
  margin-bottom: 12px;
  font-size: 14px;
  color: #2c3e50;
}
.callout-demo strong { display: block; margin-bottom: 4px; }

/* Obecny */
.callout-v1 {
  background: #eaf4fc;
  border-left: 4px solid #3498db;
  border-radius: 0 6px 6px 0;
}

/* Full border + ikona */
.callout-v2 {
  background: #eaf4fc;
  border: 1px solid #3498db;
  border-radius: 8px;
  padding-left: 44px;
  position: relative;
}
.callout-v2::before {
  content: '\\2139';
  position: absolute;
  left: 14px;
  top: 14px;
  font-size: 18px;
  color: #3498db;
}

/* Rounded z wyr. tlem */
.callout-v3 {
  background: #d4efdf;
  border: none;
  border-radius: 12px;
  color: #1a5632;
}

/* Top border */
.callout-v4 {
  background: #fef9e7;
  border-top: 4px solid #f39c12;
  border-radius: 0 0 8px 8px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.06);
}

/* Gradient side */
.callout-v5 {
  background: white;
  border-left: 4px solid transparent;
  border-image: linear-gradient(to bottom, #667eea, #764ba2) 1;
  border-radius: 0;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}

/* Dark callout */
.callout-v6 {
  background: #2c3e50;
  color: #ecf0f1;
  border-left: 4px solid #e74c3c;
  border-radius: 0 8px 8px 0;
}
.callout-v6 strong { color: #e74c3c; }
")

# ============================================================================
# RIPPLE JS
# ============================================================================

ripple_js <- HTML("
  document.addEventListener('click', function(e) {
    if (!e.target.classList.contains('btn-ripple')) return;
    var btn = e.target;
    var circle = document.createElement('span');
    circle.classList.add('ripple-effect');
    var d = Math.max(btn.clientWidth, btn.clientHeight);
    circle.style.width = circle.style.height = d + 'px';
    circle.style.left = (e.clientX - btn.getBoundingClientRect().left - d/2) + 'px';
    circle.style.top = (e.clientY - btn.getBoundingClientRect().top - d/2) + 'px';
    btn.appendChild(circle);
    setTimeout(function() { circle.remove(); }, 600);
  });
")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(demo_css),
    tags$script(ripple_js)
  ),

  div(style = "max-width: 1100px; margin: 0 auto; padding: 20px;",
    h1("Katalog stylizacji Shiny", style = "text-align: center; color: #2c3e50; margin-bottom: 5px;"),
    p("Warianty CSS do wykorzystania w aplikacjach edukacyjnych",
      style = "text-align: center; color: #7f8c8d; margin-bottom: 30px;")
  ),

  # ========================================================================
  # SEKCJA 1: PRZYCISKI
  # ========================================================================
  div(class = "demo-section",
    h2("1. Warianty przycisk\u00f3w"),
    p(class = "demo-desc", "Ka\u017cdy rz\u0105d pokazuje inny styl. Najedz kursorem i kliknij."),

    # Obecny
    div(class = "variant-box",
      div(class = "variant-label", "Obecny styl (Bootstrap btn-primary)"),
      actionButton("btn_base", "Kliknij mnie", class = "btn-primary"),
      actionButton("btn_base2", "Generuj dane", class = "btn-primary"),
      actionButton("btn_base3", "Resetuj", class = "btn-default")
    ),

    # Gradient
    div(class = "variant-box",
      div(class = "variant-label", "Gradient z cieniem"),
      tags$button(class = "btn-gradient", "Kliknij mnie"),
      tags$button(class = "btn-gradient", style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
                   box-shadow: 0 4px 15px rgba(245, 87, 108, 0.4);", "Generuj dane"),
      tags$button(class = "btn-gradient", style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
                   box-shadow: 0 4px 15px rgba(79, 172, 254, 0.4);", "Resetuj")
    ),

    # Pill
    div(class = "variant-box",
      div(class = "variant-label", "Pill / zaokr\u0105glone"),
      tags$button(class = "btn-pill", "Kliknij mnie"),
      tags$button(class = "btn-pill", style = "background: #27ae60;", "Generuj dane"),
      tags$button(class = "btn-pill", style = "background: #e74c3c;", "Resetuj")
    ),

    # Outlined hover fill
    div(class = "variant-box",
      div(class = "variant-label", "Outlined + hover fill"),
      tags$button(class = "btn-outline-fill", "Kliknij mnie"),
      tags$button(class = "btn-outline-fill", style = "border-color: #27ae60; color: #27ae60;",
                  onmouseover = "this.style.background='#27ae60'; this.style.color='white'",
                  onmouseout = "this.style.background='transparent'; this.style.color='#27ae60'",
                  "Generuj dane"),
      tags$button(class = "btn-outline-fill", style = "border-color: #e74c3c; color: #e74c3c;",
                  onmouseover = "this.style.background='#e74c3c'; this.style.color='white'",
                  onmouseout = "this.style.background='transparent'; this.style.color='#e74c3c'",
                  "Resetuj")
    ),

    # Neomorphism
    div(class = "variant-box", style = "background: #e0e5ec;",
      div(class = "variant-label", "Neomorphism (mi\u0119kkie cienie)"),
      tags$button(class = "btn-neo", "Kliknij mnie"),
      tags$button(class = "btn-neo", "Generuj dane"),
      tags$button(class = "btn-neo", "Resetuj")
    ),

    # Z ikonami
    div(class = "variant-box",
      div(class = "variant-label", "Z ikonami (shiny::icon)"),
      actionButton("btn_icon1", tagList(icon("play"), " Uruchom"), class = "btn-primary"),
      actionButton("btn_icon2", tagList(icon("chart-bar"), " Wykres"), class = "btn-success"),
      actionButton("btn_icon3", tagList(icon("redo"), " Resetuj"), class = "btn-warning"),
      actionButton("btn_icon4", tagList(icon("download"), " Pobierz"), class = "btn-info")
    ),

    # Ripple
    div(class = "variant-box",
      div(class = "variant-label", "Animated ripple (kliknij!)"),
      tags$button(class = "btn-ripple", "Kliknij mnie"),
      tags$button(class = "btn-ripple", style = "background: #3498db;", "Generuj dane"),
      tags$button(class = "btn-ripple", style = "background: #9b59b6;", "Resetuj")
    ),

    # Flat minimal
    div(class = "variant-box",
      div(class = "variant-label", "Flat minimal"),
      tags$button(class = "btn-flat", "Kliknij mnie"),
      tags$button(class = "btn-flat", "Generuj dane"),
      tags$button(class = "btn-flat", "Resetuj")
    ),

    # Gradient outline
    div(class = "variant-box",
      div(class = "variant-label", "Gradient outline (hover = gradient fill)"),
      tags$button(class = "btn-gradient-outline", "Kliknij mnie"),
      tags$button(class = "btn-gradient-outline", "Generuj dane")
    ),

    # Bounce
    div(class = "variant-box",
      div(class = "variant-label", "Bounce (hover animation)"),
      tags$button(class = "btn-bounce", "Kliknij mnie"),
      tags$button(class = "btn-bounce", style = "background: #3498db;", "Generuj dane"),
      tags$button(class = "btn-bounce", style = "background: #f39c12;", "Resetuj")
    )
  ),

  # ========================================================================
  # SEKCJA 2: KARTY I PANELE
  # ========================================================================
  div(class = "demo-section",
    h2("2. Karty i panele"),
    p(class = "demo-desc", "Alternatywy dla obecnego .widget-block"),

    fluidRow(
      column(4,
        div(class = "variant-label", "Obecny widget-block"),
        div(class = "card-demo card-baseline",
          h4("Tytu\u0142 widgetu"),
          p("Standardowy kontener na interaktywne elementy.")
        )
      ),
      column(4,
        div(class = "variant-label", "Glass-morphism"),
        div(class = "card-glass-bg",
          div(class = "card-demo card-glass",
            h4(style = "color: white;", "Tytu\u0142 widgetu"),
            p(style = "color: rgba(255,255,255,0.8);", "Przezroczyste t\u0142o z efektem blur.")
          )
        )
      ),
      column(4,
        div(class = "variant-label", "Gradient border"),
        div(class = "card-demo card-gradient-border",
          h4("Tytu\u0142 widgetu"),
          p("T\u0119czowa obw\u00f3dka z gradientem.")
        )
      )
    ),

    fluidRow(
      column(4,
        div(class = "variant-label", "Colored top bar"),
        div(class = "card-demo card-topbar",
          h4("Tytu\u0142 widgetu"),
          p("Kolorowy pasek u g\u00f3ry identyfikuje typ.")
        )
      ),
      column(4,
        div(class = "variant-label", "Dark mode"),
        div(class = "card-demo card-dark",
          h4("Tytu\u0142 widgetu"),
          p("Ciemny motyw, przyjemny dla oczu.")
        )
      ),
      column(4,
        div(class = "variant-label", "Neomorphism"),
        div(class = "card-demo card-neo",
          h4("Tytu\u0142 widgetu"),
          p("Mi\u0119kkie cienie, efekt wypuk\u0142o\u015bci.")
        )
      )
    ),

    fluidRow(
      column(4,
        div(class = "variant-label", "Accent left border"),
        div(class = "card-demo card-accent",
          h4("Tytu\u0142 widgetu"),
          p("Kolorowy akcent z lewej strony.")
        )
      ),
      column(4,
        div(class = "variant-label", "Top bar \u2014 red"),
        div(class = "card-demo card-topbar", style = "border-top-color: #e74c3c;",
          h4("Tytu\u0142 widgetu"),
          p("Inny kolor paska = inna kategoria.")
        )
      ),
      column(4,
        div(class = "variant-label", "Top bar \u2014 green"),
        div(class = "card-demo card-topbar", style = "border-top-color: #27ae60;",
          h4("Tytu\u0142 widgetu"),
          p("Zielony = sukces, gotowe, poprawne.")
        )
      )
    )
  ),

  # ========================================================================
  # SEKCJA 3: QUIZ ANSWERS
  # ========================================================================
  div(class = "demo-section",
    h2("3. Przyciski odpowiedzi quizu"),
    p(class = "demo-desc", "Jak mog\u0105 wygl\u0105da\u0107 opcje odpowiedzi w quizie."),

    fluidRow(
      column(6,
        div(class = "quiz-demo",
          div(class = "variant-label", "A) Lista przycisk\u00f3w (obecny styl)"),
          div(class = "quiz-question", "Kt\u00f3ry rozk\u0142ad modeluje liczb\u0119 sukces\u00f3w?"),
          div(class = "quiz-list",
            tags$button(class = "quiz-btn", "A) Rozk\u0142ad dwumianowy"),
            tags$button(class = "quiz-btn", "B) Rozk\u0142ad Poissona"),
            tags$button(class = "quiz-btn", "C) Rozk\u0142ad normalny")
          )
        )
      ),
      column(6,
        div(class = "quiz-demo",
          div(class = "variant-label", "B) Kafelki (tile/grid)"),
          div(class = "quiz-question", "Kt\u00f3ry rozk\u0142ad modeluje liczb\u0119 sukces\u00f3w?"),
          div(class = "quiz-tiles",
            div(class = "quiz-tile",
              div(class = "tile-letter", "A"),
              div(class = "tile-text", "Rozk\u0142ad dwumianowy")
            ),
            div(class = "quiz-tile",
              div(class = "tile-letter", "B"),
              div(class = "tile-text", "Rozk\u0142ad Poissona")
            ),
            div(class = "quiz-tile",
              div(class = "tile-letter", "C"),
              div(class = "tile-text", "Rozk\u0142ad normalny")
            )
          )
        )
      )
    ),

    fluidRow(
      column(6,
        div(class = "quiz-demo",
          div(class = "variant-label", "C) Karty z hover-lift"),
          div(class = "quiz-question", "Kt\u00f3ry rozk\u0142ad modeluje liczb\u0119 sukces\u00f3w?"),
          div(class = "quiz-lift",
            div(class = "quiz-card",
              div(class = "card-letter", "A"),
              div(class = "card-text", "Rozk\u0142ad dwumianowy")
            ),
            div(class = "quiz-card",
              div(class = "card-letter", "B"),
              div(class = "card-text", "Rozk\u0142ad Poissona")
            ),
            div(class = "quiz-card",
              div(class = "card-letter", "C"),
              div(class = "card-text", "Rozk\u0142ad normalny")
            )
          )
        )
      ),
      column(6,
        div(class = "quiz-demo",
          div(class = "variant-label", "D) Animowany wjazd z lewej"),
          div(class = "quiz-question", "Kt\u00f3ry rozk\u0142ad modeluje liczb\u0119 sukces\u00f3w?"),
          div(class = "quiz-slide",
            tags$button(class = "quiz-slide-btn", "A) Rozk\u0142ad dwumianowy"),
            tags$button(class = "quiz-slide-btn", "B) Rozk\u0142ad Poissona"),
            tags$button(class = "quiz-slide-btn", "C) Rozk\u0142ad normalny")
          )
        )
      )
    )
  ),

  # ========================================================================
  # SEKCJA 4: CALLOUT / ALERT BOXY
  # ========================================================================
  div(class = "demo-section",
    h2("4. Callout / alert boxy"),
    p(class = "demo-desc", "Warianty ramek informacyjnych, ostrze\u017ce\u0144 i sukces\u00f3w."),

    fluidRow(
      column(6,
        div(class = "variant-label", "Obecny (border-left)"),
        div(class = "callout-demo callout-v1",
          tags$strong("Informacja"),
          "Rozk\u0142ad normalny jest symetryczny wzgl\u0119dem \u015bredniej."
        ),

        div(class = "variant-label", "Full border + ikona"),
        div(class = "callout-demo callout-v2",
          tags$strong("Informacja"),
          "Rozk\u0142ad normalny jest symetryczny wzgl\u0119dem \u015bredniej."
        ),

        div(class = "variant-label", "Rounded z wyra\u017anym t\u0142em"),
        div(class = "callout-demo callout-v3",
          tags$strong("Sukces!"),
          "Poprawna odpowied\u017a \u2014 to rozk\u0142ad dwumianowy."
        )
      ),
      column(6,
        div(class = "variant-label", "Top border"),
        div(class = "callout-demo callout-v4",
          tags$strong("Uwaga"),
          "Parametr lambda musi by\u0107 dodatni."
        ),

        div(class = "variant-label", "Gradient side"),
        div(class = "callout-demo callout-v5",
          tags$strong("Wskaz\u00f3wka"),
          "Rozk\u0142ad Poissona przybli\u017ca si\u0119 do normalnego dla du\u017cych lambda."
        ),

        div(class = "variant-label", "Dark callout"),
        div(class = "callout-demo callout-v6",
          tags$strong("B\u0142\u0105d"),
          "Nie mo\u017cna obliczy\u0107 wariancji dla jednej obserwacji."
        )
      )
    )
  ),

  # ========================================================================
  # SEKCJA 5: MOTYWY BSLIB
  # ========================================================================
  div(class = "demo-section",
    h2("5. Gotowe motywy (bslib)"),
    p(class = "demo-desc",
      "Pakiet bslib pozwala zmieni\u0107 ca\u0142y motyw aplikacji jedn\u0105 lini\u0105 kodu."),

    div(class = "variant-box",
      p("Aby u\u017cy\u0107 bslib w aplikacji, zmie\u0144 ", tags$code("fluidPage(...)"), " na:"),
      tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; font-size: 13px;",
        'library(bslib)\n\nui <- fluidPage(\n  theme = bs_theme(bootswatch = "flatly"),  # lub "darkly", "minty", "sandstone"\n  ...\n)'
      ),
      p(style = "margin-top: 12px; font-size: 14px;",
        tags$strong("Dost\u0119pne motywy Bootswatch:"), br(),
        "flatly (jasny, nowoczesny) \u2022 ",
        "darkly (ciemny) \u2022 ",
        "minty (zielonkawy) \u2022 ",
        "sandstone (ciep\u0142y) \u2022 ",
        "lux (elegancki) \u2022 ",
        "cosmo (kolorowy) \u2022 ",
        "journal (naglog\u00f3wki serif) \u2022 ",
        "cerulean (niebieski)"
      ),
      p(style = "font-size: 14px; color: #7f8c8d; margin-top: 8px;",
        "Pe\u0142na lista: ", tags$code("bslib::bootswatch_themes()"),
        " | Podgl\u0105d: bootswatch.com"
      ),
      p(style = "font-size: 14px; margin-top: 12px;",
        tags$strong("Custom theme:"),
        tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; font-size: 13px; margin-top: 6px;",
          'theme = bs_theme(\n  bg = "#ffffff",        # tlo\n  fg = "#2c3e50",        # tekst\n  primary = "#3498db",   # kolor glowny\n  base_font = font_google("Inter"),\n  heading_font = font_google("Poppins")\n)'
        )
      )
    )
  ),

  # Footer
  div(style = "text-align: center; color: #bdc3c7; padding: 30px; font-size: 13px;",
    "Style Demo \u2014 katalog inspiracji do stylizacji aplikacji Shiny"
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # Pusta - demo jest czysto wizualne
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
