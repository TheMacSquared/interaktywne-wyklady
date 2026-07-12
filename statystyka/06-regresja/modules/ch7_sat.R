# Krótki wariant regresyjny dla danych satelitarnych.

.ch7_sat_reg_data <- read.csv(
  file.path(project_root, "dane", "satelitarne_obserwacje.csv"),
  stringsAsFactors = FALSE
)

.ch7_sat_reg_panel <- function(id, title, ...) {
  figure_panel(
    label = paste("Ćw. S", id), title = title, tagList(...),
    actionButton(paste0("ch7_sat_ans", id), "Pokaż rozwiązanie",
                 class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput(paste0("ch7_sat_sol", id))
  )
}

ch7_sat_ui <- function() tagList(
  lc_h2("ch7-satelitarne", "Wariant: dane satelitarne"),
  lc_feedback(type = "info",
    p("Użyj ", tags$code("../dane/satelitarne_obserwacje.csv"),
      ". NDVI jest tu tylko liczbowym wskaźnikiem roślinności; nie omawiamy
       sposobu jego wyznaczania z pasm spektralnych.")
  ),

  .ch7_sat_reg_panel("1", "Temperatura a wskaźnik roślinności",
    p("Dopasuj model ", tags$code("sat_temp_c ~ ndvi"), "."),
    tags$ol(
      tags$li("Narysuj wykres punktowy z prostą regresji."),
      tags$li("Zinterpretuj nachylenie dla wzrostu NDVI o 0.1."),
      tags$li("Podaj R² i nazwij ograniczenie interpretacji przyczynowej.")
    )),

  .ch7_sat_reg_panel("2", "Co zostaje po kontroli warunków?",
    p("Porównaj modele:"),
    tags$ol(
      tags$li(tags$code("sat_temp_c ~ ndvi")),
      tags$li(tags$code("sat_temp_c ~ ndvi + wysokosc_m + zachmurzenie_pct + strefa"))
    ),
    p("Jak zmienia się współczynnik NDVI i adjusted R²? Dlaczego drugi model
       nadal nie jest automatycznie modelem przyczynowym?")),

  .ch7_sat_reg_panel("3", "Kalibracja względem pomiaru naziemnego",
    p("Dopasuj ", tags$code("grunt_temp_c ~ sat_temp_c"), "."),
    tags$ol(
      tags$li("Podaj równanie, R² i RMSE."),
      tags$li("Przewidź temperaturę naziemną dla sat_temp_c=30°C."),
      tags$li("Czy wysokie R² wystarcza, aby wykluczyć systematyczne obciążenie sensora?"),
      tags$li("Sprawdź także średnią ", tags$code("sat_temp_c - grunt_temp_c"), ".")
    )),

  lc_feedback(type = "warning",
    p("W rzeczywistych danych podział trening/test powinien uwzględniać miejsce
       i czas. Losowe rozdzielenie sąsiednich pikseli może dać zbyt optymistyczną
       ocenę predykcji. Na tym kursie wystarczy rozpoznać problem.")
  )
)

ch7_sat_server <- function(input, output, session) {
  d <- .ch7_sat_reg_data

  output$ch7_sat_sol1 <- renderUI({
    if (!isTruthy(input$ch7_sat_ans1) || input$ch7_sat_ans1 < 1) return(NULL)
    m <- lm(sat_temp_c ~ ndvi, data = d)
    b <- coef(m); g <- broom::glance(m)
    lc_feedback(type = "ok",
      p(tags$code(sprintf("sat_temp_c = %.2f %+ .2f × ndvi", b[1], b[2]))),
      p(sprintf("Wzrost NDVI o 0.1 wiąże się przeciętnie ze zmianą temperatury o %.2f°C.",
                0.1 * b[2])),
      p(sprintf("R²=%.3f. Związek może odzwierciedlać także typ pokrycia, wysokość,
                 termin i inne warunki obserwacji.", g$r.squared))
    )
  })

  output$ch7_sat_sol2 <- renderUI({
    if (!isTruthy(input$ch7_sat_ans2) || input$ch7_sat_ans2 < 1) return(NULL)
    m1 <- lm(sat_temp_c ~ ndvi, data = d)
    m2 <- lm(sat_temp_c ~ ndvi + wysokosc_m + zachmurzenie_pct + strefa, data = d)
    b1 <- coef(m1)["ndvi"]; b2 <- coef(m2)["ndvi"]
    g1 <- broom::glance(m1); g2 <- broom::glance(m2)
    lc_feedback(type = "ok",
      p(sprintf("β_NDVI: model prosty %.2f, model z kontrolą %.2f.", b1, b2)),
      p(sprintf("Adjusted R²: %.3f → %.3f.", g1$adj.r.squared, g2$adj.r.squared)),
      p("Kontrola zmiennych poprawia opis, ale dane obserwacyjne nadal nie
         gwarantują kompletności zmiennych zakłócających ani kierunku przyczynowego.")
    )
  })

  output$ch7_sat_sol3 <- renderUI({
    if (!isTruthy(input$ch7_sat_ans3) || input$ch7_sat_ans3 < 1) return(NULL)
    m <- lm(grunt_temp_c ~ sat_temp_c, data = d)
    b <- coef(m); g <- broom::glance(m)
    pred <- predict(m, newdata = data.frame(sat_temp_c = 30))
    rmse <- sqrt(mean(residuals(m)^2))
    bias <- mean(d$sat_temp_c - d$grunt_temp_c)
    lc_feedback(type = "ok",
      p(tags$code(sprintf("grunt_temp_c = %.2f %+ .2f × sat_temp_c", b[1], b[2]))),
      p(sprintf("R²=%.3f, RMSE=%.2f°C, predykcja dla 30°C: %.2f°C.",
                g$r.squared, rmse, pred)),
      p(sprintf("Średnie satelita−grunt=%.2f°C. Wysoka korelacja/R² może współistnieć
                 z systematycznym przesunięciem pomiarów.", bias))
    )
  })
}
