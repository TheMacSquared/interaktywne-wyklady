# Punkt stop 13 — Przypadek · domknięcie „Serpuchów, policzony”
# Puenta: Pietrow policzył w głowie to, co sala liczy teraz na łańcuchu szans.
# Piaskownica na jawnych założeniach: prawdziwych liczb z 1983 roku nikt nie zna.
#
# Wstawienie: a3_stop_serpuchow_case_ui jako body sekcji (rozdział „sprawdzenie”),
# rycina lcSF „a3_sp” w modules/detektor.js (bez serwera).

# Rachunek (wzór 3.8: szanse a priori 10^-k, alarm satelity × 10^m, milczenie
# radaru × (1 − s)) liczy modules/detektor.js w log10 szans.

# Wykres (strzałki z mnożnikami, oś P(atak) w skali log10 szans) i odczyty:
# modules/detektor.js, rycina lcSF „a3_sp”.

# UI -------------------------------------------------------------------------------

a3_stop_serpuchow_case_ui <- tagList(
  lc_p("Wracamy do nocy z 26 września 1983 roku. System Oko zgłasza pięć startów, Pietrow ma kilka minut i nie wierzy. Teraz mamy narzędzie, którego on nie miał: łańcuch szans ze wzoru (3.8). Liczby na suwakach to założenia do dyskusji, nie dane historyczne — prawdziwych wartości nikt nie zna."),
  lc_p("Pierwszy suwak to szanse a priori 1 : 10ᵏ, że akurat tej nocy trwa pierwszy atak, i to w postaci kilku rakiet, a nie setek. Drugi to iloraz wiarygodności alarmu satelity 10ᵐ: ile razy częściej taki alarm pojawia się przy ataku niż bez niego. Trzeci to czułość radarów naziemnych s. Radar widzi rakietę dopiero, gdy ta wzniesie się nad horyzont, więc milczenie nabierało wagi z każdą minutą. Radary milczały, a milczenie ma iloraz LR₋ = (1 − s) / 1, bo radar nie widzi rakiet, których nie ma."),
  risk_formula("\\frac{P(\\text{atak}\\mid \\text{alarm},\\,\\text{cisza})}{P(\\text{brak}\\mid \\text{alarm},\\,\\text{cisza})}=10^{-k}\\cdot 10^{m}\\cdot(1-s)"),
  risk_try("odczytaj posterior po samym satelicie przy ustawieniach domyślnych. Potem, bez patrzenia na radary, znajdź najmniejsze m, przy którym alarm przekracza przerywaną linię 0.5 — jak to m zależy od k? Na koniec zmniejszaj czułość radaru s do zera: kiedy milczenie radarów przestaje uspokajać?"),
  figure_panel(label = "Przypadek", title = "Serpuchów, policzony: łańcuch szans", width_mode = "wide",
    lc_sf_figure("a3_sp",
      control = list(
        lc_sf_slider("a3_sp_prior", "Szanse a priori 1 : 10ᵏ, k", 2, 8, 5, 0.5),
        lc_sf_slider("a3_sp_lr", "LR satelity 10ᵐ, m", 0, 6, 3, 0.5),
        lc_sf_slider("a3_sp_radar", "Czułość radaru s", 0, 0.99, 0.9, 0.01)),
      toolbar = lc_readouts(
        lc_readout("Po satelicie", lc_sf_value("sp2"), color = upwr_secondary),
        lc_readout("Po radarach", lc_sf_value("sp3"), color = upwr_accent)),
      ratio = "1.9/1", max_height = "400px",
      after = lc_caption("Oś pionowa w skali log szans; przerywana linia to P(atak) = 0.5. Wszystkie liczby są założeniami."))
  ),
  lc_p("Przy ustawieniach domyślnych szanse a priori wynoszą 1 : 100 000. Alarm satelity mnoży je przez 1000: 10⁻⁵ · 10³ = 10⁻², czyli P(atak | alarm) = 0.01 / 1.01 ≈ 0.0099. Milczenie radarów przy czułości 0.9 mnoży szanse jeszcze przez 0.1: 10⁻³, czyli posterior ≈ 0.001. Nawet bardzo dobry satelita nie wystarczy, gdy zdarzenie jest tak rzadkie: żeby przekroczyć 0.5 bez radarów, LR satelity musiałby przekroczyć odwrotność szans a priori, tu 100 000. Przy s = 0 radar nic nie wnosi (LR₋ = 1) i zostaje sam satelita."),
  lc_p("Mnożenie ilorazów jest tu uprawnione, bo dwa źródła mylą się z różnych powodów. Fałszywy alarm wywołało odbicie słońca od wysokich chmur, które czujnik podczerwieni satelity wziął za płomienie silników. Radar widzi obiekty w inny sposób i tego samego odblasku nie zobaczy, więc warunkowa niezależność z definicji 3.7 jest wiarygodna. Inaczej jest z pięcioma sygnałami satelity: powstały z jednej przyczyny, więc liczymy je jako jeden alarm, a nie pięć niezależnych."),
  lc_p("Zostaje próg reakcji. Obie pomyłki są katastrofalne: przeoczony atak i wojna rozpętana przez błąd czujnika. Przy posteriorze rzędu 0.001 odwet byłby prawie na pewno błędem, a meldunek o fałszywym alarmie — prawie na pewno słuszny. Pietrow nie liczył, ale ważył te same trzy rzeczy: jak rzadki jest taki atak, ile wart jest nowy system i co mówi drugie, niezależne źródło.")
)
