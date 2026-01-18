# Instalacja wymaganych pakietów dla Symulatora Spóźnień Autobusu

required_packages <- c("shiny", "ggplot2", "dplyr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste("Instaluję pakiet:", pkg, "\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat(paste("Pakiet", pkg, "już zainstalowany\n"))
  }
}

cat("\nWszystkie pakiety zainstalowane!\n")
cat("Możesz teraz uruchomić aplikację:\n")
cat("  shiny::runApp()\n")