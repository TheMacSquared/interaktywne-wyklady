FROM rocker/shiny:4.4.1

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c( \
    'ggplot2', \
    'dplyr', \
    'rstatix', \
    'broom', \
    'lmtest' \
  ), repos='https://cran.r-project.org/')"

# Remove default Shiny Server examples
RUN rm -rf /srv/shiny-server/*

# Copy Shiny Server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy portal (landing page)
COPY portal/ /srv/shiny-server/portal/

# Copy top-level apps
COPY box-plot-builder/app.R /srv/shiny-server/box-plot-builder/app.R
COPY distribution-explorer/app.R /srv/shiny-server/distribution-explorer/app.R
COPY gra-estymacja/app.R /srv/shiny-server/gra-estymacja/app.R
COPY histogram-builder/app.R /srv/shiny-server/histogram-builder/app.R
COPY moments-explorer/app.R /srv/shiny-server/moments-explorer/app.R
COPY odchylenie-standardowe/app.R /srv/shiny-server/odchylenie-standardowe/app.R
COPY regresja-interakcja/app.R /srv/shiny-server/regresja-interakcja/app.R
COPY sampling-explorer/app.R /srv/shiny-server/sampling-explorer/app.R
COPY srednia-vs-mediana/app.R /srv/shiny-server/srednia-vs-mediana/app.R
COPY zalozenia-testow/app.R /srv/shiny-server/zalozenia-testow/app.R

# Copy losowanie_spoznienia (rename underscore to hyphen for URL consistency)
COPY losowanie_spoznienia/app.R /srv/shiny-server/losowanie-spoznienia/app.R

# Copy nested Testowanie-hipotez apps (flatten to top level)
COPY Testowanie-hipotez/test-t-builder/app.R /srv/shiny-server/test-t-builder/app.R
COPY Testowanie-hipotez/chi-kwadrat-builder/app.R /srv/shiny-server/chi-kwadrat-builder/app.R
COPY Testowanie-hipotez/korelacja-builder/app.R /srv/shiny-server/korelacja-builder/app.R

# Set permissions for shiny user
RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server

# Create log directories
RUN mkdir -p /var/log/shiny-server && \
    chown -R shiny:shiny /var/log/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
