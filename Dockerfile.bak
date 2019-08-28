FROM rocker/shiny:latest

MAINTAINER Sasa Bogdanovic "sasha@rubyind.com"

RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    libgdal-dev \
    libproj-dev \
    libssl-dev \
    git \
    libxml2-dev \
    libudunits2-dev \
    libjq-dev \
    libprotobuf-dev \
    libv8-3.14-dev \
    protobuf-compiler

# Download and install shiny server
RUN R -e "install.packages(c('here', 'shiny', 'sf', 'here', 'readr', 'stringr', 'magrittr', \
    'rlang', 'rgdal', 'leaflet', 'maps', 'tidyverse', 'dplyr', 'ggplot2', 'htmlwidgets', \
    'DT', 'rgeos', 'tidyr', 'crosstalk', 'plotly', 'devtools', 'shinyjs', 'feather', 'stringr', \
    'htmltools', 'sunburstR', 'treemap', 'data.tree', 'RColorBrewer', 'shinycssloaders', \
    'rmapshaper', 'shinyBS'), repos='https://cran.rstudio.com/'))" && \
    devtools::install_github('mountainmath/cancensus') && \
    devtools::install_github('gluc/data.tree') && \
    cd /opt/shiny-server/samples/sample-apps/ && \
    git clone https://github.com/bcgov/housing-data-challenge-ruby.git && \
    ln -s /opt/shiny-server/samples/sample-apps/housing-data-challenge-ruby /srv/shiny-server/housing \
    sudo echo "0 9 * * * Rscript /srv/shiny-server/housing/download_ptt.R" >> /etc/crontab

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
