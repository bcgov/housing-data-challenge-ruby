FROM rocker/shiny:latest

MAINTAINER Sasa Bogdanovic "sasha@rubyind.com"

RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    libgdal-dev \
    libproj-dev \
    libssl-dev \
    git

# Download and install shiny server
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'rgdal', 'leaflet', 'maps', \
    'DT', 'dplyr', 'rgeos', 'tidyr', 'crosstalk', 'plotly'), \
    repos='https://cran.rstudio.com/')" && \
    cd /opt/shiny-server/samples/sample-apps/ && \
    git clone https://github.com/bcgov/housing-data-challenge-ruby.git && \
    ln -s /opt/shiny-server/samples/sample-apps/housing-data-challenge-ruby /srv/shiny-server/housing \
    sudo echo "0 9 * * * Rscript /srv/shiny-server/housing/download_ptt.R" >> /etc/crontab

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]