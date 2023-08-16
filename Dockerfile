FROM rocker/rstudio:4.3.1
ENV SHINY_SERVER_VERSION 1.5.20.1002
RUN /rocker_scripts/install_shiny_server.sh

COPY . /srv/shiny-server/

WORKDIR /srv/shiny-server/
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore()"

USER shiny

EXPOSE 3838