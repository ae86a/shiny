FROM rocker/shiny:4.1.1
RUN install2.r rsconnect tidyverse shinythemes shinyWidgets highcharter leaflet leaflet.extras
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
COPY AC111_v2.csv AC111_v2.csv
COPY AC112.csv AC112.csv
COPY fatal_count.csv fatal_count.csv
COPY i3.csv i3.csv
CMD Rscript deploy.R
