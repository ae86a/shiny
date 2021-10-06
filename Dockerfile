FROM rocker/shiny:4.1.1
RUN install2.r rsconnect tidyverse shinythemes shinyWidgets highcharter leaflet leaflet.extras
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
COPY comparison.csv comparison.csv
COPY i3.csv i3.csv
COPY www/Ararat_Road.jpg www/Ararat_Road.jpg
COPY www/d1.png www/d1.png
COPY www/hume.png www/hume.png
CMD Rscript deploy.R
