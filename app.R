library(shiny)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(leaflet)
library(tidyverse)
library(leaflet.extras)

# load data
df <- read.csv("comparison.csv")
df$FA_TA_PCT <- round(df$FA_TA_PCT*100, 2)
df <- rename(df, PERCENTAGE=FA_TA_PCT)
# load data
df2 <- read.csv("i3.csv")

# UI
ui <- fluidPage(
  
  # design a background
  setBackgroundImage(src = "bg.png", shinydashboard = FALSE), 
  # theme
  theme = shinytheme("readable"),

  navbarPage(
    # Application title
    strong(""), id="main", 

    # tab panel 1
    tabPanel("COMPARISON", 
             sidebarLayout(
               
               # description
               sidebarPanel(

                 p("Which is more dangerous in the first chart, urban roads or rural roads?", 
                   style = "font-family: 'Arial'; font-size: 16px; color:black"), 
                 br(),
                 p(strong("However, is this really the case?"), 
                   style = "font-family: 'Arial'; font-size: 16px; color:black"), 
                 br(), 
                 p("Consider this question again after observing the second chart.", 
                   style = "font-family: 'Arial'; font-size: 16px; color:black"), 
                 br(), 
                 p("Tips: Hover your mouse on the graphs for more information.", 
                   style = "font-family: 'Arial'; font-size: 16px; color:grey"),
                 br(),
                 img(src = "Ararat_Road.jpg", width = "100%")
                 ), 
               
               # charts
               mainPanel(highchartOutput(outputId="line_graph_1", height = 360), 
                         highchartOutput(outputId="line_graph_2", height = 360))
               )
             ), 

    
    # tab panel 2
    tabPanel("RURAL ROAD ACCIDENT DISTRIBUTIONS", 
             sidebarLayout(
               
               # description
               sidebarPanel(
                 
                 # set a selection box
                 checkboxGroupInput(inputId = 'SEVERITY', 
                                    label = img(src = "d1.png", width = "46%"),  
                                    choices = c('Only Display Fatal Accidents'=1), 
                                    selected = df2$Year
                                    ),
                 
                 # set a selection box
                 pickerInput(inputId = 'Year', 
                             label = 'Select A Year:', 
                             choices = c("2020"="2020", "2019"="2019", "2018"="2018", 
                                         "2017"="2017", "2016"="2016", "2015"="2015", 
                                         "2014"="2014", "2013"="2013", "2012"="2012", "2011"="2011")
                             ), 
                 p("Tips 1: The number in circles on the map indicates the number of 
                   (fatal) accidents in a certain area. Click them for more information.", 
                   style = "font-family: 'Arial'; font-size: 16px; color:grey"),
                 p("Tips 2: Click the reset button if you get lost in the map.", 
                   style = "font-family: 'Arial'; font-size: 16px; color:grey"), 
                 p("Tips 3: Click the real-time positioning button if you want to see the traffic 
                   accident situation in the nearby area. Please remember to close it after using it.", 
                   style = "font-family: 'Arial'; font-size: 16px; color:grey"), 
                 p("Tips 4: Click the search button and input a certain road or an certain area if you 
                   want to know their accidents information.", br(), "The recommended format 1: 
                     Road, Area (or Victoria)", br(), "The recommended format 2: Area, Victoria", 
                   style = "font-family: 'Arial'; font-size: 16px; color:grey", 
                   style = "font-family: 'Arial'; font-size: 16px; color:grey"), 
                 img(src = "hume.png", width = "100%")
                 ), 
               
               # charts
               # 'tags$style' is for higher height of the map, 'width' is width of the map
               mainPanel(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), 
                         mainPanel(leafletOutput("map"), width="100%"))
               )
             )

    )
  )


server <- function(input, output, session) {
  
  # Map
  output$map <- renderLeaflet({
    
    if(length(input$SEVERITY) > 0)
      sdata <- df2[df2$Year==input$Year & df2$SEVERITY==input$SEVERITY, ]
    else
      sdata <- df2[df2$Year==input$Year, ]
    
    # set a series of color
    pal_cluster <- colorFactor(c("limegreen", "gold2", "darkorange1"), 
                       domain = c("0 - 10", "10 - 100", "100 +"))
    
    pal <- colorFactor(c("black", "steelblue", "purple", "red"), 
                       domain = c("Fatal", "Non Injury", "Other Injury", "Serious Injury"))
    
    map <- sdata %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(
        radius = ~ifelse(sdata$SEVERITY == 1, 12, 8),
        color = ~pal(sdata$Severity),
        fillOpacity = 0.8,
        ~Longitude, 
        ~Latitude, 
        # generally, use: popup = ~as.character(Accident_No)
        popup = paste('<font color=\"#3399FF\"><b>Accident No: </b></font>', 
                      sdata$Accident_No, 
                      '<br/><font color=\"#3399FF\"><b>Accident Type: </b></font>', 
                      sdata$Accident_Type, 
                      '<br/><font color=\"#3399FF\"><b>Severity: </b></font>', 
                      sdata$Severity, 
                      '<br/><font color=\"#3399FF\"><b>No. Persons Involved: </b></font>', 
                      sdata$No_Persons_Involved, 
                      '<br/><font color=\"#3399FF\"><b>No. Persons Killed: </b></font>', 
                      sdata$No_Persons_Killed, 
                      '<br/><font color=\"#3399FF\"><b>No. Serious Injuries: </b></font>', 
                      sdata$No_Serious_Injuries, 
                      '<br/><font color=\"#3399FF\"><b>Light Condition: </b></font>', 
                      sdata$Light_Condition), 
        label =  ~as.character(Roads), 
        clusterOptions = markerClusterOptions(), 
        labelOptions = labelOptions(noHide = T, direction = "top")
        ) %>% 
      addLegend(title = "Accidents Clusters", pal = pal_cluster, 
                values = ~c("0 - 10", "10 - 100", "100 +"), opacity = 0.7) %>% 
      addLegend(title = "Accidents Severity", pal = pal, opacity = 0.7, 
                values = ~c("Fatal", "Non Injury", "Other Injury", "Serious Injury")) %>% 
      addResetMapButton() %>% 

      addSearchOSM(options = searchOptions(position = "topleft", minLength = 2, 
                                           moveToLocation = TRUE, zoom = 10, 
                                           autoResize=TRUE, autoCollapse = TRUE))
    })
  
  # line graph 1
  output$line_graph_1 <- renderHighchart({
    df %>% 
      # plot line graph
      hchart('spline', hcaes(x = 'YEAR', y = 'COUNT', group = "R_U")) %>% 
      # customize interactive line chart
      # title
      hc_title(text = "Comparison of the Total Number of Accidents", margin = 20, 
               align = "left", style = list(color = "steelblue")) %>% 
      # subtitle
      hc_subtitle(text = "Recent 10 Years", align = "left", 
                  style = list(color = "darkorange", fontWeight = "bold")) %>% 
      # location of the legend
      hc_legend(align = "right", verticalAlign = "top", 
                layout = "vertical", x = 0, y = 50) %>%
      # set pop-up information
      hc_tooltip(crosshairs = TRUE, backgroundColor = "lightyellow", borderWidth = 1) %>% 
      # set lines and markers
      hc_plotOptions(spline = list(lineWidth=3, 
                                   allowPointSelect= TRUE,
                                   turboThreshold=100,
                                   cursor= 'pointer',
                                   states=list(hover=list(lineWidth=5)),
                                   marker = list(fillColor = "white",
                                                 lineWidth = 4,
                                                 lineColor = NULL))) %>% 
      hc_add_theme(hc_theme_gridlight())
    })

  # line graph 2
  output$line_graph_2 <- renderHighchart({
    df %>% 
      # plot line graph
      hchart('spline', hcaes(x = 'YEAR', y = 'PERCENTAGE', group = "R_U")) %>% 
      # customize interactive line chart
      # title
      hc_title(text = "Comparison of the Percentage (%) of Fatal Accidents", margin = 20, 
               align = "left", style = list(color = "steelblue")) %>% 
      # subtitle
      hc_subtitle(text = "Recent 10 Years", align = "left", 
                  style = list(color = "darkorange", fontWeight = "bold")) %>% 
      # location of the legend
      hc_legend(align = "right", verticalAlign = "top", 
                layout = "vertical", x = 0, y = 50) %>%
      # set pop-up information
      hc_tooltip(crosshairs = TRUE, backgroundColor = "lightyellow", borderWidth = 1) %>%
      # set lines and markers
      hc_plotOptions(spline = list(lineWidth=3, 
                                   allowPointSelect= TRUE,
                                   turboThreshold=100,
                                   cursor= 'pointer',
                                   states=list(hover=list(lineWidth=5)),
                                   marker = list(fillColor = "white",
                                                 lineWidth = 4,
                                                 lineColor = NULL))) %>% 
      hc_add_theme(hc_theme_gridlight())
    })

}


shinyApp(ui = ui, server = server)