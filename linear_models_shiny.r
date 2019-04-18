#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(magrittr)
library(leaflet)
library(geojsonio)

# Define UI for application that draws a histogram
ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Emissions vs. Energy Resource'),
    
    sidebarPanel(
      selectInput('xcol', 'Energy Resource', list("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown"), "Coal"),
      selectInput('ycol', 'Emissions (lb/MWh)', list("CO2","CH4", "N2O", "CO2e","Annual.NOx","Ozone.Season.NOx","SO2"),"CO2")
    ),
    
    mainPanel(
      fluidRow(
        column(10, plotOutput('distPlot')),
        column(10,  leafletOutput("mymap"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     source("Data_Preprocessing.r")
     x = input$xcol
     y = input$ycol
     
     
     #Array of emmissions data
     emmissions = cbind(state$CO2, state$CH4,state$N2O, state$CO2e,state$Annual.NOx, state$Ozone.Season.NOx,state$SO2)
     #Array for Energy source data
     energy = cbind(state$Coal, state$Oil, state$Gas, state$Other.Fossil, state$Nuclear,state$Hydro, state$Biomass, state$Wind, state$Solar, state$Geo..thermal, state$Other.unknown..purchased.fuel)
     #Labels for emmissions
     emmissions.lab = c("CO2","CH4", "N2O", "CO2e","Annual.NOx","Ozone.Season.NOx","SO2")
     #Labels for energy sources
     energy.lab = c("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown")
     
     xIndex = match(x,energy.lab)
     yIndex = match(y,emmissions.lab)
     
     plot(energy[,xIndex],emmissions[,yIndex],xlab=x, ylab=y,  col = "blue", pch = 15)
     model=lm(emmissions[,yIndex]~energy[,xIndex]) 
     abline(model)
     
   })
   
   output$mymap <- renderLeaflet({
     # From http://leafletjs.com/examples/choropleth/us-states.js
     states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
     
     print(states$name)
     bins <- c(0, 250, 500, 750, 1000, 1250, 1500, 1750,2000,Inf)
     pal <- colorBin("Reds", domain = state$CO2, bins = bins)
     
     labels <- sprintf(
       "<strong>%s</strong><br/>%g lb / MWH",
       state$State, state$CO2
     ) %>% lapply(htmltools::HTML)
     
     m<- leaflet(states) %>%
       setView(-96, 37.8, 4) %>%
       addProviderTiles("MapBox", options = providerTileOptions(
         id = "mapbox.light",
         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
       addPolygons(
         fillColor = ~pal(state$CO2),
         weight = 2,
         opacity = 1,
         color = "white",
         dashArray = "3",
         fillOpacity = 0.7,
         highlight = highlightOptions(
           weight = 5,
           color = "#666",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE),
         label = labels,
         labelOptions = labelOptions(
           style = list("font-weight" = "normal", padding = "3px 8px"),
           textsize = "15px",
           direction = "auto")) %>%
       addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                 position = "bottomright")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

