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

# Define UI for application
ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Emissions vs. Energy Resource'),
    
    #selection menus for linear graphs
    sidebarPanel(
      paste("Single variable Regression"),
      selectInput('xcol', 'Energy Resource', list("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown"), "Coal"),
      selectInput('ycol', 'Emissions (lb/MWh)', list("CO2","CH4", "N2O", "CO2e","Annual.NOx","Ozone.Season.NOx","SO2"),"CO2"),
      paste("Multivariable Regression"),
      selectInput('xcol2', 'Energy Resource', list("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown"), "Coal"),
      paste("Barchart"),
      selectInput('ycol3', 'Energy Resource', list("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown"), "Coal"),
      width = 2
    ),
    
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    column(10, plotOutput('linearModel')),
                    column(10,  plotOutput("multlinear"))
        ),
        column (10, plotOutput("barModel")),
        column(10,  paste("CO2 Emmissions Accross the Nation"), leafletOutput("mymap"))
        
      )
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  source("Data_Preprocessing.r")
   
   output$linearModel <- renderPlot({
    
     
     #get strings of which resource or emission the user selects
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
     
     #Find indices of user's selections in the label arrays
     xIndex = match(x,energy.lab)
     yIndex = match(y,emmissions.lab)
     
     #Use indices to select correct data, and plot it
     plot(energy[,xIndex],emmissions[,yIndex],xlab=x, ylab=y,  col = "red", pch = 15)
     model=lm(emmissions[,yIndex]~energy[,xIndex]) 
     abline(model)
     title(paste("Single Variable Regression"))
   })
   
   output$multlinear <- renderPlot({
     x = input$xcol2
     
     emmissions = cbind(state$CO2, state$CH4,state$N2O, state$CO2e,state$Annual.NOx, state$Ozone.Season.NOx,state$SO2)
     #Array for Energy source data
     energy = cbind(state$Coal, state$Oil, state$Gas, state$Other.Fossil, state$Nuclear,state$Hydro, state$Biomass, state$Wind, state$Solar, state$Geo..thermal, state$Other.unknown..purchased.fuel)
     #Labels for emmissions
     emmissions.lab = c("CO2","CH4", "N2O", "CO2e","Annual.NOx","Ozone.Season.NOx","SO2")
     #Labels for energy sources
     energy.lab = c("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown")
     xIndex = match(x,energy.lab)
     
     plot(energy[,xIndex],emmissions[,1]+emmissions[,2]+emmissions[,3],xlab=x, ylab="CO2, CH4, N2O", col = "red", pch = 15)
     model=lm(emmissions[,1]~energy[,1]+energy[,2]+energy[,3]) 
     abline(model)
     title(paste("Multiple Metric Regression"))
   })
   
   output$mymap <- renderLeaflet({
     # From http://leafletjs.com/examples/choropleth/us-states.js
     
     #get json file for US map
     states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
     
     #determine the range for the bins 
     bins <- c(0, 250, 500, 750, 1000, 1250, 1500, 1750,2000,Inf)
     
     #Set color palette 
     pal <- colorBin("Reds", domain = state$CO2, bins = bins)
     
     #state and emissions labels
     labels <- sprintf(
       "<strong>%s</strong><br/>%g lb / MWH",
       state$State, state$CO2
     ) %>% lapply(htmltools::HTML)
     
     #leaflet that draws map 
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
   
   output$barModel <- renderPlot({
     
     energy = cbind(state$Coal, state$Oil, state$Gas, state$Other.Fossil, state$Nuclear,state$Hydro, state$Biomass, state$Wind, state$Solar, state$Geo..thermal, state$Other.unknown..purchased.fuel)
     energy.lab = c("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown")
     y = input$ycol3 
     yIndex = match(y,energy.lab)
     #bargraph model for coal vs states to help with showcasing the choropleth graph
     library(ggplot2)
     #frame the states names and the col number
     stateframe <- data.frame(State = state$State, Resource = energy[,yIndex])
     #plot the state names andcol numebrs into bar graph
     plot <- ggplot(stateframe, aes(State, Resource))
     plot +geom_bar(stat = "identity", width=1, color = "black", fill = "#ea6c62") + theme_bw() +
       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +ggtitle("Energy Resource Percentage by State")+ylab(y)
     
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

