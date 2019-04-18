#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Emissions vs. Energy Resource'),
    
    sidebarPanel(
      selectInput('xcol', 'Energy Resource', list("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown"), "Coal"),
      selectInput('ycol', 'Emissions (lb/MWh)', list("CO2","CH4", "N2O", "CO2e","Annual.NOx","Ozone.Season.NOx","SO2"),"CO2")
    ),
    
    mainPanel(
      plotOutput("distPlot")
      
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
}

# Run the application 
shinyApp(ui = ui, server = server)

