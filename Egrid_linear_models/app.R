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
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     source("Data_Preprocessing.r")
     
     #Array of emmissions data
     emmissions = cbind(state$CO2, state$CH4,state$N2O, state$CO2e,state$Annual.NOx, state$Ozone.Season.NOx,state$SO2)
     #Array for Energy source data
     energy = cbind(state$Coal, state$Oil, state$Gas, state$Other.Fossil, state$Nuclear,state$Hydro, state$Biomass, state$Wind, state$Solar, state$Geo..thermal, state$Other.unknown..purchased.fuel)
     #Labels for emmissions
     emmissions.lab = c("CO2","CH4", "N2O", "CO2e","Annual.NOx","Ozone.Season.NOx","SO2")
     #Labels for energy sources
     energy.lab = c("Coal", "Oil", "Gas", "Other.Fossil", "Nuclear", "Hydro", "Biomass", "Wind","Solar","Geothermal","Unknown")
     
     
     #lengths if emmissions/energy arrays
     emmissionsLen <-7
     energyLen <- 11 
     
     #Scatterplots and linear models for all pairs of energy sources and emmissions
     for(i in 1:energyLen){
       for(j in 1:emmissionsLen){
         plot(energy[,i],emmissions[,j],xlab=energy.lab[i], ylab=emmissions.lab[j])
         model=lm(emmissions[,j]~energy[,i]) 
         abline(model)
       }
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

