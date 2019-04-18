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

#Correlations between all pairs of energy sources and emmissions
cat(sprintf("Correlations\n"))
for(i in 1:energyLen){
  for(j in 1:emmissionsLen){
    correlation <- cor(energy[,i],emmissions[,j])
    cat(sprintf("%s and %s: %s \n", energy.lab[i],emmissions.lab[j], correlation))
  }
  cat(sprintf("\n"))
}

#Choropleth test with states and CO2 data 
library(maps)
library(magrittr)
library(leaflet)
library(geojsonio)
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))
mapStates = map("state", fill = TRUE, plot = FALSE)
m<-leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(25, alpha = NULL), stroke = FALSE)


# From http://leafletjs.com/examples/choropleth/us-states.js
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

bins <- c(0, 250, 500, 750, 1000, 1250, 1500, 1750,2000,Inf)
pal <- colorBin("Reds", domain = state$CO2, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g lb / MWH<sup>2</sup>",
  state$State.FullName, state$CO2
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

print(m)
