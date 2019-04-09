#Load in data and print out summaries 
emissions = read.csv("state_output_emissions.csv",header=T, na.strings = "?")
emissions <- na.omit(emissions)
fix(emissions)
summary(emissions)

state_resource = read.csv("state_resource_mix.csv",header=T, na.strings = "?")
state_resource <- na.omit(state_resource)
fix(state_resource)
summary(state_resource)

sub_resource = read.csv("subregion_resource_mix.csv",header=T, na.strings = "?")
sub_resource <- na.omit(sub_resource)
fix(sub_resource)
summary(sub_resource)

egrid = read.csv("egrid_summary.csv",header=T, na.strings = "?")
egrid <- na.omit(egrid)
fix(egrid)
summary(egrid)

state = read.csv("state.csv",header=T, na.strings = "?")
state <- na.omit(state)
fix(state)
summary(state)

#Removing commas from data
state$CO2 <- gsub(",", "", state$CO2)
state$CH4 <- gsub(",", "", state$CH4)
state$N2O<- gsub(",", "", state$N2O)
state$CO2e <- gsub(",", "", state$CO2e)
state$Annual.NOx <- gsub(",", "", state.Annual.NOx)
state$Ozone.Season.NOx <- gsub(",", "", state$Ozone.Season.NOx)
state$SO2 <- gsub(",", "", state$SO2)

#changing emmissions to numeric form so that R functions work properly
state$CO2 <- as.numeric(state$CO2)
state$CH4 <- as.numeric(state$CH4)
state$N2O <- as.numeric(state$N2O)
state$CO2e <- as.numeric(state$CO2e)
state$Annual.NOx <- as.numeric(state$Annual.NOx)
state$Ozone.Season.NOx <- as.numeric(state$Ozone.Season.NOx)
state$SO2 <- as.numeric(state$SO2)

#viewing correlations between various variables
cor(state$CO2,state$Oil)
cor(state$CO2,state$Coal)
cor(state$CO2,state$Gas)
cor(state$CO2,state$Other.Fossil)
cor(state$CO2,state$Nuclear)
cor(state$CO2,state$Biomass)
cor(state$CO2,state$Hydro)
cor(state$CO2,state$Geo..thermal)
cor(state$CO2,state$Solar)

#attempted for loops that 
emmissions = c(state$CO2, state$CH4,state$N2O, state$CO2e,state$Annual.NOx, state$Ozone.Season.NOx,state$SO2)
energy = c(state$Coal, state$Oil, state$Gas, state$Gas, state$Other.Fossil, state$Nuclear,state$Hydro, state$Biomass, state$Wind, state$Solar, state$Geo..thermal, state$Other.unknown..purchased.fuel)
for(i in emmissions){
  for(j in energy){
    cor(i,j)
  }
  
}
for(i in 1:length(emmissions)){
  for(j in 1:length(energy)){
    print(energy[j])
  }
  print(emmissions[i])
}


#had issues creating linear model, processing to remove some stray commas in data
state$CO2 <- gsub(",", "", state$CO2)   # remove comma
par(mfrow=c(2,2))
model=lm(CO2~Coal, data=state) 
plot(model)
plot(state$CO2, state$Coal)

#covariance,histograms, and means
cov(state$CO2, state$Coal)
hist(state$CO2)
hist(state$Coal)
mean(state$CO2)
mean(state$Coal)



