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

#had issues creating linear model, processing to remove some stray commas in data
state$CO2 <- gsub(",", "", state$CO2)   # remove comma
state$CO2 <- as.numeric(state$CO2)  
par(mfrow=c(2,2))
model=lm(CO2~Coal, data=state) 
plot(model)

#covariance,histograms, and means
cov(state$CO2, state$Coal)
hist(state$CO2)
hist(state$Coal)
mean(state$CO2)
mean(state$Coal)



