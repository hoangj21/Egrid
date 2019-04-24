#Load in data and print out summaries 

#we ended up not using these files
# emissions = read.csv("state_output_emissions.csv",header=T, na.strings = "?")
# emissions <- na.omit(emissions)
# #fix(emissions)
# summary(emissions)
# 
# state_resource = read.csv("state_resource_mix.csv",header=T, na.strings = "?")
# state_resource <- na.omit(state_resource)
# #fix(state_resource)
# summary(state_resource)
# 
# sub_resource = read.csv("subregion_resource_mix.csv",header=T, na.strings = "?")
# sub_resource <- na.omit(sub_resource)
# #fix(sub_resource)
# summary(sub_resource)
# 
# egrid = read.csv("egrid_summary.csv",header=T, na.strings = "?")
# egrid <- na.omit(egrid)
# #fix(egrid)
# summary(egrid)

state = read.csv("state.csv",header=T, na.strings = "?")
state <- na.omit(state)
#fix(state)
summary(state)

#Removing commas from data
#Did this manually because had issues when I tried running through w/ a loop
state$CO2 <- gsub(",", "", state$CO2)
state$CH4 <- gsub(",", "", state$CH4)
state$N2O<- gsub(",", "", state$N2O)
state$CO2e <- gsub(",", "", state$CO2e)
state$Annual.NOx <- gsub(",", "", state$Annual.NOx)
state$Ozone.Season.NOx <- gsub(",", "", state$Ozone.Season.NOx)
state$SO2 <- gsub(",", "", state$SO2)
state$State.Income <- gsub(",", "", state$State.Income)
state$Nameplate.Capacity..MW. <- gsub(",", "", state$State.Income)
#state$Net.Generation..MWh. <- gsub(",", "", state$Net.Generation..MWh.)

#changing emmissions to numeric form so that R functions work properly
state$CO2 <- as.numeric(state$CO2)
state$N2O <- as.numeric(state$N2O)
state$CO2e <- as.numeric(state$CO2e)
state$Annual.NOx <- as.numeric(state$Annual.NOx)
state$Ozone.Season.NOx <- as.numeric(state$Ozone.Season.NOx)
state$SO2 <- as.numeric(state$SO2)
state$State.Income <- as.numeric(state$State.Income)
state$CH4 <- as.numeric(state$CH4)
state$Nameplate.Capacity..MW. <- as.numeric(state$Nameplate.Capacity..MW.)
state$Net.Generation..MWh. <- as.numeric(state$Net.Generation..MWh.)


