emissions = read.csv("state_output_emissions.csv",header=T, na.strings = "?")
emissions <- na.omit(emissions)
fix(emissions)
summary(emissions)
