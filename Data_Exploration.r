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