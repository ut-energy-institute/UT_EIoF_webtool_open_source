## Author: Carey W. King, 2020. Unviersity of Texas at Austin, Energy Institute.
##
## File for determining the amount of power plant capacity needed to satisfy hourly load
## given a user's input of the percentage of electricity generation from each major energy
## source or technology. 
## This code was derived from previous codes named "UserEnergy_to_Capacity" but is 
## shortened to "solveGEN".

## ++++++++++++++++++++++
## UserEnergy_to_Capacity_20190208V3: This version has a "full functionality" of calculating when each power plant
## is dispatched to meet the user-specified fraction of generation from each type of power plant.
## It assumes each power plant can be turned on and off at will with no cost to do so.
## It assumes each power plant can ramp up and down at any rate needed.
##
## UserEnergy_to_Capacity_20190208V4: In picking the order to determine the capacity of each non-Natural Gas
## dispatchable power plant, V4 assumes this order is based on only the variable
## cost of the power plant.  V1-V3 assume this order is based on the total cost
## to operate the power plant at full capacity factor (e.g., the plant that is 
## lowest cost at near "baseload" conditions.)
##
## UserEnergy_to_Capacity_20190208v5: This version makes the assumption that nuclear generation DOES NOT ramp up and down.
## To do this, wind and solar are curtailed to enable the targeted amount of MWh of nuclear generation
## to exist on the grid at full capacity each hour of the year.
## This is a non-trivial change in terms of calculating the order of items within the code.
## In essence, in V4 and earlier, "nuclear" is a "dispatchable" technology.
## In essence, in V5, "nuclear" is now a "nondispatchable" technology.
##
## UserEnergy_to_Capacity_20190214v1 = UserEnergy_to_Capacity_20190208v5
##
## UserEnergy_to_Capacity_20190214v2: 
## Adds new generators to the possible mix and uses inputs that limit the i) total 
## capacity (MW), or ii) total annual energy (MWh/yr) that can exist for a given type of
## generation for a given region.
## 1. "Biomass": biomass power (assumed dispatchable; limited by MWh/yr based on biomass resource)
## 2. "Geothermal": geothermal power (assumed dispatchable; capacity installed limited to resource assessments)
## 3. Hydropower (assumed partially non-dispatchable and partially dispatchable per how it is treated in NREL ReEDS model; MWh is limited for a given season/day to incorporate water storage limts; MW capacity is limited per resource sites)
##    "HydroDispatch": has some limited amount of energy (MWh) to dispatch each month (can be based on seasonal energy allowed to dispatch)
##    "HydroNonDispatch": has some limited constant power (MW) output each hour specified each month
## 4. "PetroleumCC": petroleum combined cycle  (assumed dispatchable; no maximum MWh/yr or MW capacity)
## 5. "CSP": concentrating solar power (assumed non-dispatchable; MW limited by resource assessment)
##
## UserEnergy_to_Capacity_20190214v3: 
## Unlike "UserEnergy_to_Capacity_20190214v2" which based hydropower calculations on monthly
## data for hydropower energy budgets (e.g., how much hydro MWh can be generated each month),
## this code mimics the method of the NREL ReEDS model by using four seasons to define the hydro
## MWh budget. These are Winter, Spring, Summer, and Fall, and each have a defined set of months:
## Winter (Nov/Dec/Jan/Feb) = 2880 hours, Spring (Mar/Apr/May) = 2208 hours, 
## Summer (June/July/Aug) = 2208 hours, Fall (Sept/Oct) = 1464 hours.
## Because this code starts on January 1, we split Winter into two parts for the calculations as
## Winter_Jan/Feb and Winter_Nov/Dec.
## In this "v3", there is still NO ABILITY TO ADD NEW HYDRO CAPACITY, you can only use existing hydro capacity. 
## 
## UserEnergy_to_Capacity_20190312v1: 
## This version incorporates a version of electricity storage that only addresses DAILY (or day-ahead dispatch)
## net load and generaton from "wind + solar".
## This version also made an update to the sub-function "function_solve_hydro_dispatch" to enable it
## to also be used for dispatching daily electricity stored from wind and solar. Part of the adjustment was to 
## enable it to dispatch low quantities of MWh of storage over multiple hours but for which each hour
## has lower MW dispatch than the installed output capacity (MW) of the storage system. This is done
## using the new sub-sub-function (a function used within "function_solve_hydro_dispatch") called "function_Wind_PV_CSP_daily_storage_tiny".
## THE SECTION THAT RUNS THIS NEW PART OF THE CODE STARTS WITH:
## ## +++++++++++++++++++++++++
## ## Subroutine for storage dispatch - BEGIN
## ## +++++++++++++++++++++++++
## AND IS CALLED WITH THE LINE:
## wind_solar_storage_multipliers <- Rcgmin(fn=function_Wind_PV_CSP_daily_storage,lower=lb,par=init_guesses,control=list(dowarn=FALSE,maxit=max_iters))
## UserEnergy_to_Capacity_20190312v2 .. TBD : 
## This version incorporates a version of electricity storage that only addresses SEASONAL (or long-term storage for dispatch in another season)
## net load and generaton from "wind + solar".
## 
## UserEnergy_to_Capacity_20190312v2A: 
## This version removes "SOC_initial" from the parameters to be optimized in "function_Wind_PV_CSP_annual_storage"
## 
## UserEnergy_to_Capacity_20190312v2B : 
## This version solves for ANNUAL storage of Wind, PV, and CSP but does NOT try to 
## optimize capital costs of "storage+PV+CSP+Wind". It just assumes you are going to build 
## the storage system (by MW and MWh sizing) required to dispatch 100% of stored Wind, PV, and CSP.
## 
## UserEnergy_to_Capacity_20190312v3C: 
## This version changes this line in "function_solve_hydro_dispatch":
## hr_end_area2_vector <- which(net_load_duration_input$MW<(net_load_duration_input$MW[hr_transition] - NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]))
## to this line:
## hr_end_area2_vector <- which(net_load_duration_input$MW<(net_load_duration_input$MW[hr_transition] - max_capacity))
## Where "max_capacity" in the function is assumed to be the MW capacity of the storage system (hydro or otherwise that uses this function).
## This version also makes a crucial modificition to "function_solve_hydro_dispatch" that allows it to solve
## for storage dispatch when "area1" = 0. This is necessary when the storage system cannot discharge at its maximum capacity (in MW)
## for even one hour (this is why "area1" = 0).
##
## UserEnergy_to_Capacity_20190403v1: 
## This version uses a different optimization package (now "optim") and algorithm as compared to "UserEnergy_to_Capacity_20190312v3C" (which used "Rcgmin").
## This is faster as "optim" chooses to use a non-gradient-based optimization algorithm.
## 
## ++++++++++++++++++++++

#solveGEN <- function(RegionNumber,year, coal_percent, PV_percent, CSP_percent, wind_percent, nuclear_percent, hydro_percent, biomass_percent, geothermal_percent, petroleum_percent){
solveGEN <- function(RegionNumber,year, coal_percent, PV_percent, CSP_percent, wind_percent, nuclear_percent, hydro_percent, biomass_percent, geothermal_percent, petroleum_percent,Total_Hourly_MW_8760_CurrentRegion){

## This code actually uses fractions (between 0 and 1, not percentages) of total electricity generated by each type of power plant.
## But the data are input by the website user as percentages (between 0 and 100), so here we make the conversion.
coal_fraction = coal_percent / 100
PV_fraction = PV_percent / 100
CSP_fraction = CSP_percent / 100
wind_fraction = wind_percent / 100
biomass_fraction = biomass_percent / 100
hydro_fraction = hydro_percent / 100
petroleum_fraction = petroleum_percent / 100
nuclear_fraction = nuclear_percent / 100
geothermal_fraction = geothermal_percent / 100

## +++++++++
## Read data and set initial parameters
## +++++++++
if (year == 2016) {
  load("solveGen_data/renewables_2016profiles_8760.Rdata")
  } else {
  stop("Error: You chose to use an incompatible 'year' for weather and renewable profiles to use as a basis of analysis.")
}

## ++++++
## Data indicating were renewables (wind, PV, and CSP) can be built yet serve load in another region
## ++++++

load("solveGen_data/Tranfer_RegionFromTo.Rdata")

## ++++++
## Load data for land area per EIoF region
## ++++++
load("solveGen_data/land_area_ByEIoF.Rdata")
acre_perMW_CSP_totalarea <- 9.0  ## assumed number of acres of "Total Area" for CSP Power Tower configurations 
acre_perMW_CSP_direct <- 8.0  ## assumed number of acres of "Direct Area" for CSP Power Tower configurations 
acre_perMW_PV_totalarea <- 5.4  ## assumed number of acres of "Total Area" for solar PV 
acre_perMW_PV_direct <- 4.9  ## assumed number of acres of "Direct Area" for solar PV 
hectare_perMW_wind_direct <- 0.3  ## assumed number of hectares of "Direct Area" for wind farms 
km2_per_hectare_wind_totalarea <- 1/5  ## From Hand et al. (2012) Table A-10 (NREL Renewable Futures Study)
hectare_per_acre <- 0.404686
acre_per_km2 <- 247.105
acre_perMW_wind_direct <- hectare_perMW_wind_direct/hectare_per_acre  ## assumed number of acres of "Direct Area" for wind farms 
acre_perMW_wind_totalarea <- km2_per_hectare_wind_totalarea*acre_per_km2  ## assumed number of acres of "Total Project Area" for wind farms 

## ++++++
## Load data that houses the "capacity and cost ($/MW for spur line connection)" data from whcih we can obtain
## the maximum amount of installed capacity per EIoF region per renewable technology.
## ++++++
load("solveGen_data/ReEDS_CapacityCostCurves_ByEIoFRegion.rdata")
## Create data frame with the cumulative maximum installed capacity per technology
regions <- c('NW','CA','MN','SW','CE','TX','MW','AL','MA','SE','FL','NY','NE')  ## EIoF regions
MaxCapacity_perRegion_perTech_MW <- data.frame(rep(0,13),rep(0,13),rep(0,13),rep(0,13),rep(0,13))
names(MaxCapacity_perRegion_perTech_MW) <- c("Wind_total","PV","CSP","Biomass","Geothermal")
row.names(MaxCapacity_perRegion_perTech_MW) <- regions
for (i in 1:length(regions)) {
  MaxCapacity_perRegion_perTech_MW$Wind_total[i] <- sum(wind_data_onshore_EIoF_List[[i]]$capacity_MW) + sum(wind_data_offshore_EIoF_List[[i]]$capacity_MW)
  MaxCapacity_perRegion_perTech_MW$PV[i] <- sum(upv_data_all_EIoF_List[[i]]$capacity_MW)
  if (i!=12 & i!=13) { ## Some EIoF reigons (Regions 12=NY and 13=NE) have 0 MW of capacity potential for CSP
    MaxCapacity_perRegion_perTech_MW$CSP[i] <- sum(csp_data_all_EIoF_List[[i]]$capacity_MW)
  } else {
    MaxCapacity_perRegion_perTech_MW$CSP[i] = 0
  }
}


## Now find maximum allowed installed capacity, across ALL REGIONS including those that can import PV, Wind, and CSP into RegionNumber
## Max total capacity for Wind
binding_regions <- 0*Tranfer_RegionFromTo_Wind
indices.temp <- which(Tranfer_RegionFromTo_Wind[,RegionNumber]>0)
regions_with_Wind <- indices.temp
for (i in 1:length(indices.temp)) {  ## rows of "binding_regions" are "i"
  for (j in 1:(length(indices.temp))) { ## columns of "binding_regions" are "j"
    ## binding_regions[i,j] <- amount of capacity that would be in region "j" (column) if the max capacity in region "i" (row) were binding
    binding_regions[indices.temp[i],indices.temp[j]] <- MaxCapacity_perRegion_perTech_MW$Wind_total[indices.temp[i]]*Tranfer_RegionFromTo_Wind[indices.temp[j],RegionNumber]/Tranfer_RegionFromTo_Wind[indices.temp[i],RegionNumber]
  } ## for (j in 1:length(indices.temp))
} ## for (i in 1:length(indices.temp))
constrained_capacity_NoStorage.Wind <- rowSums(binding_regions)  ## sum every column of values for each given row
if ( max(constrained_capacity_NoStorage.Wind)==0 ) {
  constrained_capacity_NoStorage.Wind <- 0.001  ## make this just > 0 since I need an upper bound ("ub") that is > 0 for the "optim" algorithms later
} else {
  constrained_capacity_NoStorage.Wind <- min(constrained_capacity_NoStorage.Wind[constrained_capacity_NoStorage.Wind>0])  ## Find the minimum sum (that is not 0) of capacity that can be installed in all regions that can contribute to RegionNumber
}
rm(indices.temp,binding_regions)

## Max total capacity for PV
binding_regions <- 0*Tranfer_RegionFromTo_PV
indices.temp <- which(Tranfer_RegionFromTo_PV[,RegionNumber]>0)
regions_with_PV <- indices.temp
for (i in 1:length(indices.temp)) {  ## rows of "binding_regions" are "i"
  for (j in 1:(length(indices.temp))) { ## columns of "binding_regions" are "j"
    ## binding_regions[i,j] <- amount of capacity that would be in region "j" (column) if the max capacity in region "i" (row) were binding
    binding_regions[indices.temp[i],indices.temp[j]] <- MaxCapacity_perRegion_perTech_MW$PV[indices.temp[i]]*Tranfer_RegionFromTo_PV[indices.temp[j],RegionNumber]/Tranfer_RegionFromTo_PV[indices.temp[i],RegionNumber]
  } ## for (j in 1:length(indices.temp))
} ## for (i in 1:length(indices.temp))
constrained_capacity_NoStorage.PV <- rowSums(binding_regions)  ## sum every column of values for each given row
if ( max(constrained_capacity_NoStorage.PV)==0 ) {
  constrained_capacity_NoStorage.PV <- 0.001  ## make this just > 0 since I need an upper bound ("ub") that is > 0 for the "optim" algorithms later
} else {
  constrained_capacity_NoStorage.PV <- min(constrained_capacity_NoStorage.PV[constrained_capacity_NoStorage.PV>0])  ## Find the minimum sum (that is not 0) of capacity that can be installed in all regions that can contribute to RegionNumber
}
rm(indices.temp,binding_regions)
## Max total capacity for CSP
binding_regions <- 0*Tranfer_RegionFromTo_CSP
indices.temp <- which(Tranfer_RegionFromTo_CSP[,RegionNumber]>0)
regions_with_CSP <- indices.temp
for (i in 1:length(indices.temp)) {  ## rows of "binding_regions" are "i"
  for (j in 1:(length(indices.temp))) { ## columns of "binding_regions" are "j"
    ## binding_regions[i,j] <- amount of capacity that would be in region "j" (column) if the max capacity in region "i" (row) were binding
    binding_regions[indices.temp[i],indices.temp[j]] <- MaxCapacity_perRegion_perTech_MW$CSP[indices.temp[i]]*Tranfer_RegionFromTo_CSP[indices.temp[j],RegionNumber]/Tranfer_RegionFromTo_CSP[indices.temp[i],RegionNumber]
  } ## for (j in 1:length(indices.temp))
} ## for (i in 1:length(indices.temp))
constrained_capacity_NoStorage.CSP <- rowSums(binding_regions)  ## sum every column of values for each given row
if ( max(constrained_capacity_NoStorage.CSP)==0 ) {
  constrained_capacity_NoStorage.CSP <- 0.001  ## make this just > 0 since I need an upper bound ("ub") that is > 0 for the "optim" algorithms later
} else {
  constrained_capacity_NoStorage.CSP <- min(constrained_capacity_NoStorage.CSP[constrained_capacity_NoStorage.CSP>0])  ## Find the minimum sum (that is not 0) of capacity that can be installed in all regions that can contribute to RegionNumber
}
#browser()
rm(indices.temp,binding_regions)



## ++++++
## Initial guesses for optimization routines
## ++++++
MW_data_PV <- 1     ## [this is a needed initial input for internal optimization] MW of installed solar PV associated with the input 8760 MW output of the solar PV profile
MW_data_Wind <- 1   ## [this is a needed initial input for internal optimization] MW of installed wind associated with the input 8760 MW output of the wind profile
MW_data_CSP <- 1    ## [this is a needed initial input for internal optimization] MW of installed solar CSP associated with the input 8760 MW output of the solar CSP profile

## ++++++
## Cost data (that mostly set up names for internal data frames)
## Cost data do not influence the calculations of this code (except to confirm NG is )
## ++++++
load("solveGen_data/PowerPlantCosts_20200413.Rdata")

## +++++++++
## Create the main "data" data frame that has the region-specific 8760 profile for
## 1. generation needed from all generators (called "load" for legacy reasons, but this includes generation needed to overcome transmission and distribution losses)
## 2. To contribute to the load for "RegionNumber", the wind, PV, and CSP profiles are a mix of profiles from "RegionNumber" AND its neighboring regions, as defined by "Tranfer_RegionFromTo_Wind", Tranfer_RegionFromTo_PV", and "Tranfer_RegionFromTo_CSP"
## +++++++++
wind_profiles_multiplier<-matrix(rep(Tranfer_RegionFromTo_Wind[,RegionNumber],each=8760),nrow=8760)
wind_8760_profile <- wind_profiles_multiplier*wind_8760[,3:15]
PV_profiles_multiplier<-matrix(rep(Tranfer_RegionFromTo_PV[,RegionNumber],each=8760),nrow=8760)
PV_8760_profile <- PV_profiles_multiplier*PV_8760[,3:15]
CSP_profiles_multiplier<-matrix(rep(Tranfer_RegionFromTo_CSP[,RegionNumber],each=8760),nrow=8760)
CSP_8760_profile <- CSP_profiles_multiplier*CSP_8760[,3:15]

## +++++++++
## Load the 2050 hourly generation requirement from the "generate8760.R" code
## These data are an output from "generate8760.R".
## +++++++++
data=data.frame(seq(1,8760),Total_Hourly_MW_8760_CurrentRegion,rowSums(wind_8760_profile[,]),rowSums(PV_8760_profile[,]),rowSums(CSP_8760_profile[,]))
names(data)=c("Hour.ending","Load_MW","Wind_MW","SolarPV_MW","SolarCSP_MW")


## +++++++++
## User desired fractions of electricity from each power plant type
## +++++++++
# Initialize "Frac_MWhDesired" for dispatchable technologies
## +++++++++++++++
## THe assumption for user inputs for hydro is that:
## 1. The user's desired fraction for TOTAL Hydro goes into "Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))]"
## 2. Then HydroNonDispatch is solved, and any remaining MWh from hydro is the assigned to: Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("HydroDispatch"))]
## +++++++++++++++

Frac_MWhDesired_dispatchable <- df.tech_generic
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[,-c(3:dim(Frac_MWhDesired_dispatchable)[2])]
colnames(Frac_MWhDesired_dispatchable) <- c("Technology","Fraction_MWhDesired")
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[-which(Frac_MWhDesired_dispatchable$Technology==c("Wind")),]
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[-which(Frac_MWhDesired_dispatchable$Technology==c("PV")),]
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[-which(Frac_MWhDesired_dispatchable$Technology==c("Nuclear")),]
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[-which(Frac_MWhDesired_dispatchable$Technology==c("HydroNonDispatch")),]
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[-which(Frac_MWhDesired_dispatchable$Technology==c("CSP")),]
Frac_MWhDesired_dispatchable[,c(2)] <- 0
## Sort Frac_MWhDesired alphabetically by technology name (A-->Z)
Frac_MWhDesired_dispatchable <- Frac_MWhDesired_dispatchable[order(Frac_MWhDesired_dispatchable$Technology),]
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("Coal"))] <- coal_fraction  ## Coal
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("NGCC"))] <- 0  ## NGCC
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("NGCT"))] <- 0  ## NGCT
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("UserTech1"))] <- 0  ## UserTech1
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("UserTech2"))] <- 0  ## UserTech2
#Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("HydroDispatch"))] <- 0.0  ## Dispatchable portion of hydro power
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("PetroleumCC"))] <- petroleum_fraction  ## Petroleum fired power
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("Biomass"))] <- biomass_fraction  ## Biomass
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("Geothermal"))] <- geothermal_fraction  ## Geothermal

# Initialize "Frac_MWhDesired" for non-dispatchable technologies
Frac_MWhDesired_Nondispatchable <- df.tech_generic
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[,-c(3:dim(Frac_MWhDesired_Nondispatchable)[2])]
colnames(Frac_MWhDesired_Nondispatchable) <- c("Technology","Fraction_MWhDesired")
Frac_MWhDesired_Nondispatchable[,c(2)] <- 0
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("NGCC")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("NGCT")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("Coal")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("UserTech1")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("UserTech2")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroDispatch")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("PetroleumCC")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("Biomass")),]
Frac_MWhDesired_Nondispatchable <- Frac_MWhDesired_Nondispatchable[-which(Frac_MWhDesired_Nondispatchable$Technology==c("Geothermal")),]
Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("Nuclear"))] <- nuclear_fraction ## Nuclear
Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("PV"))] <- PV_fraction      ## Solar PV
Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("Wind"))] <- wind_fraction    ## Wind
Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("CSP"))] <- CSP_fraction    ## Solar CSP
Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))] <- hydro_fraction    ## Nondispatchable portio of hydropower

Frac_MWhDesired_NG_total <- 1 - sum(Frac_MWhDesired_dispatchable[,2]) - sum(Frac_MWhDesired_Nondispatchable[,2]) + Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("NGCC"))] + Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("NGCT"))]
assign('Frac_MWhDesired_dispatchable',Frac_MWhDesired_dispatchable,envir=.GlobalEnv)
assign('Frac_MWhDesired_Nondispatchable',Frac_MWhDesired_Nondispatchable,envir=.GlobalEnv)
if (Frac_MWhDesired_NG_total<0) {
  print("The inputs have specified a desired percentage of NG generation < 0%. Setting NG generation to 0%. Ensure the sum of each type of generation = 100%.")
  Frac_MWhDesired_NG_total=as.integer(0)
}

## +++++++++++++++
## Calculate the variable cost of each new power plant technology for operating 1 hour.
## Costs are calculated in total $ per MW installed per hour.
## +++++++++++++++
PPcost_Variable <- rbind(Frac_MWhDesired_dispatchable,Frac_MWhDesired_Nondispatchable)
PPcost_Variable <- PPcost_Variable[order(PPcost_Variable$Technology),]
PPcost_Variable[,2] <- 0
names(PPcost_Variable) <- c("Technology","Cost_Variable")
PPCost_US <- NewPPcost[which(NewPPcost$EIoF_region=="US"),]
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Coal"))] <-  (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("Coal"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("Coal"))])  ## Coal
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Nuclear"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("Nuclear"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("Nuclear"))])  ## Nuclear
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCC"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("NGCC"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("NGCC"))])  ## NGCC
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCT"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("NGCT"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("NGCT"))])  ## NGCT
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("UserTech1"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("UserTech1"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("UserTech1"))])  ## UserTech1
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("UserTech2"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("UserTech2"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("UserTech2"))])  ## UserTech2
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Biomass"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("Biomass"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("Biomass"))])  ## Biomass
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("HydroDispatch"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("HydroDispatch"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("HydroDispatch"))])  ## Dispatchable Hydropower
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("PetroleumCC"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("PetroleumCC"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("PetroleumCC"))])  ## Petroleum CombinedCycle
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Geothermal"))] <- (PPCost_US$VariableOMCost_..MWh[which(PPCost_US$Technology==c("Geothermal"))]+PPCost_US$VariableFuelCost_..MWh[which(PPCost_US$Technology==c("Geothermal"))])  ## Geothermal
## Determine the capacity for dispatchable generators, that ARE NOT NATURAL GAS,
## needed to meet the user-specified % of MWh for each.
## Find the order of dispatchable generators from lowest to highest cost if operating at 100% capacity factor (i.e., at PPcost$hours_operating=8760)
## Make the NGCC and NGCT very high now so they are not chosen since their capacity will be decided later.
## Also ensure that the "PPcost_Variable$Cost_Variable" for each nondispatchable generator is too high to be chosen for next steps.
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCC"))] <- 1e15*PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCC"))] 
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCT"))] <- 1e15*PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCT"))] 
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("PV"))] <- 99999e15    ## Make some ridiculously high number so when ordering PP_MWneeded by "Cost_Variable" PV is higher than all disptachable generators
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Wind"))] <- 99999e15  ## Make some ridiculously high number so when ordering PP_MWneeded by "Cost_Variable" Wind is higher than all disptachable generators
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Nuclear"))] <- 1e15*PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Nuclear"))]   ## Make some ridiculously high number so when ordering PP_MWneeded by "Cost_Variable" Nuclear is higher than all disptachable generators
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("HydroNonDispatch"))] <- 99999e15   ## Make some ridiculously high number so when ordering PP_MWneeded by "Cost_Variable" HydroNonDispatch is higher than all disptachable generators
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("CSP"))] <- 99999e15   ## Make some ridiculously high number so when ordering PP_MWneeded by "Cost_Variable" CSP is higher than all disptachable generators
PPcost_Variable <-PPcost_Variable[order(PPcost_Variable$Cost_Variable),]


## +++++++++++++++
## Merge data frames to create a data frame that keeps track of how much MW to install for each generator
## +++++++++++++++
PP_MWneeded <- rbind(Frac_MWhDesired_dispatchable,Frac_MWhDesired_Nondispatchable)  ## Inclulde the Nondispatchable Power Plants now so we can add their needed capacity value later
PP_MWneeded <- merge(PP_MWneeded,PPcost_Variable,by="Technology")
PP_MWneeded$MW_needed <- rep(0,dim(PP_MWneeded)[1])
PP_MWneeded <- PP_MWneeded[order(PP_MWneeded$Cost_Variable),]  ## Order PP_MWneeded by increasing variable costs to later solve their dispatch in order from least to highest varaible cost

## Add data columns for the hourly MW generation from each type of generator
num_cols_now <- dim(data)[2]
zeros <- rep(0,8760)
data <- cbind(data,matrix(zeros , length(zeros) , (dim(Frac_MWhDesired_dispatchable)[1]+1) ))  ## add a column for generation each hour from each disptachable generator PLUS a column for nuclear generation

## ++++++++++++
## Determine Nuclear Capacity as running at a constant capacity each hour, but with 
## capacity ONLY up to the minimum load (minimum of data$Load_MW)
## ++++++++++++
PPindex <- num_cols_now+1  ## This is the column in data.frame "data" to add the hourly generation for the current power plant type
names(data)[PPindex] <- paste0("Nuclear_MW")
nuclear_avg_CapacityFactor <- 0.95  ## a value < 1.0 implies that even if nukes run at max cacity each hour they are operating, they are down for refueling and maintenance on average some fraction of the time = (1-nuclear_avg_CapacityFactor)
PP_MWneeded_NukeTemp <- sum(data$Load_MW)/8760*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Nuclear")]
data$Nuclear_MW <- rep(1,8760)*PP_MWneeded_NukeTemp
PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")] <- PP_MWneeded_NukeTemp/nuclear_avg_CapacityFactor
if (max(data$Nuclear_MW) > min(data$Load_MW)) {  ## Then nuclear capacitiy is larger than the minimum load and the EIoF tool assumes this is not allowed
  Nuclear.MaxPct <- min(data$Load_MW)*8760/sum(data$Load_MW)
  PP_MWneeded_NukeTemp <- min(data$Load_MW)*.999
  data$Nuclear_MW <- rep(1,8760)*PP_MWneeded_NukeTemp
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")] <- PP_MWneeded_NukeTemp/nuclear_avg_CapacityFactor
  cat(paste0("Your choice leads to ",sprintf("%.1f", max(data$Nuclear_MW))," MW of nuclear.",sep="\n"))
  cat(paste0("The maximum allowed capacity for nuclear is ",sprintf("%.1f", min(data$Load_MW))," MW to achieve up to ",sprintf("%.1f", Nuclear.MaxPct*100)," % of generation.",sep="\n"))
  cat(paste0("Reduce your desired % of MWh from nuclear."))
}

## ++++++++++++
## Determine Dispatchable & NonDispatchable Hydro generation and capacity as running at a constant capacity each hour.
## In each option below, we can calculate a needed capacity and check if it is more 
## than the (1) existing hydro capacity and (2) maximum capacity based on a resource assessment.
## 
## In the end, the capacity (MW) is only for "hydro" as a combination of the concepts of
## Dispatchable & NonDispatchable Hydro. So only one of those will have a non-zero "PP_MWneeded$MW_needed"
##
## Option 1: First determine "NonDispatchable Hydro MW" and second determine "Dispatchable Hydro MW".
## NonDispatchableHydro thus becomes a constant level for each month/season. This constant level is 
## the minimum of (1) the user's desired hydro generation (annually) and (2) the maximum allowed
## by the regional constraint of the maximum NonDispatchableHydro per month/season.  One could also 
## have curtailment if the net load (after accounting for nuclear power already) becomes negative.
## For NonDispatchableHydro curtailment, this represents the need to let water flow through the 
## dams but without generating power that could otherwise be generated.  Once NonDispatchableHydro
## is determined, then DispatchableHydro is determined the same as other dispatchable power plants
## but with the constraint on the total (1) MWh available to dispatch any given month and (2) the
## maximum installed capacity allowed for that region (e.g. there is an hourly MW maximum for TOTAL hydro).
## The code is set up to solve for DispatchableHydro before any other dispatchable power plants.
## 
## Option 2: First determine "Dispatchable Hydro MW" and second determine "NonDispatchable Hydro MW"
## Here the rationale is that DispatchableHydro is the highest value use of hydro, and then NonDispatchableHydro
## is secondary in importance.
##
## Option 3: We do not allow the user to remove hydro, and we hard wire an amount of total hydro 
## that represents the current (2015-2018) amount of annual/monthly generation (MWh) and with maximum
## capacity (MW) of the existing fleet per region.
## 
## ++++++++++++

##  First check if user wants more total hydro than allowed by existing energy budget.
PPCost_CurrentRegion <- NewPPcost[which(NewPPcost$EIoF_region==regions[RegionNumber]),]
Hydro_MaxTotalAnnualMWh <- sum(PPCost_CurrentRegion$MWh_max_WinterJanFeb,na.rm = TRUE) + sum(PPCost_CurrentRegion$MWh_max_Spring,na.rm = TRUE) + sum(PPCost_CurrentRegion$MWh_max_Summer,na.rm = TRUE) + sum(PPCost_CurrentRegion$MWh_max_Fall,na.rm = TRUE) + sum(PPCost_CurrentRegion$MWh_max_WinterNovDec,na.rm = TRUE)
#if ( (sum(NewPPcost[which(NewPPcost$X=="HydroNonDispatch"),9:13])+sum(NewPPcost[which(NewPPcost$X=="HydroDispatch"),9:13]))/sum(data$Load_MW) < (Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("HydroDispatch"))] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))] ) ) {
if ( Hydro_MaxTotalAnnualMWh/sum(data$Load_MW) < (Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("HydroDispatch"))] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))] ) ) {
  NotEnoughHydro <- 1  ## Yes, there is not enough hydro energy budget to reach user's target. So use all of energy budget for hydro later in code.
} else {
  NotEnoughHydro <- 0  ## No, there is more hydro energy budget that can be used beyond the user's target for hydro. So need to solve for how much energy budget is actually used.
}

## OPTION 1 Programming ...
## ++++++++++++
## Solving for NonDispatchableHydro 
## ++++++++++++
PPindex <- PPindex+1 ## This is the column in data.frame "data" to add the hourly generation for the current power plant type
names(data)[PPindex] <- paste0("HydroNonDispatch_MW")
## Must go through each month to determine the maximum constant hourly "HydroNonDispatch" power output
hour_per_season <- c(((31+28)*24),2208,2208,1464,((30+31)*24))  ## In the NREL ReEDS model, Winter (Nov/Dec/Jan/Feb) = 2880 hours, Spring (Mar/Apr/May) = 2208 hours, Summer (June/July/Aug) = 2208 hours, Fall (Sept/Oct) = 1464 hours
assign('hour_per_season',hour_per_season,envir=.GlobalEnv)
HydroNonDispatch_hourly_max <- rep(0,5)
# Note: "PPCost_CurrentRegion$MWh_max_..." and NewPPcost$MWh_max_..." represents the total MWh available (from ReEDS), in the season, and there is a value for both NonDispatchable Hydro, and Dispatchable Hydro
HydroNonDispatch_hourly_max[1] <- PPCost_CurrentRegion$MWh_max_WinterJanFeb[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]/(hour_per_season[1])  ## This is Winter for Jan/Feb ONLY
HydroNonDispatch_hourly_max[2] <- PPCost_CurrentRegion$MWh_max_Spring[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]/hour_per_season[2]
HydroNonDispatch_hourly_max[3] <- PPCost_CurrentRegion$MWh_max_Summer[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]/hour_per_season[3]
HydroNonDispatch_hourly_max[4] <- PPCost_CurrentRegion$MWh_max_Fall[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]/hour_per_season[4]
HydroNonDispatch_hourly_max[5] <- PPCost_CurrentRegion$MWh_max_WinterNovDec[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]/(hour_per_season[5])  ## This is Winter for Nov/Dec ONLY
start.ind <- 1
for (i in 1:5) {
  end.ind <- start.ind + hour_per_season[i] - 1
  data$HydroNonDispatch_MW[start.ind:end.ind] <- rep(HydroNonDispatch_hourly_max[i],hour_per_season[i])
  start.ind <- end.ind + 1
}

## Account for any possible curtailment. Here, there is NO adjustment of hydro capacity if there is curtailment.
## There is only a reduction in the hourly output as needed to ensure "Nuclear + HydroNonDispatch" generation <= total load.
data$Load_minus_Nuclear_MW <- data$Load_MW - data$Nuclear_MW
data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW
if ( length(which(data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW<0)) != 0 )  {
  cat(paste0("There is too much capacity for the combination of non-dispatchable Hydro and Nuclear.",sep="\n"))
  cat(paste0("This program assumes the curtailment of non-dispatchable hydro to ameliorate the problem.",sep="\n"))
  cat(paste0("Alternatively, you can choose to reduce your desired % of MWh from non-dispatchable hydro or nuclear power.",sep="\n"))
  ## Curtail HydroNonDispatch to prevent negative "data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW"
  ind.temp <- which(data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW < 0)  ## These are the indices to curtail data$HydroNonDispatch_MW at some amount
  curtailed_NonDispatchHydro <- 0*data$HydroNonDispatch_MW
  curtailed_NonDispatchHydro[ind.temp] <- -data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW[ind.temp]
  data$HydroNonDispatch_MW <- data$HydroNonDispatch_MW - curtailed_NonDispatchHydro
}
## Reduce data$HydroNonDispatch_MW if the user's desired total Hydro MWh is less than what can be provided by data$HydroNonDispatch_MW
frac_HydroNonDispatch <- sum(data$HydroNonDispatch_MW)/sum(data$Load_MW)
if ( frac_HydroNonDispatch > Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")] ) {
  ## Reduce data$HydroNonDispatch_MW by a constant fraction for each hour of the year (a simple assumption among other possible choices).
  data$HydroNonDispatch_MW <- data$HydroNonDispatch_MW*(Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")]/frac_HydroNonDispatch)
  frac_HydroNonDispatch <- sum(data$HydroNonDispatch_MW)/sum(data$Load_MW)
}

## Now that we have solved for data$HydroNonDispatch, 
## calculate the fraction of user's desired hydro MWh needs to come from HydroDispatch.
Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology=="HydroDispatch")] <- max(0,(Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))] - frac_HydroNonDispatch))
# Must update the desired fraction from HydroNonDispatch (by lowering it) because this is used for later calculations that determine HydroDipatch.
Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")] <- frac_HydroNonDispatch

## Check if annual HydroNonDispatch MWh output (now at its maximum) is greater than the desired output by the user. 
## If the user wants less HydroNonDispatch than is possible, then reduce HydroNonDispatch later 
## after we also solve for HydroDispatch.  If Hydro hourly MW in TOTAL needs to be curtailed, then
## we proceed as follows:
## SITUATION 1: If the user wants less "HydroNonDispatch + HydroDispatch" than is possible (due to resource constraints), 
## 1st. ... then curtail HydroDispatch first, and then HydroNonDispatch second as needed.
## 2nd. Use as much HydroDispatch as possible to lower peak needs from other disptachable resources (right before solving for NG power plant needs)
##      Curtail HydroNonDispatch if user wants less total hydro than is possible.
##      Curtail HydroDispatch also if uswer wants less total hydro than is possible if HydroNonDispatch = 0.
## 
## SITUATION 2: If the user wants more "HydroNonDispatch + HydroDispatch" than is possible (due to resource constraints), 
## 1st. Then use all "HydroNonDispatch + HydroDispatch" that is possible from existing generation and tell
## user that hydro cannot deliver that much MWh.
## 
## SITUATION 3: You can add new hydro capacity to meet some or all of the user's specification.
## TBD ... "existing capacity" + "additional hydro capacity" that <= NewPPcost$MW_maximum defined by resource constraints related to capacity.
## +++++++++++++++++++++++++++++++++++++++


## ++++++++++++
## HydroDispatch is solved later in the code ...
## JUST BEFORE solving for NG power plants but AFTER solving for output from all power plants .
## ++++++++++++


## ++++++++++++
## FUNCTION for evaluationg the objective function to determine the capacity of 
## a dispatchable generator meeting a given input net load.
## ASSUMES NO ELECTRICITY STORAGE OF ANY KIND.
## ++++++++++++
function_dispatchable_generators <- function(multiplier){
  ## Calculate how much the dispatchable generator can generate if at a constant amount each hour
  MWgeneration_temp <- multiplier*rep(1,length(data$net_load_WithCurtailment))  
  ## remove generation each hour that is above net load (generator must operate < 100% capacity)
  net_load_minus_MWgeneration <- data$net_load_WithCurtailment - MWgeneration_temp
  MWgeneration_to_subtract <- net_load_minus_MWgeneration
  MWgeneration_to_subtract[MWgeneration_to_subtract>0] <- 0
  MWgeneration <- MWgeneration_temp + MWgeneration_to_subtract
  ## calculate the fraction of total load served by the current dispatchable generator
  frac.MWh <- sum(MWgeneration)/sum(data$Load_MW)
  ## Objective function to minimize
  sum( 1e6*(frac.MWh-PP_MWneeded$Fraction_MWhDesired[PPnumber])^2 )
} ## function_dispatchable_generators <- function(multiplier){

## ++++++++++++
## FUNCTION for evaluationg the objective function to determine the capacity of 
## a dispatchable generator meeting a given input net load.
## ASSUMES ANNUAL ELECTRICITY STORAGE.
## ++++++++++++
function_dispatchable_generators_AnnualStorage <- function(multiplier){
  ## Calculate how much the dispatchable generator can generate if at a constant amount each hour
  MWgeneration_temp <- multiplier*rep(1,length(data$net_load_WithAnnualStorageWithCurtailment))  
  ## remove generation each hour that is above net load (generator must operate < 100% capacity)
  net_load_minus_MWgeneration <- data$net_load_WithAnnualStorageWithCurtailment - MWgeneration_temp
  MWgeneration_to_subtract <- net_load_minus_MWgeneration
  MWgeneration_to_subtract[MWgeneration_to_subtract>0] <- 0
  MWgeneration <- MWgeneration_temp + MWgeneration_to_subtract
  ## calculate the fraction of total load served by wind and solar
  frac.MWh <- sum(MWgeneration)/sum(data$Load_MW)
  ## Objective function to minimize
  sum( 1e6*(frac.MWh-PP_MWneeded$Fraction_MWhDesired[PPnumber])^2 )
} ## function_dispatchable_generators_AnnualStorage <- function(multiplier){


## ++++++++++++
## START: FUNCTION function_Wind_PV_CSP
## FUNCTION for evaluationg the objective function to determine wind and solar capacity (both PV and CSP).
## ++++++++++++
function_Wind_PV_CSP <- function(multipliers){
  multiplier.wind <- multipliers[1]
  multiplier.PV <- multipliers[2]
  multiplier.CSP <- multipliers[3]
  multiplied.wind <- multiplier.wind*data$Wind_MW
  multiplied.PV <- multiplier.PV*data$SolarPV_MW
  multiplied.CSP <- multiplier.CSP*data$SolarCSP_MW
  
  ## "net load" here is = Load - Nuclear - HydroNonDispatch - Wind - PV
  net_load <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW - multiplied.wind - multiplied.PV - multiplied.CSP
  ind <- which(net_load<0)  ## These are the indices of the net load vector that are negative
  
  ## subtract a portion of curtailment from the current amount of wind and PV in "multiplied.wind"
  ## and "multiplied.PV".  The wind and solar are curtailed in proportion to how much they are generating.
  ## So if you have to curtail 1000 MW, and at that hour there is 10,000 MW of wind and 5,000 MW of PV,
  ## then PV is 33% of the total and wind is 67% of total "wind + solar", and wind will be curtailed
  ## 1000MW*67% = 667 MW and PV will be curtailed 1000MW*33% = 333 MW. 
  ## First: calculate how much wind and PV is put onto grid after removing curtailed MW from their non-curtailed MW generation
  wind_fraction = multiplied.wind/(multiplied.PV+multiplied.wind+multiplied.CSP)
  PV_fraction = multiplied.PV/(multiplied.PV+multiplied.wind+multiplied.CSP)
  CSP_fraction = multiplied.CSP/(multiplied.PV+multiplied.wind+multiplied.CSP)
  multiplied.wind_AfterCurtailment <- multiplied.wind 
  multiplied.wind_AfterCurtailment[ind] <- multiplied.wind[ind] + net_load[ind]*wind_fraction[ind]
  multiplied.PV_AfterCurtailment <- multiplied.PV
  multiplied.PV_AfterCurtailment[ind] <- multiplied.PV[ind] + net_load[ind]*(PV_fraction[ind])
  multiplied.CSP_AfterCurtailment <- multiplied.CSP
  multiplied.CSP_AfterCurtailment[ind] <- multiplied.CSP[ind] + net_load[ind]*(CSP_fraction[ind])
  ## Second: calculate the fraction of total load served by wind and solar
  frac.wind <- sum(multiplied.wind_AfterCurtailment)/sum(data$Load_MW)
  frac.PV <- sum(multiplied.PV_AfterCurtailment)/sum(data$Load_MW)
  frac.CSP <- sum(multiplied.CSP_AfterCurtailment)/sum(data$Load_MW)
  return(sum(1e6*( (frac.wind-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Wind")])^2 + (frac.PV-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="PV")])^2 + (frac.CSP-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="CSP")])^2 ) ))
}  
## ++++++++++++
## END: FUNCTION function_Wind_PV_CSP
## ++++++++++++


## ++++++++++++
## START: FUNCTION function_Wind_PV_CSP_daily_storage_tiny
## ++++++++++++
function_Wind_PV_CSP_daily_storage_tiny <- function(multiplier){
  ## Calculate the "bottom" constant generation each hour from which to subtract from net load.
  MWgeneration_bottom <- multiplier*rep(1,length(net_load_storage_tiny))  
  ## remove generation each hour that is above net load (generator must operate < 100% capacity)
  net_load_minus_MWgeneration_bottom <- net_load_storage_tiny - MWgeneration_bottom
  net_load_minus_MWgeneration_bottom[net_load_minus_MWgeneration_bottom<0] <- 0
  ## Objective function to minimize
  sum( 1e6*(storage_budget_tiny - sum(net_load_minus_MWgeneration_bottom))^2 )
} ## function_Wind_PV_CSP_daily_storage_tiny <- function(multiplier,storage_budget,net_load){
## ++++++++++++
## END: FUNCTION function_Wind_PV_CSP_daily_storage_tiny
## ++++++++++++


## ++++++++++++
## BEGIN: FUNCTION function_solve_hydro_dispatch
## ++++++++++++## ++++++++++++++++
## Sequence and Functions to solve for HydroDispatch 
## ++++++++++++++++
## Use this "function_solve_hydro_dispatch" to solve for data$HydroDispatch_MW for each season.
## If there is enough energy budget each season for what the user wants, then the dispatch
## will be at the maximum capacity (MW) each hour of the year. If the energy budget is less than
## the user wishes for HydroDispatch, then this function dispatches HydroDispatch at the highest 
## values of net load (at this point in the algorithm) first.
function_solve_hydro_dispatch <- function(hr_max,max_capacity,hydro_budget,net_load_duration_input,hours_in_season){
  max_hydro_dispatch_possible <- min(max_capacity,max(net_load_duration_input$MW)) ## the storage cannot dispatch more than its rated capacity or more than the value of the net load
  if (hr_max == 0) {  ## Then HydroDispatch for this season is zero for all hours
    ## Do nothing because "data$HydroDispatch_MW" is already all zeros
  } else { 
    for (i in 1:(hr_max-1) ) {
      ## Calculate "area1" and "area2", the sum of which needs to equal the total seasonal energy budget.
      ## The definitions of "area1" and "area2" are based on the net load duration curve for each season.
      ## They are defined (partially) by "hr_transition" which is some unknown
      ## hour in the net load duration curve less than "hr_max".  So we start at "hr_max = hr_transition", and reduce it
      ## until "area1" + "area2" = "energy budget".
      ## area1 = the area enclosed by ... (on the left): the y-axis, (on the top): the load duration curve, (on the bottom): MAXIMUM of (1) zero or (2) load duration curve minus a constant value "max_hydro_dispatch_possible", (on the right): hr_transition (nothing to the right of this)
      ## area2 = the area enclosed by ... (on the left): hr_transition (nothing to the left of this), (on the top): load duration curve, (on the bottom): MAXIMUM of (1) zero or (2) load duration curve at "hr_transition" minus a constant value "max_hydro_dispatch_possible", (on the right): either the end of the load duration curve (hour = 8760) or "hr_end_area2_vector" where the last value is where when the load duration curve ~= the value at "hr_transition"
      hr_transition = hr_max + 1 - i
      
      ## Calculate area1
      if (net_load_duration_input$MW[hr_transition] > max_hydro_dispatch_possible) { ## Then the bottom boundary of area1 is not zero
        #area1 = hr_transition*max_capacity
        area1 = hr_transition*max_hydro_dispatch_possible
      } else {  ## Then the bottom boundary of area1 is zero (the x-axis)
        area1 = sum(net_load_duration_input$MW[1:hr_transition])
      }
      
      hr_end_area2_vector <- which(net_load_duration_input$MW<(net_load_duration_input$MW[hr_transition] - max_hydro_dispatch_possible))
      
      if ( length(hr_end_area2_vector)==0 ) {
        hr_end_area2 = hours_in_season
      } else {
        hr_end_area2 <- min(hr_end_area2_vector)  ## This returns "hr_end_area2 = Inf" if there are no hours when any of "net_load_duration_input$MW" are less than the quantity in question
        if (hr_end_area2>hours_in_season) {
          hr_end_area2 = hours_in_season
        }
      }
      area2 = sum(net_load_duration_input$MW[(hr_transition+1):hr_end_area2]-rep(net_load_duration_input$MW[hr_end_area2],(hr_end_area2-hr_transition)))
      
      if ( (area1+area2)<=hydro_budget)  {  ## Then we are now successfully dispatching HydroDispatch as much as we can within capacity (MW) and energy (MWh) constraints
        ## NOTE: If we never reach the condition "(area1+area2)<=hydro_budget", "then net_load_duration_input$HydroDispatch_MW" remains all zeros as per it is input as all zeros.
        ## Then we can end this for loop because we have found the critical "hr_transition"
        net_load_duration_input$HydroDispatch_MW[1:hr_transition] <- pmin(net_load_duration_input$MW[1:hr_transition],rep(max_hydro_dispatch_possible,hr_transition))
        if ( hr_end_area2 > (hr_transition+1) ) {
          ## Need to figure out how to detrmine dispatch each hour from "(hr_transition+1)" to "hr_end_area2"
          net_load_duration_input$HydroDispatch_MW[(hr_transition+1):hr_end_area2] <- net_load_duration_input$MW[(hr_transition+1):hr_end_area2] - net_load_duration_input$MW[hr_end_area2]
        } else {
          ## Have to individually specify the HydroDispatch for the last hour (lowest net load) for this season.
          if ( (hydro_budget-(area1+area2)) > max_hydro_dispatch_possible ) {
            ## If this is the case, then there is more energy budget than can be dispatched at the maximum capacity.
            ## Thus, the HydroDispatch for this last hour of net load is dispatched at the exising MW cacpacity for HydroDispatch.
            net_load_duration_input$HydroDispatch_MW[hr_end_area2] <- max_hydro_dispatch_possible
          } else {
            ## Then the hydro_budget that is left is less than the total capacity of existing HydroDispatch, so just dispatch all of the remaining energy budget.
            net_load_duration_input$HydroDispatch_MW[hr_end_area2] <- hydro_budget-(area1+area2)
          }
        }  ## if ( hr_end_area2 > (hr_transition+1) ) 
        break
      }  else if ( (hr_max-1)==1  &  i==1  &  ((area1+area2)>hydro_budget) ) { ## "else if" for ... if ( (area1+area2)<=hydro_budget )
        ## Go here if we can possibly dispatch all MWh of storage within the 1 hour of peak net load and it must be less than the 
        ## "max_capapcity" in MW since the stored MWh is less than 1 hr at full MW storage dispatch.
        multiplier_init <- 0.95*max(net_load_duration_input$MW)
        assign('net_load_storage_tiny',net_load_duration_input$MW,envir=.GlobalEnv)
        assign('storage_budget_tiny',hydro_budget,envir=.GlobalEnv)
        temp_multiplier <- Rcgmin(fn=function_Wind_PV_CSP_daily_storage_tiny,lower=0,upper=max(net_load_duration_input$MW),par=multiplier_init,control=list(dowarn=FALSE))
        if (temp_multiplier$par < 1e-6) {
          temp_multiplier$par <- 0
        }
        MWgeneration_bottom <- temp_multiplier$par*rep(1,length(net_load_storage_tiny))  
        ## remove generation each hour that is above net load (generator must operate < 100% capacity)
        net_load_minus_MWgeneration_bottom <- net_load_storage_tiny - MWgeneration_bottom
        net_load_minus_MWgeneration_bottom[net_load_minus_MWgeneration_bottom<0] <- 0
        net_load_duration_input$HydroDispatch_MW <- net_load_minus_MWgeneration_bottom
      } else if ( hr_transition==2 & ((area1+area2)>hydro_budget) ) { ## minimum hr_transition = 2; 
        ## Then the hydro or other storage can never discharge at its maximum capacity.
        area1 <- 0
        ## Increase "hr_end_area2" starting from 1 until "area2 > hydro_budget" and then set the 
        ## dispatch of the storage to be exactly the same as the net_load_duration_input$MW from 
        ## index = 1 to index = "hr_end_area2 - 1" (the index just before finding out that "area2 > hydro_budget")
        ## IT SEEMS LIKE THIS "for (k ...)" loop could be put outside of the loop "for (i in 1:(hr_max-1) )" to avoid that "for (i ..." loop in some cases.
        for (k in 1:dim(net_load_duration_input)[1]) {
          hr_end_area2 <- k
          area2 <- sum(net_load_duration_input$MW[1:hr_end_area2]-rep(net_load_duration_input$MW[hr_end_area2],hr_end_area2))
          if (area2 > hydro_budget) {
            hr_end_area2 <- hr_end_area2 - 1
            net_load_duration_input$HydroDispatch_MW[1:hr_end_area2] <- net_load_duration_input$MW[1:hr_end_area2] - net_load_duration_input$MW[hr_end_area2]
            break
          } ## end of "if (area2 > hydro_budget) {"
        } ## end of "for (k in 1:dim(net_load_duration_input)[1]) {"
      } ## end of "else if" part of ...  if ( (area1+area2)<=hydro_budget )
    } ## for (i in 1:hr_max) 
    ## Reorder "net_load_duration_input" to go in chronological order to later be insert into "data$HydroDispatch_MW".
    net_load_duration_input <- net_load_duration_input[order(net_load_duration_input$Hrs),]
    output <- data.frame(net_load_duration_input)
    return(output)
  } ## if (hr_max == 0) { 
}  ## function_solve_hydro_dispatch <- function(factors){
## ++++++++++++
## END: FUNCTION function_solve_hydro_dispatch
## ++++++++++++


## ****************************************
## ++++++++++++
## BEGIN: FUNCTION function_Wind_PV_CSP_annual_storage
## FUNCTION for evaluationg the objective function to determine wind and solar capacity (both PV and CSP)
## when assuming THERE IS SEASONAL STORAGE OF WIND AND SOLAR ELECTRICITY.
## ++++++++++++
function_Wind_PV_CSP_annual_storage <- function(x){
  multiplier.wind <- x[1]
  multiplier.PV <- x[2]
  multiplier.CSP <- x[3]

  multiplied.wind <- multiplier.wind*data$Wind_MW
  multiplied.PV <- multiplier.PV*data$SolarPV_MW
  multiplied.CSP <- multiplier.CSP*data$SolarCSP_MW
  ## "net load" here is = Load - Nuclear - HydroNonDispatch - Wind - PV
  net_load <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW - multiplied.wind - multiplied.PV - multiplied.CSP
  ind <- which(net_load<0)  ## These are the indices of the net load vector that are negative
  
  ## +++++++++++++++++
  ## First: Calculate curtailed wind and solar (both PV and CSP) generation.
  ## This curtailment is what is potentially stored.
  ## Subtract a portion of curtailment from the current amount of wind and PV in "multiplied.wind"
  ## "multiplied.PV", and "multiplied.CSP".  The wind and solar are curtailed in proportion to how much they are generating.
  ## So if you have to curtail 1000 MW, and at that hour there is 10,000 MW of wind and 5,000 MW of PV,
  ## then PV is 33% of the total and wind is 67% of total "wind + solar", and wind will be curtailed
  ## 1000MW*67% = 667 MW and PV will be curtailed 1000MW*33% = 333 MW.
  ## +++++++++++++++++
  curtailed_WindSolar <- net_load  ## initialize
  curtailed_WindSolar[curtailed_WindSolar>0]=0
  curtailed_WindSolar <- -curtailed_WindSolar
  noncurtailed_WindSolar <- multiplied.wind+multiplied.PV+multiplied.CSP
  wind_fraction = multiplied.wind/noncurtailed_WindSolar
  PV_fraction = multiplied.PV/noncurtailed_WindSolar
  CSP_fraction = multiplied.CSP/noncurtailed_WindSolar
  curtailed_Wind <- curtailed_WindSolar*wind_fraction
  curtailed_PV   <- curtailed_WindSolar*PV_fraction
  curtailed_CSP  <- curtailed_WindSolar*CSP_fraction
  curtailed_Wind[is.infinite(curtailed_Wind)==TRUE] <- 0  ## ensure there are no "inf" from dividing by zero
  curtailed_PV[is.infinite(curtailed_PV)==TRUE] <- 0      ## ensure there are no "inf" from dividing by zero
  curtailed_CSP[is.infinite(curtailed_CSP)==TRUE] <- 0    ## ensure there are no "inf" from dividing by zero
  curtailed_Wind[is.na(curtailed_Wind)==TRUE] <- 0  ## ensure there are no NA values
  curtailed_PV[is.na(curtailed_PV)==TRUE] <- 0      ## ensure there are no NA values
  curtailed_CSP[is.na(curtailed_CSP)==TRUE] <- 0    ## ensure there are no NA values
  MW_capacity_AnnualStorage <- max(curtailed_WindSolar)  ## The MW capacity of the storage system is assumed = that require to store the maxim MW of curtailed Wind+PV+CSP
  
  ## Second: calculate how much wind and PV is put onto grid after removing curtailed MW from their non-curtailed MW generation
  multiplied.wind_DirectToGrid <- multiplied.wind
  multiplied.wind_DirectToGrid <- multiplied.wind - curtailed_Wind
  # multiplied.wind_DirectToGrid[ind] <- multiplied.wind[ind] + net_load[ind]*wind_fraction[ind]
  multiplied.PV_DirectToGrid <- multiplied.PV
  multiplied.PV_DirectToGrid <- multiplied.PV - curtailed_PV
  multiplied.CSP_DirectToGrid <- multiplied.CSP
  multiplied.CSP_DirectToGrid <- multiplied.CSP - curtailed_CSP
  
  ## +++++++++++++++++
  ## Assume that we can neglect the initial state of charge (SOC_initial) by assuming that 
  ## 1. the storage system stores all curtailed wind and solar (= "curtailed_WindSolar")
  ## 2. the storage system, over the course of the year, cannot dispatches more than "curtailed_WindSolar*efficiency_OneWay_DailyStorage"
  ## 3. if the system needed an initial state of charge (say on January 1 at midnight), then this could be provided by carrying over 
  ##    storage from the previous year as long as it is not larger than the maximum total MWh storage capacity needed on any other day point during the year.
  ## The storage algorithm works as follows:
  ## 1. Assume that "charging" only occurs for wind and solar generation that would otherwise be curtailed.
  ##    Thus, storage "charging" cannot occur when net load is >= 0.
  ## 2. Storage "discharging" can only occur when net load is >= 0.
  ## 3. The MW capacity for "charging" equals the MW capacity for "discharging".
  ## 4. The MW capacity for "charging" is an optimized (why did I write "optimized"? it is not really optimized) variable defined as the maximum amount of "otherwise curtailed wind and solar" (or negative net load) experienced.
  ## +++++++++++++++++
  capacity.wind <- MW_data_Wind*multiplier.wind
  capacity.PV <- MW_data_PV*multiplier.PV
  capacity.CSP <- MW_data_CSP*multiplier.CSP
  StoredWindSolar <- curtailed_WindSolar*efficiency_OneWay_DailyStorage^2                ## First, assume all curtailed wind and solar is stored, subtracting MWh due to efficiency loss
  StoredWindSolar[StoredWindSolar>MW_capacity_AnnualStorage] <- MW_capacity_AnnualStorage  ## Next, this says I can't store energy faster in any given hour, due to power constraint, than the MW power capacity of the storage system
  StoredWindSolar_cumulative <- cumsum(StoredWindSolar) ## Cumulative storage of wind and solar MWh
  StoredWind <- StoredWindSolar*curtailed_Wind/(curtailed_Wind+curtailed_PV+curtailed_CSP)
  StoredPV <- StoredWindSolar*curtailed_PV/(curtailed_Wind+curtailed_PV+curtailed_CSP)
  StoredCSP <- StoredWindSolar*curtailed_CSP/(curtailed_Wind+curtailed_PV+curtailed_CSP)
  StoredWind[is.nan(StoredWind)=="TRUE"]=0
  StoredPV[is.nan(StoredPV)=="TRUE"]=0
  StoredCSP[is.nan(StoredCSP)=="TRUE"]=0
  
  ## Dispatch storage on "day_now" using storage existing on previous "day_now - 1" to reduce the peak load hours on "day_now"
  ## using as much energy as is stored or until all net_load has been dispatched using storage.
  ## Use the function "function_solve_hydro_dispatch" that was derived for dispatching seasonal HydroDispatch
  ## because we can use the same algorithm (e.g., a limited storage budget dispatched ideally for a given net load)
  hrs_per_year = 8760
  positive_net_load <- net_load
  positive_net_load[positive_net_load<0] <- 0
  Dispatched_StoredWindSolar <- rep(0,dim(data)[1])  ## Initialize a data frame of hourly storage dispatch
  Dispatched_StoredWindSolar <- matrix(0, ncol = 1, nrow = dim(data)[1])  
  Dispatched_StoredWindSolar <- data.frame(Dispatched_StoredWindSolar)
  names(Dispatched_StoredWindSolar) <- c("MW")
  Dispatched_StoredWindSolar$Hrs <- seq(1,dim(data)[1])
  net_load_duration <- matrix(0, ncol = 1, nrow = hrs_per_year)  ## Initialize a data frame that will be the daily net load duration curve that is updated as the dispatch function is called for each day of the year
  net_load_duration <- data.frame(net_load_duration)
  names(net_load_duration) <- c("MW")
  net_load_duration$Hrs <- seq(1,hrs_per_year)
  net_load_duration$HydroDispatch_MW <- rep(0,dim(net_load_duration)[1])  ## here the term "HydroDispatch" is used becuase we use the "function_solve_hydro_dispatch" that already assumed that terminology, and we can reuse it to solve for storage dispatch
  net_load_duration$MW <- positive_net_load[1:hrs_per_year]
  net_load_duration <- net_load_duration[order(-net_load_duration$MW),]
  if (MW_capacity_AnnualStorage > 0) {
    hr_max <- min((hrs_per_year-1),ceiling(sum(StoredWindSolar)/MW_capacity_AnnualStorage)) ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hours in the day
    hr_max <- max(hr_max,0) ## ensure there is no negative "hr_max".
    ## Solve storage dispatch for the year that takes off annual net load peaks as much as possible
    if (hr_max>0) {
      storage_annual_outputs <- function_solve_hydro_dispatch(hr_max,MW_capacity_AnnualStorage,sum(StoredWindSolar),net_load_duration,hrs_per_year)
      Dispatched_StoredWindSolar <- storage_annual_outputs$HydroDispatch_MW
    } else {
      hr_max <- 0
      Dispatched_StoredWindSolar <- rep(0,length(net_load))
    }
  } else {
    hr_max <- 0
    Dispatched_StoredWindSolar <- rep(0,length(net_load))
  }
  
  ## Figure out how much of wind, PV, and CSP were dispatched as storage.
  ## For simplicity, assume storage dispatched from each of wind, PV, and CSP
  ## occurs at the same fraction each was curtailed each day. This should be close enough.
  Dispatched_StoredWindSolar_cumulative <- cumsum(Dispatched_StoredWindSolar)
  StoredWind_cumulative <- cumsum(StoredWind)
  StoredPV_cumulative <- cumsum(StoredPV)
  StoredCSP_cumulative <- cumsum(StoredCSP)
  last_yr <- length(Dispatched_StoredWindSolar_cumulative)
  Dispatched_StoredWind <- StoredWind_cumulative[last_yr]*(StoredWind_cumulative[last_yr]+StoredPV_cumulative[last_yr]+StoredCSP_cumulative[last_yr])/Dispatched_StoredWindSolar_cumulative[last_yr]
  Dispatched_StoredPV <- StoredPV_cumulative[last_yr]*(StoredWind_cumulative[last_yr]+StoredPV_cumulative[last_yr]+StoredCSP_cumulative[last_yr])/Dispatched_StoredWindSolar_cumulative[last_yr]
  Dispatched_StoredCSP <- StoredCSP_cumulative[last_yr]*(StoredWind_cumulative[last_yr]+StoredPV_cumulative[last_yr]+StoredCSP_cumulative[last_yr])/Dispatched_StoredWindSolar_cumulative[last_yr]
  Dispatched_StoredWind[is.nan(Dispatched_StoredWind)=="TRUE"]=0
  Dispatched_StoredPV[is.nan(Dispatched_StoredPV)=="TRUE"]=0
  Dispatched_StoredCSP[is.nan(Dispatched_StoredCSP)=="TRUE"]=0
  Dispatched_StoredWind[is.infinite(Dispatched_StoredWind)=="TRUE"]=0
  Dispatched_StoredPV[is.infinite(Dispatched_StoredPV)=="TRUE"]=0
  Dispatched_StoredCSP[is.infinite(Dispatched_StoredCSP)=="TRUE"]=0
  
  ## +++++++++++++++++
  ## Calculate the fraction of total load served by wind and solar to use
  ## in the objective function.
  ## +++++++++++++++++
  ## Need the fraction of generation from wind, PV, and CSP
  frac.wind <- sum(multiplied.wind_DirectToGrid,Dispatched_StoredWind)/sum(data$Load_MW)
  frac.PV <- sum(multiplied.PV_DirectToGrid,Dispatched_StoredPV)/sum(data$Load_MW)
  frac.CSP <- sum(multiplied.CSP_DirectToGrid,Dispatched_StoredCSP)/sum(data$Load_MW)
  ## Objective function with:
  ## (1) targeting desired fractions of wind, PV, Solar
  return(  sum(1e7*( (frac.wind-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Wind")])^2 + (frac.PV-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="PV")])^2 + (frac.CSP-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="CSP")])^2 ) ))
}
## ++++++++++++
## END: FUNCTION function_Wind_PV_CSP_annual_storage
## FUNCTION for evaluationg the objective function to determine wind and solar capacity (both PV and CSP)
## when assuming THERE IS SEASONAL STORAGE OF WIND AND SOLAR ELECTRICITY.
## ++++++++++++

## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## CALCULATIONS FOR GENERATION CAPACITY (not nuclear or non-dispatchable hydro)
## ASSUMING NO ELECTRICITY STORAGE - BEGIN
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++

## +++++++++++++++
## Call function to optimize the capacity needed for wind and solar PV.
##
## CALL PROTOCOL FOR CALLING R OPTIMIZATION FUNCTION
## Rcgmin(par, fn, gr, lower, upper, bdmsk, control = list(), ...)
## +++++++++++++++
## Solve for PV and Wind using gradient based search
## To make the gradient-based optimization work well, the initial conditions 
## need to be scaled with the desired fractions of wind and solar.
multiplier_init.wind <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("Wind"))]  ## Wind
multiplier_init.PV <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("PV"))] ## Solar PV
multiplier_init.CSP <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("CSP"))] ## Solar CSP
multipliers <- c(multiplier_init.wind,multiplier_init.PV)  ## initial guesses into optimization to find multipliers for wind and solar profiles
lb = c(0,0,0)   ## lower bound on multipliers for wind and solar profiles
ub = c(constrained_capacity_NoStorage.Wind,constrained_capacity_NoStorage.PV,constrained_capacity_NoStorage.CSP)   ## upper bound on multipliers for wind and solar profiles
init_guesses <- c(multiplier_init.wind,multiplier_init.PV,multiplier_init.CSP)
max_iters <- 200
tolerance_NoStorage <- 1e10
print("In solveGEN.R: Start optimize wind and solar capacity (no storage).")
wind_solar_multipliers <- optim(par=init_guesses,fn=function_Wind_PV_CSP,lower=lb,upper=ub,method = c("L-BFGS-B"),control=list(maxit=max_iters,factr=tolerance_NoStorage))
print("In solveGEN.R: Completed optimize wind and solar capacity (no storage).")
multiplier.wind <- wind_solar_multipliers$par[1]
multiplier.PV <- wind_solar_multipliers$par[2]
multiplier.CSP <- wind_solar_multipliers$par[3]


## +++++++++++++++
## Now calculate the net load (in absoluate MW each hour) that needs to
## be served by the dispatchable generators after accounting for
## wind and solar (with no electricity storage).
## +++++++++++++++
data$Wind_MW_total <- multiplier.wind*data$Wind_MW  ## total Wind output per hour, no curtailment
data$PV_MW_total  <- multiplier.PV*data$SolarPV_MW  ## total PV output per hour, no curtailment
data$CSP_MW_total  <- multiplier.CSP*data$SolarCSP_MW  ## total CSP output per hour, no curtailment
data$net_load_NoCurtailment <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW - data$Wind_MW_total - data$PV_MW_total - data$CSP_MW_total
data$net_load_WithCurtailment <- data$net_load_NoCurtailment
data$net_load_WithCurtailment[data$net_load_WithCurtailment<0]=0


## +++++++++++++++++
## BEGIN
## Perform optimization to find the capacity of dispatchables in the actual time series of net_load.
## This for loop solves for dispatchable generation when there is NO STORAGE of any kind.
## +++++++++++++++++
## Go through each power plant type to determine its capacity, for power plants that are not 
## (1) wind, solar, nuclear, and other assumed non-dispatchable power plants
## (2) natural gas combined cycle (NGCC) or natural gas combustion turbine (NGCT), since these are determined later, or
## (3) dispatchable hydro (HydroDispatch) as this is solved for right before solving for NG capacity.
## Thus this sequence is for power plants: coal, biomass, petroleum, and geothermal

## Set maximum MWh for biomass power plants (used as a check after solving for biomass power plant capacity and dispatch)
biomass_MMBtu_max <- sum(biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu)
HeatRate_biomass <- 7845  ## Assumed heat rate of biomass power plant in 2050 [Btu/kWh]
MWh_max_biomass <- biomass_MMBtu_max*1e6/HeatRate_biomass/1e3  # set upper bound on biomass MWh based on the maximum MMBtu limit of the EIoF region's fuel supply. Assume a heat rate for biommass in year 2050 = 7845 Btu/kWh
#cat(paste0("Upper Bound MWh for Biomass is MWh_max_biomass =: ",MWh_max_biomass, ", but my algorithm is setting a limit on MW now MWh!!!"),sep="\n")

## Set maximum MW for geothermal power plants as set by NREL ReEDS data
geothermal_MW_max <- sum(geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW)

print("In solveGEN.R: Start of solve dispatch of dispatchable technologies (no storage).")
## Now go through solving for dispatch and capacity of dispatchable power plants
for (i in 1:(dim(Frac_MWhDesired_dispatchable)[1]-2)) {  ## Subtract 2 for 1) NGCC and 2) NGCT
  names(data)[PPindex+i] <- paste0(PP_MWneeded$Technology[i],"_MW")  ## Recall, PP_MWneeded has been put in an order from smallest to largest variable operating cost, but with renewables, nuclear, and NG plants arbitrarily set to very high variable costs such that they are ordered at the end
  assign("PPnumber",i) ## Find capacity of the 1st cheapest dispatchable generator (i=1) with Frac_MWhDesired > 0

  ## ++++++++++++++
  ## Call optimization to determine the MW capacity of dispatchable technologies 
  ## chosen in order from cheapest to most expensive technology to run at 100% capacity.
  ## ++++++++++++++
  multiplier_init.dispatch <- 0.1  ## initial guess at how much to multiply generation profile
  lb = c(0)   ## lower bound on multipliers (which will be solved for MW capacities for each power plant type)
  ub = c(9.999e9)  ## make an abitrarily large number of 9.999 x 10^9 MW that will hold for all technologies except geothermal (per how algorithm works: biomass is limited by MWh, not MW)
  init_guesses <- c(multiplier_init.dispatch)
  if (names(data)[PPindex+i] != "HydroDispatch_MW") {
    if (names(data)[PPindex+i] == "Geothermal_MW") {
      ub = c(geothermal_MW_max)
    }
    ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible later in the code.
    MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_generators,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE))

    # Make any PP_MWneeded$MW_needed < a certain threshold just become zero.
    # Then all generation from that technology is zero each hour of the year.
    if (MWcapacity_multiplier$par > 1e-6) {
      PP_MWneeded$MW_needed[i] <- MWcapacity_multiplier$par
    } else {
      PP_MWneeded$MW_needed[i] <-0
    }
  } else { ## Then names(data)[PPindex+i] == "HydroDispatch_MW"
    MWcapacity_multiplier$par <- 0
    PP_MWneeded$MW_needed[i] <- 0
  }## if (names(data)[PPindex+i] != "HydroDispatch_MW") {
  
  ## ++++++++++++++
  ## Need to recalculate what is left for "net load" after removing the generation
  ## from the dispatchable generators that have already been determined at this point.
  ## ++++++++++++++
  MWgeneration_temp <- PP_MWneeded$MW_needed[i]*rep(1,length(data$net_load_WithCurtailment))  
  ## remove generation each hour that is above net load (generator must operate < 100% capacity)
  net_load_minus_MWgeneration <- data$net_load_WithCurtailment - MWgeneration_temp
  MWgeneration_to_subtract <- net_load_minus_MWgeneration
  MWgeneration_to_subtract[MWgeneration_to_subtract>0] <- 0
  MWgeneration <- MWgeneration_temp + MWgeneration_to_subtract
  ## Make a correction if you are over the limit on MWgeneration for Biomass
  if (PP_MWneeded$Technology[i] == "Biomass" & sum(MWgeneration)>MWh_max_biomass) {
    biomass_reduction_factor_NoStorage <- MWh_max_biomass/sum(MWgeneration)
    MWgeneration <- biomass_reduction_factor_NoStorage*MWgeneration # Reduce generation each hour by the same fraction
    PP_MWneeded$MW_needed[i] <- biomass_reduction_factor_NoStorage*PP_MWneeded$MW_needed[i] # Reduce Power Plant capacity of Biomass by neccesary factor
  } else {
    biomass_reduction_factor_NoStorage <- 0
  }
  data$net_load_WithCurtailment <- data$net_load_WithCurtailment - MWgeneration
  data[,i+PPindex] <- MWgeneration
  rm(MWgeneration_temp,MWgeneration_to_subtract,MWgeneration)
} ## for (i in 1:dim(Frac_MWhDesired_dispatchable)[1]) {
## +++++++++++++++++
## END
## Perform optimization to find the capacity of dispatchables in the actual time series of net_load.
## This for loop solves for dispatchable generation when there is NO STORAGE of any kind.
## +++++++++++++++++
print("In solveGEN.R: End of solve dispatch of dispatchable technologies (no storage).")

## +++++++++++++++++
## Input the MW of installed capacity needed for Wind, PV, and CSP into "PP_MWneeded".
## Here this assumes there is no electricity storage of any kind.
## +++++++++++++++++
PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind")] <- MW_data_Wind*multiplier.wind
PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV")] <- MW_data_PV*multiplier.PV
PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP")] <- MW_data_CSP*multiplier.CSP

## +++++++++++++++++
## Calculate curtailed wind and solar (both PV and CSP) generation.
## Here this assumes there is no electricity storage of any kind.
## +++++++++++++++++
curtailed_WindSolar <- data$net_load_NoCurtailment
curtailed_WindSolar[curtailed_WindSolar>0]=0
curtailed_WindSolar<- -curtailed_WindSolar
noncurtailed_WindSolar <- data$Wind_MW_total+data$PV_MW_total+data$CSP_MW_total
curtailed_Wind <- curtailed_WindSolar*(data$Wind_MW_total)/(noncurtailed_WindSolar)
curtailed_PV   <- curtailed_WindSolar*(data$PV_MW_total)  /(noncurtailed_WindSolar)
curtailed_CSP  <- curtailed_WindSolar*(data$CSP_MW_total) /(noncurtailed_WindSolar)
curtailed_Wind[is.na(curtailed_Wind)==TRUE] <- 0  ## ensure there are no NA from dividing by zero
curtailed_PV[is.na(curtailed_PV)==TRUE] <- 0      ## ensure there are no NA from dividing by zero
curtailed_CSP[is.na(curtailed_CSP)==TRUE] <- 0    ## ensure there are no NA from dividing by zero


## ++++++++++++++++
## Sequence of loops to solve for HydroDispatch (NO ANNUAL ELECTRICITY STORAGE) - BEGIN
## ++++++++++++++++
## ++++++++++++
## Solving for DispatchableHydro JUST BEFORE solving for NG power plants but AFTER solving for 
## output from all power plants.
## Here we assume that DispatchableHydro removes peak loads as much as possible, as a simple
## way to "dispatch" in a way that alleviates peaking capacity on the grid and occurs at times of highest marginal cost.
## Here this assumes there is no electricity storage of any kind.
## ++++++++++++
data$generation_non_NG_HydroDispatch <- noncurtailed_WindSolar-curtailed_WindSolar+data$Coal_MW+data$Nuclear_MW+data$UserTech1_MW+data$UserTech2_MW+data$Biomass_MW+data$HydroDispatch_MW+data$HydroNonDispatch_MW+data$PetroleumCC_MW+data$Geothermal_MW
data$net_load_solve_HydroDispatch <- data$Load_MW - data$generation_non_NG_HydroDispatch
## Sometimems data$net_load_solve_HydroDispatch is < 0 within a small tolerance, so turn these into zero to avoid problems of sorting later
data$net_load_solve_HydroDispatch[(data$net_load_solve_HydroDispatch<0)&(data$net_load_solve_HydroDispatch>-1e-10)]=0


## +++++++++++
## Initializing information for solving for HydroDispatch.
## Here this assumes there is no electricity storage of any kind.
## Hydro dispatch occurs separately for each of the four seasons as defined in NREL ReEDS.
## +++++++++++
## First: Create a "net load duration curve" for each season. Separate net load for each season
net_load_duration_season <- matrix(0, ncol = 4, nrow = max(c((hour_per_season[1]+hour_per_season[5]),hour_per_season[2],hour_per_season[3],hour_per_season[4])))  ## ncol= 4 seasons (combine Jan/Feb with Nov/Dec for 1 Winter); nrow = maximum number of days in any season
net_load_duration_season <- data.frame(net_load_duration_season)
names(net_load_duration_season) <- c("Winter_MW","Spring_MW","Summer_MW","Fall_MW")
net_load_duration_season$Winter_MW <- c(data$net_load_solve_HydroDispatch[1:hour_per_season[1]],data$net_load_solve_HydroDispatch[(sum(hour_per_season[1:4])+1):sum(hour_per_season[1:5])])
net_load_duration_season$HrsWinter <- c(seq(1,hour_per_season[1]),seq((sum(hour_per_season[1:4])+1),(sum(hour_per_season[1:5])),1))
hrs_temp <- seq((sum(hour_per_season[1])+1),(sum(hour_per_season[1:2])))
net_load_duration_season$HrsSpring <- c(hrs_temp,rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
net_load_duration_season$Spring_MW <- c(data$net_load_solve_HydroDispatch[(sum(hour_per_season[1])+1):sum(hour_per_season[1:2])],rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
rm(hrs_temp)
hrs_temp <- seq((sum(hour_per_season[1:2])+1),(sum(hour_per_season[1:3])))
net_load_duration_season$HrsSummer <- c(hrs_temp,rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
net_load_duration_season$Summer_MW <- c(data$net_load_solve_HydroDispatch[(sum(hour_per_season[1:2])+1):sum(hour_per_season[1:3])],rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
rm(hrs_temp)
hrs_temp <- seq((sum(hour_per_season[1:3])+1),(sum(hour_per_season[1:4])))
net_load_duration_season$HrsFall <- c(hrs_temp,rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
net_load_duration_season$Fall_MW <- c(data$net_load_solve_HydroDispatch[(sum(hour_per_season[1:3])+1):sum(hour_per_season[1:4])],rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
rm(hrs_temp)
## Second: separate net load for each season 
net_load_duration_winter <- net_load_duration_season
net_load_duration_winter <- net_load_duration_winter[,-c(2:4,6:8)]
net_load_duration_winter$HydroDispatch_MW <- rep(0,dim(net_load_duration_winter)[1])
names(net_load_duration_winter) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_winter <- net_load_duration_winter[order(-net_load_duration_winter$MW),]
net_load_duration_spring <- net_load_duration_season
net_load_duration_spring <- net_load_duration_spring[,-c(1,3:5,7:8)]
net_load_duration_spring$HydroDispatch_MW <- rep(0,dim(net_load_duration_spring)[1])
names(net_load_duration_spring) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_spring <- net_load_duration_spring[order(-net_load_duration_spring$MW),]
net_load_duration_summer <- net_load_duration_season
net_load_duration_summer <- net_load_duration_summer[,-c(1:2,4:6,8)]
net_load_duration_summer$HydroDispatch_MW <- rep(0,dim(net_load_duration_summer)[1])
names(net_load_duration_summer) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_summer <- net_load_duration_summer[order(-net_load_duration_summer$MW),]
net_load_duration_fall <- net_load_duration_season
net_load_duration_fall <- net_load_duration_fall[,-c(1:3,5:7)]
net_load_duration_fall$HydroDispatch_MW <- rep(0,dim(net_load_duration_fall)[1])
names(net_load_duration_fall) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_fall <- net_load_duration_fall[order(-net_load_duration_fall$MW),]
## Third:  Go season by season and find the hours and quantity of dispatch to use up the entire energy budget
## Call function for determining disptach of hydro.
## Winter
HydroDispatch_budget_winter <- PPCost_CurrentRegion$MWh_max_WinterJanFeb[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]+PPCost_CurrentRegion$MWh_max_WinterNovDec[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
hr_max_winter <- ceiling(HydroDispatch_budget_winter/PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")])
if (hr_max_winter>sum(hour_per_season[c(1,5)])) {
  hr_max_winter = sum(hour_per_season[c(1,5)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_winter <- PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
## Spring
HydroDispatch_budget_spring <- PPCost_CurrentRegion$MWh_max_Spring[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
hr_max_spring <- ceiling(HydroDispatch_budget_spring/PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")])
if (hr_max_spring >sum(hour_per_season[c(2)])) {
  hr_max_spring  = sum(hour_per_season[c(2)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_spring <- PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
## Summer
HydroDispatch_budget_summer <- PPCost_CurrentRegion$MWh_max_Summer[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
hr_max_summer <- ceiling(HydroDispatch_budget_summer/PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")])
if (hr_max_summer >sum(hour_per_season[c(3)])) {
  hr_max_summer  = sum(hour_per_season[c(3)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_summer <- PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
## Fall
HydroDispatch_budget_fall <- PPCost_CurrentRegion$MWh_max_Fall[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]
hr_max_fall <- ceiling(HydroDispatch_budget_fall/PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")])
if (hr_max_fall >sum(hour_per_season[c(4)])) {
  hr_max_fall  = sum(hour_per_season[c(4)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_fall <- PPCost_CurrentRegion$MW_existing[which(PPCost_CurrentRegion$Technology=="HydroDispatch")]

## Here we solve for hydro dispatch by reducing the peak net loads each season as much as possible usign HydroDispatch.
## This is done by analyzing the net load duration curve for each season.

print("In solveGEN.R: Start of solve dispatch of dispatchable hydro (no storage).")
## Fourth:  Go season by season and find the hours and quantity of dispatch to use up the entire energy budget
## Call function for determining disptach of hydro.
## Winter
HydroOutputs_winter <- function_solve_hydro_dispatch(hr_max_winter,max_capacity_winter,HydroDispatch_budget_winter,net_load_duration_winter,(hour_per_season[1]+hour_per_season[5]))
##HydroOutputs_winter <- HydroOutputs_winter[-which(HydroOutputs_winter$Hrs==0),]
## Spring
HydroOutputs_spring <- function_solve_hydro_dispatch(hr_max_spring,max_capacity_spring,HydroDispatch_budget_spring,net_load_duration_spring,hour_per_season[2])
HydroOutputs_spring <- HydroOutputs_spring[-which(HydroOutputs_spring$Hrs==0),]
## Summer
HydroOutputs_summer <- function_solve_hydro_dispatch(hr_max_summer,max_capacity_summer,HydroDispatch_budget_summer,net_load_duration_summer,hour_per_season[3])
HydroOutputs_summer <- HydroOutputs_summer[-which(HydroOutputs_summer$Hrs==0),]
## Fall
HydroOutputs_fall <- function_solve_hydro_dispatch(hr_max_fall,max_capacity_fall,HydroDispatch_budget_fall,net_load_duration_fall,hour_per_season[4])
HydroOutputs_fall <- HydroOutputs_fall[-which(HydroOutputs_fall$Hrs==0),]
#} ## else related to: if (NotEnoughHydro == 1) 
print("In solveGEN.R: End of solve dispatch of dispatchable hydro (no storage).")

## At this point in the code it is POSSIBLE that the solved HydroDispatch (e.g., in HydroOutputs_fall$HydroDispatch_MW)
## causes negative net load after accounting for the dispatch. So we correct this here.
HydroOutputs_winter$negative_net_load <- HydroOutputs_winter$MW - HydroOutputs_winter$HydroDispatch_MW
HydroOutputs_winter$negative_net_load[HydroOutputs_winter$negative_net_load>0] <- 0
HydroOutputs_winter$HydroDispatch_MW <- HydroOutputs_winter$HydroDispatch_MW + HydroOutputs_winter$negative_net_load
HydroOutputs_spring$negative_net_load <- HydroOutputs_spring$MW - HydroOutputs_spring$HydroDispatch_MW
HydroOutputs_spring$negative_net_load[HydroOutputs_spring$negative_net_load>0] <- 0
HydroOutputs_spring$HydroDispatch_MW <- HydroOutputs_spring$HydroDispatch_MW + HydroOutputs_spring$negative_net_load
HydroOutputs_summer$negative_net_load <- HydroOutputs_summer$MW - HydroOutputs_summer$HydroDispatch_MW
HydroOutputs_summer$negative_net_load[HydroOutputs_summer$negative_net_load>0] <- 0
HydroOutputs_summer$HydroDispatch_MW <- HydroOutputs_summer$HydroDispatch_MW + HydroOutputs_summer$negative_net_load
HydroOutputs_fall$negative_net_load <- HydroOutputs_fall$MW - HydroOutputs_fall$HydroDispatch_MW
HydroOutputs_fall$negative_net_load[HydroOutputs_fall$negative_net_load>0] <- 0
HydroOutputs_fall$HydroDispatch_MW <- HydroOutputs_fall$HydroDispatch_MW + HydroOutputs_fall$negative_net_load

## Fill in data from the HydroDisptach functions each season into "data$HydroDispatch_MW"
data$HydroDispatch_MW <- c(HydroOutputs_winter$HydroDispatch_MW[1:hour_per_season[1]],HydroOutputs_spring$HydroDispatch_MW[1:hour_per_season[2]],HydroOutputs_summer$HydroDispatch_MW[1:hour_per_season[3]],HydroOutputs_fall$HydroDispatch_MW[1:hour_per_season[4]],HydroOutputs_winter$HydroDispatch_MW[(hour_per_season[1]+1):(hour_per_season[1]+hour_per_season[5])])

if (NotEnoughHydro == 0) { 

  ## Here, if the user does not want to have as much total hydro MWh, HydroDispatch + HydroNonDispatch,
  ## then to meet the user's requirements, we subtract some amount of hydro generation from that already 
  ## solved as if the total amount of hydro energy (MWh) budget were used.
  user_hydro_budget <- sum(data$Load_MW)*(Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology=="HydroDispatch")] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")])
  HydroNonDispatch_budget <- PPCost_CurrentRegion$MWh_max_WinterJanFeb[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_WinterNovDec[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_Spring[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_Summer[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_Fall[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]
  if (user_hydro_budget <= HydroNonDispatch_budget) {
    data$HydroDispatch_MW <- rep(0,dim(data)[1])  ## there is no HydroDispatch if the user wants less than the energy budget of HydroNonDispatch
    ## Reduce data$HydroNonDispatch_MW by the same fraction each hour so that the total MWh from HydroNonDispatch = user_hydro_budget
    if (user_hydro_budget>0) {  ## Don't calculate a new "data$HydroNonDispatch_MW" if it is equal to zero for all hours, to avoid dividing by zero.
      fraction_to_multiply <- user_hydro_budget/sum(data$HydroNonDispatch_MW)   ## This line I think is now redundant
      data$HydroNonDispatch_MW <- fraction_to_multiply*data$HydroNonDispatch_MW ## This line I think is now redundant
    }
  } else if (user_hydro_budget > HydroNonDispatch_budget) {
    ## Keep data$HydroNonDispatch_MW as it has already been solved.
    ##
    ## Reduce data$HydroDispatch_MW by the amount desired per user input.
    ## We do this by removing MWh from HydroDispatch as much as needed or possible. We remove the smallest MW 
    ## of generation first, then the second smallest, etc., until we've removed a targeted amount of HydroDispatch.
    HydroDispatch_budget_total <- user_hydro_budget - HydroNonDispatch_budget
    data_hydro_dispatch_temp <- data.frame(data$Hour.ending,data$HydroDispatch_MW)
    names(data_hydro_dispatch_temp) <- c("Hour","HydroDispatch_MW")
    data_hydro_dispatch_temp <- data_hydro_dispatch_temp[order(-data_hydro_dispatch_temp$HydroDispatch_MW),]  ## rank the "data_hydro_dispatch_temp" from highest to lowest MW of dispatch
    hr_end <- max(which(data_hydro_dispatch_temp$HydroDispatch_MW>0))
    HydroDispatch_toRemove <- sum(data$HydroDispatch_MW) - HydroDispatch_budget_total 
    for (i in 1:hr_end) {  
      HydroDispatch_removed <- sum(data_hydro_dispatch_temp$HydroDispatch_MW)-sum(data_hydro_dispatch_temp$HydroDispatch_MW[1:(hr_end-1)])
      if (HydroDispatch_removed < HydroDispatch_toRemove) {
        hr_end = hr_end-1
      } else {
        break
      }
    }
    if (hr_end==dim(data)[1]) {
      data_hydro_dispatch_temp$HydroDispatch_MW <- data_hydro_dispatch_temp$HydroDispatch_MW[1:hr_end]
    } else {
      data_hydro_dispatch_temp$HydroDispatch_MW <- c(data_hydro_dispatch_temp$HydroDispatch_MW[1:hr_end],0*data_hydro_dispatch_temp$HydroDispatch_MW[(hr_end+1):dim(data)[1]])
    }
    data_hydro_dispatch_temp <- data_hydro_dispatch_temp[order(data_hydro_dispatch_temp$Hour),]
    data$HydroDispatch_MW <- data_hydro_dispatch_temp$HydroDispatch_MW  ## put curtailed HydroDispatch_MW into data$HydroDispatch_MW
  }
} ## if (NotEnoughHydro == 0) { 
## ++++++++++++++++
## Sequence of loops to solve for HydroDispatch (NO ANNUAL ELECTRICITY STORAGE) - END
## ++++++++++++++++

## +++++++++++++++++
## Input the TOTAL (dispatchable and non-dispatchable) MW of installed capacity for Hydro into "PP_MWneeded" under "HydroDispatch"
## Here this assumes there is no electricity storage of any kind.
## +++++++++++++++++
PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="HydroDispatch")] <- max(data$HydroDispatch_MW + data$HydroNonDispatch_MW)


## +++++++++++++++++
## Now determine the amount of NGCC and NGCT that are needed to meet the remaining
## "net load" by using the typical screening curve method ONLY to determine the 
## MW capacity of NGCC and NGCT.
## Create the "current" load duration cuve to solve for NGCC and NGCT amounts.
## Here this assumes there is no electricity storage of any kind.
## +++++++++++++++++
## At this point in the code, "frac_MWh_withoutNG_actual" equation should be true since data$NGCC and data$NGCT are all zero at this point
frac_MWh_withoutNG_actual <- (sum(noncurtailed_WindSolar)-sum(curtailed_WindSolar)+sum(data$Coal_MW)+sum(data$NGCC_MW)+sum(data$NGCT_MW)+sum(data$Nuclear_MW)+sum(data$UserTech1_MW)+sum(data$UserTech2_MW)+sum(data$Geothermal_MW)+sum(data$Biomass_MW)+sum(data$PetroleumCC_MW)+sum(data$HydroDispatch_MW)+sum(data$HydroNonDispatch_MW))/sum(data$Load_MW)
frac_MWh_withoutNG_target <- sum(Frac_MWhDesired_dispatchable$Fraction_MWhDesired)+sum(Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired)
data$generation_non_NG <- noncurtailed_WindSolar-curtailed_WindSolar+data$Coal_MW+data$Nuclear_MW+data$UserTech1_MW+data$UserTech2_MW+data$Biomass_MW+data$HydroDispatch_MW+data$HydroNonDispatch_MW+data$PetroleumCC_MW+data$Geothermal_MW
data$net_load_solveNG <- data$Load_MW - data$generation_non_NG
net_load_duration_solveNG <- data$net_load_solveNG
net_load_duration_solveNG <- net_load_duration_solveNG[order(-net_load_duration_solveNG)]
## find range of data$hour.ending when NGCC is lowest cost and/or NGCT is lowest cost.
ind.NGCC <- which(PPCost_US$Technology==c("NGCC"))
ind.NGCT <- which(PPCost_US$Technology==c("NGCT"))
data$NGCC_Cost_8760 <- PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCC] + data$Hour.ending*((PPCost_US$VariableOMCost_..MWh[ind.NGCC]+PPCost_US$VariableFuelCost_..MWh[ind.NGCC])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
data$NGCT_Cost_8760 <- PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCT] + data$Hour.ending*((PPCost_US$VariableOMCost_..MWh[ind.NGCT]+PPCost_US$VariableFuelCost_..MWh[ind.NGCT])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
ind.lowest_cost_NGCC <- which(data$NGCC_Cost_8760<=data$NGCT_Cost_8760)
ind.lowest_cost_NGCT <- which(data$NGCT_Cost_8760<data$NGCC_Cost_8760)

## +++++++++++++++++
## Now solve for the MW capacity needed for NGCC and NGCT.
## Here this assumes there is no electricity storage of any kind.
## +++++++++++++++++
ind.PP_MWneeded_NGCC <- which(PP_MWneeded$Technology==c("NGCC"))
ind.PP_MWneeded_NGCT <- which(PP_MWneeded$Technology==c("NGCT"))
if (min(ind.lowest_cost_NGCT) <= min(ind.lowest_cost_NGCC)) { ## Then NGCT serves the peak net_load and NGCC starts serving remaining net_load from the minimum net_load value 
  # find capacity of NGCC first:
  net_load.min_temp <- max(0,min(net_load_duration_solveNG))
  net_load.max_temp <- net_load_duration_solveNG[min(ind.lowest_cost_NGCC)]
  PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCC] <- net_load.max_temp - net_load.min_temp
  # Determine generation from NGCC each hour. 
  # This is equal to all of "data$net_load_solveNG" that is <= the just-determined MW capacity of NGCC
  data$NGCC_MW <- data$net_load_solveNG
  data$NGCC_MW[data$NGCC_MW>=PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCC]] <- PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCC]
  data$NGCC_MW[data$NGCC_MW<0] <- 0
  # find capacity of NGCT second:
  PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCT] <- max(net_load_duration_solveNG) - net_load.max_temp
  # Determine generation from NGCT each hour. 
  # This is equal to all of "data$net_load_solveNG" minus data$NGCC_MW
  data$NGCT_MW <- data$net_load_solveNG - data$NGCC_MW
} else if (min(ind.lowest_cost_NGCC) < min(ind.lowest_cost_NGCT)) { ## Then NGCC serves the peak net_load and NGCT starts serving remaining net_load from the minimum net_load value 
  # find capacity of NGCT first:
  net_load.min_temp <- min(net_load_duration_solveNG)
  net_load.max_temp <- net_load_duration_solveNG[min(ind.lowest_cost_NGCT)]
  PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCT] <- net_load.max_temp - net_load.min_temp
  # Determine generation from NGCT each hour. 
  # This is equal to all of "data$net_load_solveNG" that is <= the just-determined MW capacity of NGCT
  data$NGCT_MW <- data$net_load_solveNG
  data$NGCT_MW[data$NGCT_MW>=PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCT]] <- PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCT]
  data$NGCT_MW[data$NGCT_MW<0] <- 0
  # find capacity of NGCC second:
  PP_MWneeded$MW_needed[ind.PP_MWneeded_NGCC] <- max(net_load_duration_solveNG) - net_load.max_temp
  # Determine generation from NGCC each hour. 
  # This is equal to all of "data$net_load_solveNG" minus data$NGCT_MW
  data$NGCC_MW <- data$net_load_solveNG - data$NGCT_MW
}
rm(net_load.min_temp,net_load.max_temp)

## +++++++++++++++
## calculate actual % of total generation from dispatchable generators.
## Here this assumes there is no electricity storage of any kind.
## +++++++++++++++
PP_MWneeded <- cbind(PP_MWneeded,PP_MWneeded)
PP_MWneeded <- PP_MWneeded[,-c(5,6,7)]
names(PP_MWneeded)[5] <- c("Fraction_MWhActual")
PP_MWneeded$Fraction_MWhActual <- 0
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Coal")] <- sum(data$Coal_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="NGCC")] <- sum(data$NGCC_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="NGCT")] <- sum(data$NGCT_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="UserTech1")] <- sum(data$UserTech1_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="UserTech2")] <- sum(data$UserTech2_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Nuclear")] <- sum(data$Nuclear_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Wind")] <- sum(data$Wind_MW_total-curtailed_Wind)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="PV")] <- sum(data$PV_MW_total-curtailed_PV)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="CSP")] <- sum(data$CSP_MW_total-curtailed_CSP)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Biomass")] <- sum(data$Biomass_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Geothermal")] <- sum(data$Geothermal_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="PetroleumCC")] <- sum(data$PetroleumCC_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="HydroDispatch")] <- sum(data$HydroDispatch_MW)/sum(data$Load_MW)
PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="HydroNonDispatch")] <- sum(data$HydroNonDispatch_MW)/sum(data$Load_MW)
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## CALCULATIONS FOR GENERATION CAPACITY (not nuclear or non-dispatchable hydro)
## ASSUMING NO ELECTRICITY STORAGE - END
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++


## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## CALCULATIONS FOR GENERATION CAPACITY (not nuclear or non-dispatchable hydro)
## ASSUMING ANNUAL ELECTRICITY STORAGE - BEGIN
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++

## +++++++++++++++
## Call function to optimize the capacity needed for wind and solar PV 
## WITH ANNUAL STORAGE
## +++++++++++++++
## Solve for PV and Wind using gradient based search
## To make the gradient-based optimization work well, the initial conditions 
## need to be scaled with the desired fractions of wind and solar.

multiplier_init.wind <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("Wind"))]  ## Wind
multiplier_init.PV <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("PV"))] ## Solar PV
multiplier_init.CSP <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("CSP"))] ## Solar CSP
assign('efficiency_OneWay_DailyStorage',sqrt(.85))  ## Make the storage efficiency a global variable
lb = c(0,0,0)   ## lower bound on multipliers for wind and solar profiles and storage parameters
ub = c(constrained_capacity_NoStorage.Wind,constrained_capacity_NoStorage.PV,constrained_capacity_NoStorage.CSP)   ## upper bound on multipliers for wind and solar profiles
init_guesses <- c(multiplier_init.wind,multiplier_init.PV,multiplier_init.CSP)
max_iters <- 200
tolerance_AnnualStorage <- 1e10
print("In solveGEN.R: Start optimize wind and solar capacity (annual storage).")
wind_solar_AnnualStorage_multipliers <- optim(par=init_guesses,fn=function_Wind_PV_CSP_annual_storage,lower=lb,upper=ub,method = c("L-BFGS-B"),control=list(maxit=max_iters,factr=tolerance_AnnualStorage))
print("In solveGEN.R: Completed optimize wind and solar capacity (annual storage).")
multiplier_WithAnnualStorage.wind <- wind_solar_AnnualStorage_multipliers$par[1]
multiplier_WithAnnualStorage.PV   <- wind_solar_AnnualStorage_multipliers$par[2]
multiplier_WithAnnualStorage.CSP  <- wind_solar_AnnualStorage_multipliers$par[3] # make these zero if desired fraction is zero


print("In solveGEN.R: Start of solve dispatch of annual storage.")
## +++++++++++++++
## POST PROCESSING TO GET DISPATCHED STORAGE - BEGIN
## from "function_Wind_PV_CSP_annual_storage"
## +++++++++++++++
## Calculate the net load (in absoluate MW each hour) that needs to
## be served by the dispatchable generators after accounting for
## wind and solar (with no electricity storage).
data$Wind_MW_WithAnnualStorage_total <- multiplier_WithAnnualStorage.wind*data$Wind_MW  ## total Wind output per hour, no curtailment
data$PV_MW_WithAnnualStorage_total   <- multiplier_WithAnnualStorage.PV*data$SolarPV_MW  ## total PV output per hour, no curtailment
data$CSP_MW_WithAnnualStorage_total  <- multiplier_WithAnnualStorage.CSP*data$SolarCSP_MW  ## total CSP output per hour, no curtailment
data$net_load_WithAnnualStorageNoCurtailment <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW - data$Wind_MW_WithAnnualStorage_total - data$PV_MW_WithAnnualStorage_total - data$CSP_MW_WithAnnualStorage_total
data$net_load_WithAnnualStorageWithCurtailment <- data$net_load_WithAnnualStorageNoCurtailment
data$net_load_WithAnnualStorageWithCurtailment[data$net_load_WithAnnualStorageWithCurtailment<0]=0

hrs_per_year <- 8760
net_load_duration <- matrix(0, ncol = 1, nrow = hrs_per_year)  ## Initialize a data frame that will be the daily net load duration curve that is updated as the dispatch function is called for each day of the year
net_load_duration <- data.frame(net_load_duration)
net_load_duration$MW <- data$net_load_WithAnnualStorageWithCurtailment
net_load_duration <- net_load_duration[-c(1)]
net_load_duration$Hrs <- data$Hour.ending
net_load_duration$HydroDispatch_MW <- rep(0,dim(net_load_duration)[1])  ## here the term "HydroDispatch" is used becuase we use the "function_solve_hydro_dispatch" that already assumed that terminology
net_load_duration <- net_load_duration[order(-net_load_duration$MW),]
curtailed_WindSolar_WithAnnualStorage <- data$net_load_WithAnnualStorageNoCurtailment
curtailed_WindSolar_WithAnnualStorage[curtailed_WindSolar_WithAnnualStorage>0]=0
curtailed_WindSolar_WithAnnualStorage <- -curtailed_WindSolar_WithAnnualStorage
curtailed_WindSolar_WithAnnualStorage_AnnualTWh <- sum(curtailed_WindSolar_WithAnnualStorage)/1e6
MW_capacity_AnnualStorage <- max(curtailed_WindSolar_WithAnnualStorage)  ## The MW capacity of the storage system is assumed = that require to store the maxim MW of curtailed Wind+PV+CSP
StoredWindSolar_AnnualStorage <- curtailed_WindSolar_WithAnnualStorage*efficiency_OneWay_DailyStorage^2                ## First, assume all curtailed wind and solar is stored, subtracting MWh due to efficiency loss
StoredWindSolar_AnnualStorage[StoredWindSolar_AnnualStorage>MW_capacity_AnnualStorage] <- MW_capacity_AnnualStorage  ## Next, this says I can't store energy faster in any given hour, due to power constraint, than the MW power capacity of the storage system which is set by the highest MW (in all hours of the year) of all curtailed renewables needed to be stored.

noncurtailed_WindSolar_WithAnnualStorage <- data$Wind_MW_WithAnnualStorage_total+data$PV_MW_WithAnnualStorage_total+data$CSP_MW_WithAnnualStorage_total
wind_fraction_WithAnnualStorage = data$Wind_MW_WithAnnualStorage_total/noncurtailed_WindSolar_WithAnnualStorage
PV_fraction_WithAnnualStorage = data$PV_MW_WithAnnualStorage_total/noncurtailed_WindSolar_WithAnnualStorage
CSP_fraction_WithAnnualStorage = data$CSP_MW_WithAnnualStorage_total/noncurtailed_WindSolar_WithAnnualStorage
curtailed_Wind_WithAnnualStorage <- curtailed_WindSolar_WithAnnualStorage*wind_fraction_WithAnnualStorage
curtailed_PV_WithAnnualStorage   <- curtailed_WindSolar_WithAnnualStorage*PV_fraction_WithAnnualStorage
curtailed_CSP_WithAnnualStorage  <- curtailed_WindSolar_WithAnnualStorage*CSP_fraction_WithAnnualStorage
curtailed_Wind_WithAnnualStorage[is.na(curtailed_Wind_WithAnnualStorage)==TRUE] <- 0  ## ensure there are no NA from dividing by zero
curtailed_PV_WithAnnualStorage[is.na(curtailed_PV_WithAnnualStorage)==TRUE] <- 0      ## ensure there are no NA from dividing by zero
curtailed_CSP_WithAnnualStorage[is.na(curtailed_CSP_WithAnnualStorage)==TRUE] <- 0    ## ensure there are no NA from dividing by zero
data$StoredWind_WithAnnualStorage <- StoredWindSolar_AnnualStorage*curtailed_Wind_WithAnnualStorage/(curtailed_Wind_WithAnnualStorage+curtailed_PV_WithAnnualStorage+curtailed_CSP_WithAnnualStorage)
data$StoredPV_WithAnnualStorage   <- StoredWindSolar_AnnualStorage*curtailed_PV_WithAnnualStorage  /(curtailed_Wind_WithAnnualStorage+curtailed_PV_WithAnnualStorage+curtailed_CSP_WithAnnualStorage)
data$StoredCSP_WithAnnualStorage  <- StoredWindSolar_AnnualStorage*curtailed_CSP_WithAnnualStorage /(curtailed_Wind_WithAnnualStorage+curtailed_PV_WithAnnualStorage+curtailed_CSP_WithAnnualStorage)
data$StoredWind_WithAnnualStorage[is.nan(data$StoredWind_WithAnnualStorage)=="TRUE"]=0
data$StoredPV_WithAnnualStorage[is.nan(data$StoredPV_WithAnnualStorage)=="TRUE"]=0
data$StoredCSP_WithAnnualStorage[is.nan(data$StoredCSP_WithAnnualStorage)=="TRUE"]=0
data$Wind_MW_DirectToGrid_AnnualStorage <-  data$Wind_MW_WithAnnualStorage_total - curtailed_Wind_WithAnnualStorage
data$PV_MW_DirectToGrid_AnnualStorage <-  data$PV_MW_WithAnnualStorage_total - curtailed_PV_WithAnnualStorage
data$CSP_MW_DirectToGrid_AnnualStorage <-  data$CSP_MW_WithAnnualStorage_total - curtailed_CSP_WithAnnualStorage
frac.wind_WithAnnualStorage <- sum(data$Wind_MW_DirectToGrid_AnnualStorage,data$StoredWind_WithAnnualStorage)/sum(data$Load_MW)
frac.PV_WithAnnualStorage <- sum(data$PV_MW_DirectToGrid_AnnualStorage,data$StoredPV_WithAnnualStorage)/sum(data$Load_MW)
frac.CSP_WithAnnualStorage <- sum(data$CSP_MW_DirectToGrid_AnnualStorage,data$StoredCSP_WithAnnualStorage)/sum(data$Load_MW)
ObjectiveFunctionTest <-   sum(1e7*( (frac.wind_WithAnnualStorage-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Wind")])^2 + (frac.PV_WithAnnualStorage-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="PV")])^2 + (frac.CSP_WithAnnualStorage-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="CSP")])^2 ) )

if (MW_capacity_AnnualStorage>0) {
  hr_max <- min((dim(data)[1]-1),ceiling(sum(StoredWindSolar_AnnualStorage)/MW_capacity_AnnualStorage)) ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hours in the day
  hr_max <- max(hr_max,0) ## ensure there is no negative "hr_max".
  storage_annual_outputs <- function_solve_hydro_dispatch(hr_max,MW_capacity_AnnualStorage,sum(StoredWindSolar_AnnualStorage),net_load_duration,hrs_per_year)
  data$Dispatched_StoredWindSolar_MW <- storage_annual_outputs$HydroDispatch_MW
} else {
  data$Dispatched_StoredWindSolar_MW <- rep(0,dim(data)[1])
}
StoredWindSolar_AnnualStorage_cumulative <- cumsum(StoredWindSolar_AnnualStorage) ## Cumulative storage of wind and solar MWh
SOC_hourly_WithAnnualStorage <- StoredWindSolar_AnnualStorage_cumulative - cumsum(data$Dispatched_StoredWindSolar_MW) ## hourly state of charge of storage
if (min(SOC_hourly_WithAnnualStorage)<0) {
  SOC_hourly_WithAnnualStorage <- SOC_hourly_WithAnnualStorage + max(0,-min(SOC_hourly_WithAnnualStorage[SOC_hourly_WithAnnualStorage<0]))  ## Adjust SOC_hourly UPWARD to make the minimum SOC = 0 and ensure it is never negative (e.g,. the intital state of charge is suffient to prevent negative state of charge at any hour)
} else {
  SOC_hourly_WithAnnualStorage <- SOC_hourly_WithAnnualStorage - max(0,min(SOC_hourly_WithAnnualStorage))   ## Adjust SOC_hourly DOWNWARD to make the minimum SOC = 0
}
SOC_hourly_WithAnnualStorage_init <- SOC_hourly_WithAnnualStorage[1]
## +++++++++++++++
## POST PROCESSING TO GET DISPATCHED STORAGE - END
## from "function_Wind_PV_CSP_annual_storage"
## +++++++++++++++
print("In solveGEN.R: End of solve dispatch of annual storage.")


## +++++++++++++++++
## BEGIN
## Perform optimization to find the capacity of dispatchables in the actual time series of net_load.
## This for loop solves for dispatchable generation when there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++
## Go through each power plant type to determine its capacity, for power plants that are not 
## (1) wind, solar, nuclear, and other assumed non-dispatchable power plants
## (2) natural gas combined cycle (NGCC) or natural gas combustion turbine (NGCT), since these are determined later, or
## (3) dispatchable hydro (HydroDispatch) as this is solved for right before solving for NG capacity.
PP_MWneeded_AnnualStorage <- PP_MWneeded  ## make a new PP_MWneeded data frame that stores results when assuming Annual Electricity Storage
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="Geothermal")] <- 0  ## Since "PP_MWneeded_AnnualStorage" was copied, make all previously solved dispatchable capacities = 0 
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="Biomass")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="Coal")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="HydroDispatch")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="UserTech1")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="UserTech2")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="PetroleumCC")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="NGCC")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="NGCT")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="Wind")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="CSP")] <- 0
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded_AnnualStorage$Technology=="PV")] <- 0

print("In solveGEN.R: Start of solve dispatch of dispatchable technologies (annual storage).")
for (i in 1:(dim(Frac_MWhDesired_dispatchable)[1]-2)) {  ## Subtract 2 for 1) NGCC and 2) NGCT
  new_column_number <- dim(data)[2]+1
  data[,new_column_number] <- 0*data$Load_MW
  names(data)[new_column_number] <- paste0(PP_MWneeded_AnnualStorage$Technology[i],"_AnnualStorage_MW")
  assign("PPnumber",i) ## Find capacity of the 1st cheapest dispatchable generator (i=1) with Frac_MWhDesired > 0
  
  ## ++++++++++++++
  ## Call optimization to determine the MW capacity of dispatchable technologies 
  ## chosen in order from cheapest to most expensive technology to run at 100% capacity.
  ## ++++++++++++++
  multiplier_init.dispatch <- 0.1  ## initial guess at how much to multiply existing PV profile
  lb = c(0)   ## lower bound on multipliers for wind and solar profiles
  ub = c(9.999e9)  ## make an abitrarily large number of 9.999 x 10^9 MW that will hold for all technologies except geothermal (per how algorithm works: biomass is limited by MWh, not MW)
  init_guesses <- c(multiplier_init.dispatch)
  
  if (names(data)[new_column_number] != "HydroDispatch_MW") {
    if (names(data)[new_column_number] == "Geothermal_AnnualStorage_MW") {
      ub = c(geothermal_MW_max)
    }
    ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible later in the code.
    MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_generators_AnnualStorage,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE))
    
    # Make any PP_MWneeded$MW_needed < a certain threshold just become zero.
    # Then all generation from that technology is zero each hour of the year.
    if (MWcapacity_multiplier$par > 1e-6) {
      PP_MWneeded_AnnualStorage$MW_needed[i] <- MWcapacity_multiplier$par
    } else {
      PP_MWneeded_AnnualStorage$MW_needed[i] <-0
    }
  } else { ## Then names(data)[PPindex+i] == "HydroDispatch_MW"
    MWcapacity_multiplier$par <- 0
    PP_MWneeded_AnnualStorage$MW_needed[i] <- 0
  }## if (names(data)[PPindex+i] != "HydroDispatch_MW") {
  
  ## ++++++++++++++
  ## Need to recalculate what is left for "net load" after removing the generation
  ## from the dispatchable generators that have already been determined at this point.
  ## ++++++++++++++
  MWgeneration_temp <- PP_MWneeded_AnnualStorage$MW_needed[i]*rep(1,length(data$net_load_WithAnnualStorageWithCurtailment))  
  ## remove generation each hour that is above net load (generator must operate < 100% capacity)
  net_load_minus_MWgeneration <- data$net_load_WithAnnualStorageWithCurtailment - MWgeneration_temp
  MWgeneration_to_subtract <- net_load_minus_MWgeneration
  MWgeneration_to_subtract[MWgeneration_to_subtract>0] <- 0
  MWgeneration <- MWgeneration_temp + MWgeneration_to_subtract
  ## Make a correction if you are over the limit on MWgeneration for Biomass
  if (PP_MWneeded_AnnualStorage$Technology[i] == "Biomass" & sum(MWgeneration)>MWh_max_biomass) {
    biomass_reduction_factor_AnnualStorage <- MWh_max_biomass/sum(MWgeneration)
    MWgeneration <- biomass_reduction_factor_AnnualStorage*MWgeneration # Reduce generation each hour by the same fraction
    PP_MWneeded_AnnualStorage$MW_needed[i] <- biomass_reduction_factor_AnnualStorage*PP_MWneeded_AnnualStorage$MW_needed[i] # Reduce Power Plant capacity of Biomass by neccesary factor
  } else {
    biomass_reduction_factor_AnnualStorage <- 0
  }
  data$net_load_WithAnnualStorageWithCurtailment <- data$net_load_WithAnnualStorageWithCurtailment - MWgeneration
  data[,new_column_number] <- MWgeneration
  rm(MWgeneration_temp,MWgeneration_to_subtract,MWgeneration)
} ## for (i in 1:dim(Frac_MWhDesired_dispatchable)[1]) {
## +++++++++++++++++
## END
## Perform optimization to find the capacity of dispatchables in the actual time series of net_load.
## This for loop solves for dispatchable generation when there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++
print("In solveGEN.R: End of solve dispatch of dispatchable technologies (annual storage).")



## +++++++++++++++++
## Solving for data$HydroDispatch_AnnualStorage_MW -- BEGIN
## +++++++++++++++++
data$HydroDispatch_AnnualStorage_MW <- 0*data$HydroDispatch_MW

## ++++++++++++
## Solving for DispatchableHydro JUST BEFORE solving for NG power plants but AFTER solving for 
## output from all power plants .
## Here we assume that DispatchableHydro removes peak loads as much as possible, as a simple
## way to "dispatch" in a way that alleviates peaking capacity on the grid and occurs at times of highest marginal cost.
## Here this assumes there is ANNUAL ELECTRICITY STORAGE.
## ++++++++++++
data$generation_non_NG_HydroDispatch_AnnualStorage <- data$Wind_MW_DirectToGrid_AnnualStorage + data$PV_MW_DirectToGrid_AnnualStorage + data$CSP_MW_DirectToGrid_AnnualStorage +
  data$Dispatched_StoredWindSolar_MW +
  data$Nuclear_MW+data$HydroNonDispatch_MW+  ## Both Nuclear and HydroDispatch are the same WITH and WITHOUT ANNUAL ELECTRICITY STORAGE
  data$Coal_AnnualStorage_MW+data$UserTech1_AnnualStorage_MW+data$UserTech2_AnnualStorage_MW+data$Biomass_AnnualStorage_MW+data$HydroDispatch_AnnualStorage_MW+data$PetroleumCC_AnnualStorage_MW+data$Geothermal_AnnualStorage_MW
data$net_load_solve_HydroDispatch_AnnualStorage <- data$Load_MW - data$generation_non_NG_HydroDispatch_AnnualStorage
## Sometimes data$net_load_solve_HydroDispatch is < 0 within a small tolerance, so turn these into zero to avoid problems of sorting later
data$net_load_solve_HydroDispatch_AnnualStorage[(data$net_load_solve_HydroDispatch_AnnualStorage<0)&(data$net_load_solve_HydroDispatch_AnnualStorage>-1e-10)]=0


## +++++++++++
## Initializing information for solving for HydroDispatch.
## Here this assumes there IS ANNUAL electricity storage (stores all excess wind, PV, and CSP for discharge at highest net load times during the year).
## +++++++++++
## First: Create a "net load duration curve" for each season. Separate net load for each season
net_load_duration_season <- matrix(0, ncol = 4, nrow = max(c((hour_per_season[1]+hour_per_season[5]),hour_per_season[2],hour_per_season[3],hour_per_season[4])))  ## ncol= 4 seasons (combine Jan/Feb with Nov/Dec for 1 Winter); nrow = maximum number of days in any season
net_load_duration_season <- data.frame(net_load_duration_season)
names(net_load_duration_season) <- c("Winter_MW","Spring_MW","Summer_MW","Fall_MW")
net_load_duration_season$Winter_MW <- c(data$net_load_solve_HydroDispatch_AnnualStorage[1:hour_per_season[1]],data$net_load_solve_HydroDispatch_AnnualStorage[(sum(hour_per_season[1:4])+1):sum(hour_per_season[1:5])])
net_load_duration_season$HrsWinter <- c(seq(1,hour_per_season[1]),seq((sum(hour_per_season[1:4])+1),(sum(hour_per_season[1:5])),1))
hrs_temp <- seq((sum(hour_per_season[1])+1),(sum(hour_per_season[1:2])))
net_load_duration_season$HrsSpring <- c(hrs_temp,rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
net_load_duration_season$Spring_MW <- c(data$net_load_solve_HydroDispatch_AnnualStorage[(sum(hour_per_season[1])+1):sum(hour_per_season[1:2])],rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
rm(hrs_temp)
hrs_temp <- seq((sum(hour_per_season[1:2])+1),(sum(hour_per_season[1:3])))
net_load_duration_season$HrsSummer <- c(hrs_temp,rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
net_load_duration_season$Summer_MW <- c(data$net_load_solve_HydroDispatch_AnnualStorage[(sum(hour_per_season[1:2])+1):sum(hour_per_season[1:3])],rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
rm(hrs_temp)
hrs_temp <- seq((sum(hour_per_season[1:3])+1),(sum(hour_per_season[1:4])))
net_load_duration_season$HrsFall <- c(hrs_temp,rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
net_load_duration_season$Fall_MW <- c(data$net_load_solve_HydroDispatch_AnnualStorage[(sum(hour_per_season[1:3])+1):sum(hour_per_season[1:4])],rep(0,(dim(net_load_duration_season)[1]-length(hrs_temp))))
rm(hrs_temp)
## Second: separate net load for each season 
net_load_duration_winter <- net_load_duration_season
net_load_duration_winter <- net_load_duration_winter[,-c(2:4,6:8)]
net_load_duration_winter$HydroDispatch_MW <- rep(0,dim(net_load_duration_winter)[1])
names(net_load_duration_winter) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_winter <- net_load_duration_winter[order(-net_load_duration_winter$MW),]
net_load_duration_spring <- net_load_duration_season
net_load_duration_spring <- net_load_duration_spring[,-c(1,3:5,7:8)]
net_load_duration_spring$HydroDispatch_MW <- rep(0,dim(net_load_duration_spring)[1])
names(net_load_duration_spring) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_spring <- net_load_duration_spring[order(-net_load_duration_spring$MW),]
net_load_duration_summer <- net_load_duration_season
net_load_duration_summer <- net_load_duration_summer[,-c(1:2,4:6,8)]
net_load_duration_summer$HydroDispatch_MW <- rep(0,dim(net_load_duration_summer)[1])
names(net_load_duration_summer) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_summer <- net_load_duration_summer[order(-net_load_duration_summer$MW),]
net_load_duration_fall <- net_load_duration_season
net_load_duration_fall <- net_load_duration_fall[,-c(1:3,5:7)]
net_load_duration_fall$HydroDispatch_MW <- rep(0,dim(net_load_duration_fall)[1])
names(net_load_duration_fall) = c("MW","Hrs","HydroDispatch_MW")
net_load_duration_fall <- net_load_duration_fall[order(-net_load_duration_fall$MW),]

print("In solveGEN.R: Start of solve dispatch of dispatchable hydro (annual storage).")
## Third:  Go season by season and find the hours and quantity of dispatch to use up the entire energy budget
## Call function for determining disptach of hydro.
HydroOutputs_winter[,] <- 0  ## reset this variable for when it was solved without assuming Annual Electricity Storage
HydroOutputs_spring[,] <- 0  ## reset this variable for when it was solved without assuming Annual Electricity Storage
HydroOutputs_summer[,] <- 0  ## reset this variable for when it was solved without assuming Annual Electricity Storage
HydroOutputs_fall[,] <- 0    ## reset this variable for when it was solved without assuming Annual Electricity Storage
## Winter
HydroOutputs_winter <- function_solve_hydro_dispatch(hr_max_winter,max_capacity_winter,HydroDispatch_budget_winter,net_load_duration_winter,(hour_per_season[1]+hour_per_season[5]))
## Spring
HydroOutputs_spring <- function_solve_hydro_dispatch(hr_max_spring,max_capacity_spring,HydroDispatch_budget_spring,net_load_duration_spring,hour_per_season[2])
HydroOutputs_spring <- HydroOutputs_spring[-which(HydroOutputs_spring$Hrs==0),]
## Summer
HydroOutputs_summer <- function_solve_hydro_dispatch(hr_max_summer,max_capacity_summer,HydroDispatch_budget_summer,net_load_duration_summer,hour_per_season[3])
HydroOutputs_summer <- HydroOutputs_summer[-which(HydroOutputs_summer$Hrs==0),]
## Fall
HydroOutputs_fall <- function_solve_hydro_dispatch(hr_max_fall,max_capacity_fall,HydroDispatch_budget_fall,net_load_duration_fall,hour_per_season[4])
HydroOutputs_fall <- HydroOutputs_fall[-which(HydroOutputs_fall$Hrs==0),]
print("In solveGEN.R: End of solve dispatch of dispatchable hydro (no storage).")

## At this point in the code it is POSSIBLE that the solved HydroDispatch (e.g., in HydroOutputs_fall$HydroDispatch_MW)
## causes negative net load after accounting for the dispatch. So we correct this here.
HydroOutputs_winter$negative_net_load <- HydroOutputs_winter$MW - HydroOutputs_winter$HydroDispatch_MW
HydroOutputs_winter$negative_net_load[HydroOutputs_winter$negative_net_load>0] <- 0
HydroOutputs_winter$HydroDispatch_MW <- HydroOutputs_winter$HydroDispatch_MW + HydroOutputs_winter$negative_net_load
HydroOutputs_spring$negative_net_load <- HydroOutputs_spring$MW - HydroOutputs_spring$HydroDispatch_MW
HydroOutputs_spring$negative_net_load[HydroOutputs_spring$negative_net_load>0] <- 0
HydroOutputs_spring$HydroDispatch_MW <- HydroOutputs_spring$HydroDispatch_MW + HydroOutputs_spring$negative_net_load
HydroOutputs_summer$negative_net_load <- HydroOutputs_summer$MW - HydroOutputs_summer$HydroDispatch_MW
HydroOutputs_summer$negative_net_load[HydroOutputs_summer$negative_net_load>0] <- 0
HydroOutputs_summer$HydroDispatch_MW <- HydroOutputs_summer$HydroDispatch_MW + HydroOutputs_summer$negative_net_load
HydroOutputs_fall$negative_net_load <- HydroOutputs_fall$MW - HydroOutputs_fall$HydroDispatch_MW
HydroOutputs_fall$negative_net_load[HydroOutputs_fall$negative_net_load>0] <- 0
HydroOutputs_fall$HydroDispatch_MW <- HydroOutputs_fall$HydroDispatch_MW + HydroOutputs_fall$negative_net_load

## Fill in data from the HydroDisptach functions each season into "data$HydroDispatch_MW"
data$HydroDispatch_AnnualStorage_MW <- c(HydroOutputs_winter$HydroDispatch_MW[1:hour_per_season[1]],HydroOutputs_spring$HydroDispatch_MW[1:hour_per_season[2]],HydroOutputs_summer$HydroDispatch_MW[1:hour_per_season[3]],HydroOutputs_fall$HydroDispatch_MW[1:hour_per_season[4]],HydroOutputs_winter$HydroDispatch_MW[(hour_per_season[1]+1):(hour_per_season[1]+hour_per_season[5])])

if (NotEnoughHydro == 0) { 
  # ## Then solve for some level of HydroDispatch that is < total energy budget possible
  # ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible.
  # MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_hydro_seasonally,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE))
  
  ## Here, if the user does not want to have as much total hydro MWh, HydroDispatch + HydroNonDispatch,
  ## then to meet the user's requirements, we subtract some amount of hydro generation from that already 
  ## solved as if the total amount of hydro energy (MWh) budget were used.
  user_hydro_budget <- sum(data$Load_MW)*(Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology=="HydroDispatch")] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")])
  HydroNonDispatch_budget <- PPCost_CurrentRegion$MWh_max_WinterJanFeb[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_WinterNovDec[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_Spring[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_Summer[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]+PPCost_CurrentRegion$MWh_max_Fall[which(PPCost_CurrentRegion$Technology=="HydroNonDispatch")]
  if (user_hydro_budget <= HydroNonDispatch_budget) {
    data$HydroDispatch_AnnualStorage_MW <- rep(0,dim(data)[1])  ## there is no HydroDispatch if the user wants less than the energy budget of HydroNonDispatch
    ## Reduce data$HydroNonDispatch_MW by the same fraction each hour so that the total MWh from HydroNonDispatch = user_hydro_budget
    if (user_hydro_budget>0) {  ## Don't calculate a new "data$HydroNonDispatch_MW" if it is equal to zero for all hours, to avoid dividing by zero.
      fraction_to_multiply <- user_hydro_budget/sum(data$HydroNonDispatch_MW)   ## This line I think is now redundant
      data$HydroNonDispatch_MW <- fraction_to_multiply*data$HydroNonDispatch_MW ## This line I think is now redundant
    }
  } else if (user_hydro_budget > HydroNonDispatch_budget) {
    ## Keep data$HydroNonDispatch_MW as it has already been solved.
    ##
    ## Reduce data$HydroDispatch_MW by the amount desired per user input.
    ## We do this by removing MWh from HydroDispatch as much as needed or possible. We remove the smallest MW 
    ## of generation first, then the second smallest, etc., until we've removed a targeted amount of HydroDispatch.
    HydroDispatch_budget_total <- user_hydro_budget - HydroNonDispatch_budget
    data_hydro_dispatch_temp <- data.frame(data$Hour.ending,data$HydroDispatch_AnnualStorage_MW)
    names(data_hydro_dispatch_temp) <- c("Hour","HydroDispatch_MW")
    data_hydro_dispatch_temp <- data_hydro_dispatch_temp[order(-data_hydro_dispatch_temp$HydroDispatch_MW),]  ## rank the "data_hydro_dispatch_temp" from highest to lowest MW of dispatch
    hr_end <- max(which(data_hydro_dispatch_temp$HydroDispatch_MW>0))
    HydroDispatch_toRemove <- sum(data$HydroDispatch_AnnualStorage_MW) - HydroDispatch_budget_total 
    for (i in 1:hr_end) {  
      HydroDispatch_removed <- sum(data_hydro_dispatch_temp$HydroDispatch_MW)-sum(data_hydro_dispatch_temp$HydroDispatch_MW[1:(hr_end-1)])
      if (HydroDispatch_removed < HydroDispatch_toRemove) {
        hr_end = hr_end-1
      } else {
        break
      }
    }
    if (hr_end==dim(data)[1]) {
      data_hydro_dispatch_temp$HydroDispatch_MW <- data_hydro_dispatch_temp$HydroDispatch_MW[1:hr_end]
    } else {
      data_hydro_dispatch_temp$HydroDispatch_MW <- c(data_hydro_dispatch_temp$HydroDispatch_MW[1:hr_end],0*data_hydro_dispatch_temp$HydroDispatch_MW[(hr_end+1):dim(data)[1]])
    }
    data_hydro_dispatch_temp <- data_hydro_dispatch_temp[order(data_hydro_dispatch_temp$Hour),]
    data$HydroDispatch_AnnualStorage_MW <- data_hydro_dispatch_temp$HydroDispatch_MW  ## put curtailed HydroDispatch_MW into data$HydroDispatch_MW
  }
} ## if (NotEnoughHydro == 0) { 
## +++++++++++++++++
## Solving for data$HydroDispatch_AnnualStorage_MW -- END
## +++++++++++++++++

## +++++++++++++++++
## Input the TOTAL (dispatchable and non-dispatchable) MW of installed capacity for Hydro into "PP_MWneeded" under "HydroDispatch"
## Here this assumes there is ANNUAL electricity storage.
## +++++++++++++++++
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="HydroDispatch")] <- max(data$HydroDispatch_AnnualStorage_MW + data$HydroNonDispatch_MW)


## +++++++++++++++++
## Now determine the amount of NGCC and NGCT that are needed to meet the remaining
## "net load" by using the typical screening curve method ONLY to determine the 
## MW capacity of NGCC and NGCT.
## Create the "current" load duration cuve to solve for NGCC and NGCT amounts.
## Here this assumes there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++
## At this point in the code, "frac_MWh_withoutNG_actual" equation should be true since data$NGCC and data$NGCT are all zero at this point
data$NGCC_AnnualStorage_MW <- 0*data$NGCC_MW
data$NGCT_AnnualStorage_MW <- 0*data$NGCT_MW
frac_MWh_withoutNG_actual <- (sum(data$Wind_MW_DirectToGrid_AnnualStorage) + sum(data$PV_MW_DirectToGrid_AnnualStorage) + sum(data$CSP_MW_DirectToGrid_AnnualStorage) + sum(data$Dispatched_StoredWindSolar_MW) +
                              sum(data$Coal_AnnualStorage_MW)+sum(data$NGCC_AnnualStorage_MW)+sum(data$NGCT_AnnualStorage_MW)+sum(data$Nuclear_MW)+sum(data$UserTech1_AnnualStorage_MW)+sum(data$UserTech2_AnnualStorage_MW)+sum(data$Geothermal_AnnualStorage_MW)+sum(data$Biomass_AnnualStorage_MW)+sum(data$PetroleumCC_AnnualStorage_MW)+sum(data$HydroDispatch_AnnualStorage_MW)+sum(data$HydroNonDispatch_MW))/sum(data$Load_MW)
frac_MWh_withoutNG_target <- sum(Frac_MWhDesired_dispatchable$Fraction_MWhDesired)+sum(Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired)
data$generation_non_NG_AnnualStorage <- data$generation_non_NG_HydroDispatch_AnnualStorage + data$HydroDispatch_AnnualStorage_MW
data$net_load_solveNG_AnnualStorage <- data$Load_MW - data$generation_non_NG_AnnualStorage
net_load_duration_solveNG_AnnualStorage <- data$net_load_solveNG_AnnualStorage
net_load_duration_solveNG_AnnualStorage <- net_load_duration_solveNG_AnnualStorage[order(-net_load_duration_solveNG_AnnualStorage)]
## find range of data$hour.ending when NGCC is lowest cost and/or NGCT is lowest cost.
ind.NGCC <- which(PPCost_US$Technology==c("NGCC"))
ind.NGCT <- which(PPCost_US$Technology==c("NGCT"))
data$NGCC_Cost_8760 <- PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCC] + data$Hour.ending*((PPCost_US$VariableOMCost_..MWh[ind.NGCC]+PPCost_US$VariableFuelCost_..MWh[ind.NGCC])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
data$NGCT_Cost_8760 <- PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCT] + data$Hour.ending*((PPCost_US$VariableOMCost_..MWh[ind.NGCT]+PPCost_US$VariableFuelCost_..MWh[ind.NGCT])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
ind.lowest_cost_NGCC <- which(data$NGCC_Cost_8760<=data$NGCT_Cost_8760)
ind.lowest_cost_NGCT <- which(data$NGCT_Cost_8760<data$NGCC_Cost_8760)


## +++++++++++++++++
## START: Solve for NGCC and NGCT dispatch, assuming ANNUAL electricity storage.
## Now solve for the MW capacity needed for NGCC and NGCT using a screening curve approach for NGCC and NGCT only.
## Here this assumes there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++
ind.PP_MWneeded_NGCC <- which(PP_MWneeded_AnnualStorage$Technology==c("NGCC"))
ind.PP_MWneeded_NGCT <- which(PP_MWneeded_AnnualStorage$Technology==c("NGCT"))
if (min(ind.lowest_cost_NGCT) <= min(ind.lowest_cost_NGCC)) { ## Then NGCT serves the peak net_load and NGCC starts serving remaining net_load from the minimum net_load value 
  # find capacity of NGCC first:
  net_load.min_temp <- max(0,min(net_load_duration_solveNG_AnnualStorage))
  net_load.max_temp <- net_load_duration_solveNG_AnnualStorage[min(ind.lowest_cost_NGCC)]
  PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCC] <- net_load.max_temp - net_load.min_temp
  # Determine generation from NGCC each hour. 
  # This is equal to all of "data$net_load_solveNG" that is <= the just-determined MW capacity of NGCC
  data$NGCC_AnnualStorage_MW <- data$net_load_solveNG_AnnualStorage
  data$NGCC_AnnualStorage_MW[data$NGCC_AnnualStorage_MW>=PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCC]] <- PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCC]
  data$NGCC_AnnualStorage_MW[data$NGCC_AnnualStorage_MW<0] <- 0
  # find capacity of NGCT second:
  PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCT] <- max(net_load_duration_solveNG_AnnualStorage) - net_load.max_temp
  # Determine generation from NGCT each hour. 
  # This is equal to all of "data$net_load_solveNG" minus data$NGCC_MW
  data$NGCT_AnnualStorage_MW <- data$net_load_solveNG_AnnualStorage - data$NGCC_AnnualStorage_MW
} else if (min(ind.lowest_cost_NGCC) < min(ind.lowest_cost_NGCT)) { ## Then NGCC serves the peak net_load and NGCT starts serving remaining net_load from the minimum net_load value 
  # find capacity of NGCT first:
  net_load.min_temp <- min(net_load_duration_solveNG_AnnualStorage)
  net_load.max_temp <- net_load_duration_solveNG_AnnualStorage[min(ind.lowest_cost_NGCT)]
  PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCT] <- net_load.max_temp - net_load.min_temp
  # Determine generation from NGCT each hour. 
  # This is equal to all of "data$net_load_solveNG" that is <= the just-determined MW capacity of NGCT
  data$NGCT_AnnualStorage_MW <- data$net_load_solveNG_AnnualStorage
  data$NGCT_AnnualStorage_MW[data$NGCT_AnnualStorage_MW>=PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCT]] <- PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCT]
  data$NGCT_AnnualStorage_MW[data$NGCT_AnnualStorage_MW<0] <- 0
  # find capacity of NGCC second:
  PP_MWneeded_AnnualStorage$MW_needed[ind.PP_MWneeded_NGCC] <- max(net_load_duration_solveNG_AnnualStorage) - net_load.max_temp
  # Determine generation from NGCC each hour. 
  # This is equal to all of "data$net_load_solveNG" minus data$NGCT_MW
  data$NGCC_AnnualStorage_MW <- data$net_load_solveNG_AnnualStorage - data$NGCT_AnnualStorage_MW
}
rm(net_load.min_temp,net_load.max_temp)
## +++++++++++++++++
## END: Solve for NGCC and NGCT dispatch, assuming ANNUAL electricity storage.
## Now solve for the MW capacity needed for NGCC and NGCT using a screening curve approach for NGCC and NGCT only.
## Here this assumes there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++


## +++++++++++++++++
## Input the MW of installed capacity needed for Wind and PV into "PP_MWneeded".
## Here this assumes there is ANNUAL electricity storage, so that data regarding
## storage dispatch and capacity is also included.
## +++++++++++++++++
PP_MWneeded_temp <- PP_MWneeded
PP_MWneeded_temp <- PP_MWneeded_temp[-c(1:(dim(PP_MWneeded_temp)[1]-7)),]  ## remove all but 7 rows
PP_MWneeded_temp[,2:dim(PP_MWneeded_temp)[2]] <- 0
PP_MWneeded_temp$Technology <- c("Wind_DirectToGrid_WithAnnualStorage","PV_DirectToGrid_WithAnnualStorage","CSP_DirectToGrid_WithAnnualStorage","Wind_StoredToGrid_WithAnnualStorage","PV_StoredToGrid_WithAnnualStorage","CSP_StoredToGrid_WithAnnualStorage","AnnualStorage_Total")
PP_MWneeded_temp$MW_needed[which(PP_MWneeded_temp$Technology=="Wind_DirectToGrid_WithAnnualStorage")] <- MW_data_Wind*multiplier_WithAnnualStorage.wind
PP_MWneeded_temp$MW_needed[which(PP_MWneeded_temp$Technology=="PV_DirectToGrid_WithAnnualStorage")] <- MW_data_PV*multiplier_WithAnnualStorage.PV
PP_MWneeded_temp$MW_needed[which(PP_MWneeded_temp$Technology=="CSP_DirectToGrid_WithAnnualStorage")] <- MW_data_CSP*multiplier_WithAnnualStorage.CSP
PP_MWneeded_temp$MW_needed[which(PP_MWneeded_temp$Technology=="AnnualStorage_Total")] <- MW_capacity_AnnualStorage  ## This records the MW (power input/output capability) of storage capacity needed
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="Wind_DirectToGrid_WithAnnualStorage")] <- sum(data$Wind_MW_DirectToGrid_AnnualStorage)/sum(data$Load_MW)
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="PV_DirectToGrid_WithAnnualStorage")] <- sum(data$PV_MW_DirectToGrid_AnnualStorage)/sum(data$Load_MW)
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="CSP_DirectToGrid_WithAnnualStorage")] <- sum(data$CSP_MW_DirectToGrid_AnnualStorage)/sum(data$Load_MW)
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="Wind_StoredToGrid_WithAnnualStorage")] <- sum(data$StoredWind_WithAnnualStorage)/sum(data$Load_MW)
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="PV_StoredToGrid_WithAnnualStorage")] <- sum(data$StoredPV_WithAnnualStorage)/sum(data$Load_MW)
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="CSP_StoredToGrid_WithAnnualStorage")] <- sum(data$StoredCSP_WithAnnualStorage)/sum(data$Load_MW)
PP_MWneeded_temp$Fraction_MWhActual[which(PP_MWneeded_temp$Technology=="AnnualStorage_Total")] <- sum(data$Dispatched_StoredWindSolar_MW)/sum(data$Load_MW)
PP_MWneeded_temp[,6] <- rep(0,dim(PP_MWneeded_temp)[1])
names(PP_MWneeded_temp)[6] <- c("MWh_Needed_StorageCapacity")
PP_MWneeded[,6] <- rep(0,dim(PP_MWneeded)[1])
names(PP_MWneeded)[6] <- c("MWh_Needed_StorageCapacity")
PP_MWneeded_temp$MWh_Needed_StorageCapacity[which(PP_MWneeded_temp$Technology=="AnnualStorage_Total")] <- max(SOC_hourly_WithAnnualStorage)  ## This records the MWh (energy) storage capacity needed
PP_MWneeded <- rbind(PP_MWneeded,PP_MWneeded_temp)


## +++++++++++++++
## calculate actual % of total generation from dispatchable generators.
## Here this assumes there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="Coal")] <- sum(data$Coal_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="NGCC")] <- sum(data$NGCC_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="NGCT")] <- sum(data$NGCT_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="UserTech1")] <- sum(data$UserTech1_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="UserTech2")] <- sum(data$UserTech2_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="Nuclear")] <- sum(data$Nuclear_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="Wind")] <- sum(data$Wind_MW_DirectToGrid_AnnualStorage,data$StoredWind_WithAnnualStorage)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="PV")] <- sum(data$PV_MW_DirectToGrid_AnnualStorage,data$StoredPV_WithAnnualStorage)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="CSP")] <- sum(data$CSP_MW_DirectToGrid_AnnualStorage,data$StoredCSP_WithAnnualStorage)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="Biomass")] <- sum(data$Biomass_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="Geothermal")] <- sum(data$Geothermal_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="PetroleumCC")] <- sum(data$PetroleumCC_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="HydroDispatch")] <- sum(data$HydroDispatch_AnnualStorage_MW)/sum(data$Load_MW)
PP_MWneeded_AnnualStorage$Fraction_MWhActual[which(PP_MWneeded_AnnualStorage$Technology=="HydroNonDispatch")] <- sum(data$HydroNonDispatch_MW)/sum(data$Load_MW)

## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## CALCULATIONS FOR GENERATION CAPACITY (not nuclear or non-dispatchable hydro)
## ASSUMING ANNUAL ELECTRICITY STORAGE - END
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++


## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## CALCULATE SYSTEM COSTS - BEGIN
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## +++++++++++++++
## Determine the annual cost of generation with the user-chosen energy mix.  
## Cost is calculated in units of $1000 per year per MW installed if operating 
## for a given number of hours in data$hour.ending from 0 to 8760 (or 0% to 100% Capacity Factor).
## Example: data$Coal_Cost_8760 is the fixed plus variable cost to operate a coal plant for 0 to 8760 hours of the year.
## Here the costs are without any type of electricity storage.
## +++++++++++++++

ind.Coal <- which(PPCost_US$Technology==c("Coal"))
ind.Nuclear <- which(PPCost_US$Technology==c("Nuclear"))
ind.UserTech1 <- which(PPCost_US$Technology==c("UserTech1"))
ind.UserTech2 <- which(PPCost_US$Technology==c("UserTech2"))
ind.Wind <- which(PPCost_US$Technology==c("Wind"))
ind.PV <- which(PPCost_US$Technology==c("PV"))
ind.CSP <- which(PPCost_US$Technology==c("CSP"))
ind.Biomass <- which(PPCost_US$Technology==c("Biomass"))
ind.Geothermal <- which(PPCost_US$Technology==c("Geothermal"))
ind.Petroleum <- which(PPCost_US$Technology==c("PetroleumCC"))
ind.HydroDispatch <- which(PPCost_US$Technology==c("HydroDispatch"))
ind.HydroNonDispatch <- which(PPCost_US$Technology==c("HydroNonDispatch"))

## +++++++++++++++++
## Calculate Variable operating cost for each generator type for each hour
## "Cost_VariableOnly_8760$..." = MW generated per hour * $k/MWh in variable costs = 1000s of dollars per each hour
## Here the costs are without any type of electricity storage.
## +++++++++++++++++
Cost_VariableOnly_8760 <- data.frame(rep(0,8760))
names(Cost_VariableOnly_8760)=c("Coal")
Cost_VariableOnly_8760$Coal <- data$Coal_MW*(PPCost_US$VariableOMCost_..MWh[ind.Coal]+PPCost_US$VariableFuelCost_..MWh[ind.Coal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Nuclear <- data$Nuclear_MW*(PPCost_US$VariableOMCost_..MWh[ind.Nuclear]+PPCost_US$VariableFuelCost_..MWh[ind.Nuclear])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$UserTech1 <- data$UserTech1_MW*(PPCost_US$VariableOMCost_..MWh[ind.UserTech1]+PPCost_US$VariableFuelCost_..MWh[ind.UserTech1])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$UserTech2 <- data$UserTech2_MW*(PPCost_US$VariableOMCost_..MWh[ind.UserTech2]+PPCost_US$VariableFuelCost_..MWh[ind.UserTech2])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Wind <- data$Wind_MW_total*(PPCost_US$VariableOMCost_..MWh[ind.Wind]+PPCost_US$VariableFuelCost_..MWh[ind.Wind])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$PV <- data$PV_MW_total*(PPCost_US$VariableOMCost_..MWh[ind.PV]+PPCost_US$VariableFuelCost_..MWh[ind.PV])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$NGCC <- data$NGCC_MW*(PPCost_US$VariableOMCost_..MWh[ind.NGCC]+PPCost_US$VariableFuelCost_..MWh[ind.NGCC])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$NGCT <- data$NGCT_MW*(PPCost_US$VariableOMCost_..MWh[ind.NGCT]+PPCost_US$VariableFuelCost_..MWh[ind.NGCT])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$CSP <- data$CSP_MW_total*(PPCost_US$VariableOMCost_..MWh[ind.CSP]+PPCost_US$VariableFuelCost_..MWh[ind.CSP])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Biomass <- data$Biomass_MW*(PPCost_US$VariableOMCost_..MWh[ind.Biomass]+PPCost_US$VariableFuelCost_..MWh[ind.Biomass])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Petroleum <- data$PetroleumCC_MW*(PPCost_US$VariableOMCost_..MWh[ind.Petroleum]+PPCost_US$VariableFuelCost_..MWh[ind.Petroleum])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Hydro <- data$HydroDispatch_MW*(PPCost_US$VariableOMCost_..MWh[ind.HydroDispatch]+PPCost_US$VariableFuelCost_..MWh[ind.HydroDispatch])/1000+
  data$HydroNonDispatch_MW*(PPCost_US$VariableOMCost_..MWh[ind.HydroNonDispatch]+PPCost_US$VariableFuelCost_..MWh[ind.HydroNonDispatch])/1000
Cost_VariableOnly_8760$Geothermal <- data$Geothermal_MW*(PPCost_US$VariableOMCost_..MWh[ind.Geothermal]+PPCost_US$VariableFuelCost_..MWh[ind.Geothermal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed

data$Grid_Cost_8760_VariableOnly <- data$Load_MW*0  ## initialize the cost to run most expensive power plant each hour
data$Grid_Cost_8760_VariableOnly <- Cost_VariableOnly_8760$Coal +
  Cost_VariableOnly_8760$Nuclear +
  Cost_VariableOnly_8760$NGCC +
  Cost_VariableOnly_8760$NGCT +
  Cost_VariableOnly_8760$Wind +
  Cost_VariableOnly_8760$PV +
  Cost_VariableOnly_8760$UserTech1 +
  Cost_VariableOnly_8760$UserTech2 +
  Cost_VariableOnly_8760$CSP +
  Cost_VariableOnly_8760$Biomass +
  Cost_VariableOnly_8760$Geothermal +
  Cost_VariableOnly_8760$Hydro +
  Cost_VariableOnly_8760$Petroleum


## +++++++++++++++++
## Calculate Variable operating cost for each generator type for each hour
## "Cost_VariableOnly_8760$..." = MW generated per hour * $k/MWh in variable costs = 1000s of dollars per each hour
## Here the costs include ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++
Cost_VariableOnly_8760_AnnualStorage <- data.frame(rep(0,8760))
names(Cost_VariableOnly_8760_AnnualStorage)=c("Coal")
Cost_VariableOnly_8760_AnnualStorage$Coal <- data$Coal_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.Coal]+PPCost_US$VariableFuelCost_..MWh[ind.Coal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Nuclear <- data$Nuclear_MW*(PPCost_US$VariableOMCost_..MWh[ind.Nuclear]+PPCost_US$VariableFuelCost_..MWh[ind.Nuclear])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$UserTech1 <- data$UserTech1_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.UserTech1]+PPCost_US$VariableFuelCost_..MWh[ind.UserTech1])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$UserTech2 <- data$UserTech2_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.UserTech2]+PPCost_US$VariableFuelCost_..MWh[ind.UserTech2])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Wind <- data$Wind_MW_WithAnnualStorage_total*(PPCost_US$VariableOMCost_..MWh[ind.Wind]+PPCost_US$VariableFuelCost_..MWh[ind.Wind])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$PV <- data$PV_MW_WithAnnualStorage_total*(PPCost_US$VariableOMCost_..MWh[ind.PV]+PPCost_US$VariableFuelCost_..MWh[ind.PV])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$NGCC <- data$NGCC_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.NGCC]+PPCost_US$VariableFuelCost_..MWh[ind.NGCC])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$NGCT <- data$NGCT_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.NGCT]+PPCost_US$VariableFuelCost_..MWh[ind.NGCT])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$CSP <- data$CSP_MW_WithAnnualStorage_total*(PPCost_US$VariableOMCost_..MWh[ind.CSP]+PPCost_US$VariableFuelCost_..MWh[ind.CSP])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Biomass <- data$Biomass_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.Biomass]+PPCost_US$VariableFuelCost_..MWh[ind.Biomass])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Petroleum <- data$PetroleumCC_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.Petroleum]+PPCost_US$VariableFuelCost_..MWh[ind.Petroleum])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Hydro <- data$HydroDispatch_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.HydroDispatch]+PPCost_US$VariableFuelCost_..MWh[ind.HydroDispatch])/1000+
  data$HydroNonDispatch_MW*(PPCost_US$VariableOMCost_..MWh[ind.HydroNonDispatch]+PPCost_US$VariableFuelCost_..MWh[ind.HydroNonDispatch])/1000
Cost_VariableOnly_8760_AnnualStorage$Geothermal <- data$Geothermal_AnnualStorage_MW*(PPCost_US$VariableOMCost_..MWh[ind.Geothermal]+PPCost_US$VariableFuelCost_..MWh[ind.Geothermal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed

Cost_VariableOnly_8760_AnnualStorage$AnnualStorage <- 0*Cost_VariableOnly_8760_AnnualStorage$Geothermal

data$Grid_Cost_8760_VariableOnly_AnnualStorage <- data$Load_MW*0  ## initialize the cost to run most expensive power plant each hour
data$Grid_Cost_8760_VariableOnly_AnnualStorage <- Cost_VariableOnly_8760_AnnualStorage$Coal +
  Cost_VariableOnly_8760_AnnualStorage$Nuclear +
  Cost_VariableOnly_8760_AnnualStorage$NGCC +
  Cost_VariableOnly_8760_AnnualStorage$NGCT +
  Cost_VariableOnly_8760_AnnualStorage$Wind +
  Cost_VariableOnly_8760_AnnualStorage$PV +
  Cost_VariableOnly_8760_AnnualStorage$UserTech1 +
  Cost_VariableOnly_8760_AnnualStorage$UserTech2 +
  Cost_VariableOnly_8760_AnnualStorage$CSP +
  Cost_VariableOnly_8760_AnnualStorage$Biomass +
  Cost_VariableOnly_8760_AnnualStorage$Geothermal +
  Cost_VariableOnly_8760_AnnualStorage$Hydro +
  Cost_VariableOnly_8760_AnnualStorage$Petroleum +
  Cost_VariableOnly_8760_AnnualStorage$AnnualStorage


## +++++++++++++++++
## Calcualte fixed cost in $/MWh for the mix of generators chosen by the user in the final year for which the user has chosen the mix
## "AnnualGridCost_fixed" = MW of capacity * $k/MW-year = 1000s of $ per year to pay fixed costs
## +++++++++++++++++
AnnualGridCost_fixed = PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Coal")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Coal] + PPCost_US$FixedOMCost_k..MWyear[ind.Coal]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="NGCC")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCC]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="NGCT")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCT]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Nuclear] + PPCost_US$FixedOMCost_k..MWyear[ind.Nuclear]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.PV] + PPCost_US$FixedOMCost_k..MWyear[ind.PV]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Wind] + PPCost_US$FixedOMCost_k..MWyear[ind.Wind]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="UserTech1")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.UserTech1] + PPCost_US$FixedOMCost_k..MWyear[ind.UserTech1]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="UserTech2")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.UserTech2] + PPCost_US$FixedOMCost_k..MWyear[ind.UserTech2]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.CSP] + PPCost_US$FixedOMCost_k..MWyear[ind.CSP]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Biomass")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Biomass] + PPCost_US$FixedOMCost_k..MWyear[ind.Biomass]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PetroleumCC")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Petroleum] + PPCost_US$FixedOMCost_k..MWyear[ind.Petroleum]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="HydroDispatch")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.HydroDispatch] + PPCost_US$FixedOMCost_k..MWyear[ind.HydroDispatch]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="HydroNonDispatch")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.HydroNonDispatch] + PPCost_US$FixedOMCost_k..MWyear[ind.HydroNonDispatch]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Geothermal")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Geothermal] + PPCost_US$FixedOMCost_k..MWyear[ind.Geothermal])

## Load PV, CSP, and Wind capacities needed when using Annual Storage technology
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Wind")] <- PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind_DirectToGrid_WithAnnualStorage")]
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="PV")] <- PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV_DirectToGrid_WithAnnualStorage")]
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="CSP")] <- PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP_DirectToGrid_WithAnnualStorage")]
## Calculate annualized Annual Storage costs (capex, O&M, and other variable); units of $1000/yr
AnnualStorage_AnnualizedCAPEX <- max(StorageCost$AnnualizedCapitalCost_k..MWyear[which(StorageCost$X=="AnnualStorage")]*PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="AnnualStorage_Total")],
                                     StorageCost$AnnualizedCapitalCost_k..MWhyear[which(StorageCost$X=="AnnualStorage")]*PP_MWneeded$MWh_Needed_StorageCapacity[which(PP_MWneeded$Technology=="AnnualStorage_Total")])

AnnualGridCost_fixed_AnnualStorage = PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Coal")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Coal] + PPCost_US$FixedOMCost_k..MWyear[ind.Coal]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCC")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCC]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCT")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + PPCost_US$FixedOMCost_k..MWyear[ind.NGCT]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Nuclear] + PPCost_US$FixedOMCost_k..MWyear[ind.Nuclear]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="PV")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.PV] + PPCost_US$FixedOMCost_k..MWyear[ind.PV]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Wind")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Wind] + PPCost_US$FixedOMCost_k..MWyear[ind.Wind]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="UserTech1")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.UserTech1] + PPCost_US$FixedOMCost_k..MWyear[ind.UserTech1]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="UserTech2")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.UserTech2] + PPCost_US$FixedOMCost_k..MWyear[ind.UserTech2]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="CSP")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.CSP] + PPCost_US$FixedOMCost_k..MWyear[ind.CSP]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Biomass")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Biomass] + PPCost_US$FixedOMCost_k..MWyear[ind.Biomass]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="PetroleumCC")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Petroleum] + PPCost_US$FixedOMCost_k..MWyear[ind.Petroleum]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="HydroDispatch")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.HydroDispatch] + PPCost_US$FixedOMCost_k..MWyear[ind.HydroDispatch]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="HydroNonDispatch")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.HydroNonDispatch] + PPCost_US$FixedOMCost_k..MWyear[ind.HydroNonDispatch]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Geothermal")]*(PPCost_US$AnnualizedCapitalCost_k..MWyear[ind.Geothermal] + PPCost_US$FixedOMCost_k..MWyear[ind.Geothermal])+
  AnnualStorage_AnnualizedCAPEX

AnnualGridCost_variable <- sum(data$Grid_Cost_8760_VariableOnly)  ## 1000s of dollars per year
AnnualGridCost_Total <- AnnualGridCost_variable + AnnualGridCost_fixed  ## 1000s of dollars per year
AnnualGridCost_fixed_MWh <- AnnualGridCost_fixed*1000/sum(data$Load_MW) ## ($/year)/(MWh/year) = $/MWh
AnnualGridCost_variable_MWh <- AnnualGridCost_variable*1000/sum(data$Load_MW) ## ($/year)/(MWh/year) = $/MWh
AnnualGridCost_Total_MWh <- AnnualGridCost_Total*1000/sum(data$Load_MW) ## ($/year)/(MWh/year) = $/MWh

AnnualGridCost_variable_AnnualStorage <- sum(data$Grid_Cost_8760_VariableOnly_AnnualStorage)  ## 1000s of dollars per year
AnnualGridCost_Total_AnnualStorage <- AnnualGridCost_variable_AnnualStorage + AnnualGridCost_fixed_AnnualStorage  ## 1000s of dollars per year
AnnualGridCost_fixed_MWh_AnnualStorage <- AnnualGridCost_fixed_AnnualStorage*1000/sum(data$Load_MW) ## ($/year)/(MWh/year) = $/MWh
AnnualGridCost_variable_MWh_AnnualStorage <- AnnualGridCost_variable_AnnualStorage*1000/sum(data$Load_MW) ## ($/year)/(MWh/year) = $/MWh
AnnualGridCost_Total_MWh_AnnualStorage <- AnnualGridCost_Total_AnnualStorage*1000/sum(data$Load_MW) ## ($/year)/(MWh/year) = $/MWh
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## CALCULATE SYSTEM COSTS - END
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++


## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## OUTPUT RESULTS TO FEED TO WEBSITE AND GOOGLE SHEET WITH CASH FLOW CALCULATIONS
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## +++++
## No storage
## +++++
## 8760 hourly DATA - No storage
data$Wind_MW_NetToGrid_NoStorage <- data$Wind_MW_total-curtailed_Wind
data$PV_MW_NetToGrid_NoStorage <- data$PV_MW_total-curtailed_PV
data$CSP_MW_NetToGrid_NoStorage <- data$CSP_MW_total-curtailed_CSP
data$Hydro_MW <- data$HydroDispatch_MW+data$HydroNonDispatch_MW
## Some dispatch data (maybe only for NGCT or NGCC) might have slightly negative values, so assure that all hourly dispatch data are >= 0
data$NGCC_MW[which(data$NGCC_MW<0)] <- 0  ## THis might have slightly negative values, so assure they are >=0 so that negative data do not induce negative data to show on plots on the website that adjusts y-axis limits automatically
data$NGCT_MW[which(data$NGCT_MW<0)] <- 0  ## THis might have slightly negative values, so assure they are >=0 so that negative data do not induce negative data to show on plots on the website that adjusts y-axis limits automatically
hourly_MWOutput_NoStorage <- data.frame(data$Hour.ending,data$Load_MW,data$Wind_MW_NetToGrid_NoStorage,data$PV_MW_NetToGrid_NoStorage,data$CSP_MW_NetToGrid_NoStorage,
                                             data$Biomass_MW,data$Coal_MW,data$Geothermal_MW,data$Hydro_MW,data$NGCC_MW,data$NGCT_MW,data$Nuclear_MW,data$PetroleumCC_MW,
                                             curtailed_Wind,curtailed_PV,curtailed_CSP)
names(hourly_MWOutput_NoStorage) = c("Hour_ending","Load","Wind_DirectToGrid","PV_DirectToGrid","CSP_DirectToGrid",
                                          "Biomass","Coal","Geothermal","Hydro","NGCC","NGCT","Nuclear","Petroleum",
                                          "Wind_curtailed","PV_curtailed","CSP_curtailed")
## Power Plant and capacity
## Use "PP_MWneeded", but remove information that is not needed and that relates to the "annual storage" solutions
PPdata_NoStorage <- PP_MWneeded
PPdata_NoStorage <- PPdata_NoStorage[,!(names(PPdata_NoStorage) %in% c("Cost_Variable"))]
PPdata_NoStorage <- PPdata_NoStorage[,!(names(PPdata_NoStorage) %in% c("Fraction_MWhDesired"))]
PPdata_NoStorage <- PPdata_NoStorage[,!(names(PPdata_NoStorage) %in% c("MWh_Needed_StorageCapacity"))]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="Wind_StoredToGrid_WithAnnualStorage"),]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="PV_StoredToGrid_WithAnnualStorage"),]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="CSP_StoredToGrid_WithAnnualStorage"),]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="Wind_DirectToGrid_WithAnnualStorage"),]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="PV_DirectToGrid_WithAnnualStorage"),]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="CSP_DirectToGrid_WithAnnualStorage"),]
PPdata_NoStorage <- PPdata_NoStorage[-which(PPdata_NoStorage$Technology=="AnnualStorage_Total"),]

## +++++
## With "annual" storage
## +++++
## 8760 hourly DATA - With "annual" storage
data$Hydro_AnnualStorage_MW <- data$HydroDispatch_AnnualStorage_MW + data$HydroNonDispatch_MW
## Some dispatch data (maybe only for NGCT or NGCC) might have slightly negative values, so assure that all hourly dispatch data are >= 0
data$NGCC_AnnualStorage_MW[which(data$NGCC_AnnualStorage_MW<0)] <- 0  ## THis might have slightly negative values, so assure they are >=0 so that negative data do not induce negative data to show on plots on the website that adjusts y-axis limits automatically
data$NGCT_AnnualStorage_MW[which(data$NGCT_AnnualStorage_MW<0)] <- 0  ## THis might have slightly negative values, so assure they are >=0 so that negative data do not induce negative data to show on plots on the website that adjusts y-axis limits automatically
hourly_MWOutput_AnnualStorage <- data.frame(data$Hour.ending,data$Load_MW,data$Wind_MW_DirectToGrid_AnnualStorage,data$PV_MW_DirectToGrid_AnnualStorage,data$CSP_MW_DirectToGrid_AnnualStorage,
                                             data$Biomass_AnnualStorage_MW,data$Coal_AnnualStorage_MW,data$Geothermal_AnnualStorage_MW,data$Hydro_AnnualStorage_MW,data$NGCC_AnnualStorage_MW,data$NGCT_AnnualStorage_MW,data$Nuclear_MW,data$PetroleumCC_AnnualStorage_MW,
                                             data$Dispatched_StoredWindSolar_MW)
names(hourly_MWOutput_AnnualStorage) = c("Hour_ending","Load","Wind_DirectToGrid","PV_DirectToGrid","CSP_DirectToGrid",
                                          "Biomass","Coal","Geothermal","Hydro","NGCC","NGCT","Nuclear","Petroleum",
                                          "WindSolar_Stored_then_Dispatched")
## Power Plant and capacity
## SUMMARIZE ALL "ANNUAL STORAGE" DATA IN SAME DATA FRAME
## Use "PP_MWneeded", but remove information that is not needed and that relates to the "annual storage" solutions
PPdata_AnnualStorage <- PP_MWneeded_AnnualStorage
PPdata_AnnualStorage$MWh_Needed_StorageCapacity <- 0*PP_MWneeded_AnnualStorage$MW_needed
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="Wind_DirectToGrid_WithAnnualStorage"),])
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="PV_DirectToGrid_WithAnnualStorage"),])
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="CSP_DirectToGrid_WithAnnualStorage"),])
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="Wind_StoredToGrid_WithAnnualStorage"),])
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="PV_StoredToGrid_WithAnnualStorage"),])
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="CSP_StoredToGrid_WithAnnualStorage"),])
PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PP_MWneeded[which(PP_MWneeded$Technology=="AnnualStorage_Total"),])
PPdata_AnnualStorage <- PPdata_AnnualStorage[-which(PP_MWneeded$Technology=="Wind"),]
PPdata_AnnualStorage <- PPdata_AnnualStorage[-which(PP_MWneeded$Technology=="PV"),]
PPdata_AnnualStorage <- PPdata_AnnualStorage[-which(PP_MWneeded$Technology=="CSP"),]
PPdata_AnnualStorage <- PPdata_AnnualStorage[,!(names(PPdata_AnnualStorage) %in% c("Cost_Variable"))]
PPdata_AnnualStorage <- PPdata_AnnualStorage[,!(names(PPdata_AnnualStorage) %in% c("Fraction_MWhDesired"))]


## specify the hours of data to write in the "solveGEN_output" list
hour_winter_start = 500
hour_winter_end = hour_winter_start+24*7-1
hour_spring_start = hour_winter_start + 8760/4
hour_spring_end = hour_spring_start+24*7-1
hour_summer_start =  hour_spring_start + 8760/4
hour_summer_end = hour_summer_start+24*7-1
hour_fall_start =  hour_summer_start + 8760/4
hour_fall_end = hour_fall_start+24*7-1
hrs <- c(seq(hour_winter_start,hour_winter_end),seq(hour_spring_start,hour_spring_end),seq(hour_summer_start,hour_summer_end),seq(hour_fall_start,hour_fall_end))

## Write new output data frames that are in a consistent format for creating JSON output from the R codes 
## on the server to the website and Google Sheet with the cash flow calculations.
## FIRST: No Storage
PPdata_NoStorage_solveGen_output <- PPdata_NoStorage
# Add a "DUMMY" column of data that represents the "MWh_Needed_StorageCapacity" in the "AnnualStorage" calculation, and here is just zeros to keep the same output data format
PPdata_NoStorage_solveGen_output$MWh_Needed_StorageCapacity <- rep(0,dim(PPdata_NoStorage_solveGen_output)[1])
# Add a "DUMMY" row of data that represents the "AnnualStorage_Total" in the "AnnualStorage" calculation, and here is just zeros to keep the same output data format
temp_df <- PPdata_NoStorage_solveGen_output[1,]
temp_df$Technology <- c("AnnualStorage_Total")
temp_df[1,(2:dim(temp_df)[2])] <- temp_df[1,(2:dim(temp_df)[2])]*0
PPdata_NoStorage_solveGen_output <- rbind(PPdata_NoStorage_solveGen_output, temp_df)
# Add a column of data that is the total TWh of generation from each source
PPdata_NoStorage_solveGen_output$TWhGeneration <- PPdata_NoStorage_solveGen_output$Fraction_MWhActual*sum(data$Load_MW)/1e6
# Add the two rows of "TWhGeneration" for hydro (dispatch and non-dispatch)
PPdata_NoStorage_solveGen_output$TWhGeneration[which(PPdata_NoStorage_solveGen_output$Technology=="HydroDispatch")] <- 
  PPdata_NoStorage_solveGen_output$TWhGeneration[which(PPdata_NoStorage_solveGen_output$Technology=="HydroDispatch")] + 
  PPdata_NoStorage_solveGen_output$TWhGeneration[which(PPdata_NoStorage_solveGen_output$Technology=="HydroNonDispatch")]
# Add the two rows of "Fraction_MWhActual" for hydro (dispatch and non-dispatch)
PPdata_NoStorage_solveGen_output$Fraction_MWhActual[which(PPdata_NoStorage_solveGen_output$Technology=="HydroDispatch")] <- 
  PPdata_NoStorage_solveGen_output$Fraction_MWhActual[which(PPdata_NoStorage_solveGen_output$Technology=="HydroDispatch")] + 
  PPdata_NoStorage_solveGen_output$Fraction_MWhActual[which(PPdata_NoStorage_solveGen_output$Technology=="HydroNonDispatch")]
## Erase the "duplicate rows" Hydro data 
PPdata_NoStorage_solveGen_output <- PPdata_NoStorage_solveGen_output[-which(PPdata_NoStorage_solveGen_output$Technology=="HydroNonDispatch"),]
## Erase "UserTech1" and "UserTech2"
PPdata_NoStorage_solveGen_output <- PPdata_NoStorage_solveGen_output[-which(PPdata_NoStorage_solveGen_output$Technology=="UserTech1"),]
PPdata_NoStorage_solveGen_output <- PPdata_NoStorage_solveGen_output[-which(PPdata_NoStorage_solveGen_output$Technology=="UserTech2"),]
## Sort them alphabetically
PPdata_NoStorage_solveGen_output <- PPdata_NoStorage_solveGen_output[order(PPdata_NoStorage_solveGen_output$Technology),]
## Ensure values for are >=0 
indices_temp = which(PPdata_NoStorage_solveGen_output$TWhGeneration<0)
PPdata_NoStorage_solveGen_output$TWhGeneration[indices_temp] = 0
PPdata_NoStorage_solveGen_output$Fraction_MWhActual[indices_temp] = 0
PPdata_NoStorage_solveGen_output$MW_needed[indices_temp] = 0
rm(indices_temp)

## SECOND: With ANnual Storage
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage
# Add a column of data that is the total TWh of generation from each source
PPdata_AnnualStorage_solveGen_output$TWhGeneration <- PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual*sum(data$Load_MW)/1e6
# Add the two rows of "TWhGeneration" for CSP, Wind, and PV and hydro (dispatch and non-dispatch)
PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_DirectToGrid_WithAnnualStorage")] <- 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_DirectToGrid_WithAnnualStorage")] + 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_StoredToGrid_WithAnnualStorage")]
PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_DirectToGrid_WithAnnualStorage")] <- 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_DirectToGrid_WithAnnualStorage")] + 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_StoredToGrid_WithAnnualStorage")]
PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_DirectToGrid_WithAnnualStorage")] <- 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_DirectToGrid_WithAnnualStorage")] + 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_StoredToGrid_WithAnnualStorage")]
PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroDispatch")] <- 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroDispatch")] + 
  PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroNonDispatch")]
# Add the two rows of ""Fraction_MWhActual" for CSP, Wind, and PV and hydro (dispatch and non-dispatch)
PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_DirectToGrid_WithAnnualStorage")] <- 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_DirectToGrid_WithAnnualStorage")] + 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_StoredToGrid_WithAnnualStorage")]
PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_DirectToGrid_WithAnnualStorage")] <- 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_DirectToGrid_WithAnnualStorage")] + 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_StoredToGrid_WithAnnualStorage")]
PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_DirectToGrid_WithAnnualStorage")] <- 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_DirectToGrid_WithAnnualStorage")] + 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_StoredToGrid_WithAnnualStorage")]
PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroDispatch")] <- 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroDispatch")] + 
  PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroNonDispatch")]
## Erase the "duplicate rows" of CSP, Wind, and PV and Hydro data that account for how much was stored before going onto the grid 
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[-which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_StoredToGrid_WithAnnualStorage"),]
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[-which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_StoredToGrid_WithAnnualStorage"),]
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[-which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_StoredToGrid_WithAnnualStorage"),]
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[-which(PPdata_AnnualStorage_solveGen_output$Technology=="HydroNonDispatch"),]
## Erase "UserTech1" and "UserTech2"
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[-which(PPdata_AnnualStorage_solveGen_output$Technology=="UserTech1"),]
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[-which(PPdata_AnnualStorage_solveGen_output$Technology=="UserTech2"),]
## rename the Technologies to be simnple to understand in "solveGEN_output"
PPdata_AnnualStorage_solveGen_output$Technology[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind_DirectToGrid_WithAnnualStorage")] <- c("Wind")
PPdata_AnnualStorage_solveGen_output$Technology[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV_DirectToGrid_WithAnnualStorage")] <- c("PV")
PPdata_AnnualStorage_solveGen_output$Technology[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP_DirectToGrid_WithAnnualStorage")] <- c("CSP")
## Sort them alphabetically
PPdata_AnnualStorage_solveGen_output <- PPdata_AnnualStorage_solveGen_output[order(PPdata_AnnualStorage_solveGen_output$Technology),]
## Ensure values for are >=0 
indices_temp = which(PPdata_AnnualStorage_solveGen_output$TWhGeneration<0)
PPdata_AnnualStorage_solveGen_output$TWhGeneration[indices_temp] = 0
PPdata_AnnualStorage_solveGen_output$Fraction_MWhActual[indices_temp] = 0
PPdata_AnnualStorage_solveGen_output$MW_needed[indices_temp] = 0
rm(indices_temp)

## Summarize biomass-related data that will later inform the average price of biomass fuel for biomass plants
## Solve for average biomass price based on ReEDS biomass supply-price curves
biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu_cumulative <- cumsum(biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu)
biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MWh_cumulative <- biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu_cumulative*1e6/HeatRate_biomass/1e3
ind_NoStorage <- which(biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MWh_cumulative < 1e6*PPdata_NoStorage_solveGen_output$TWhGeneration[which(PPdata_NoStorage_solveGen_output$Technology=="Biomass")])
ind_AnnualStorage <- which(biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MWh_cumulative < 1e6*PPdata_AnnualStorage_solveGen_output$TWhGeneration[which(PPdata_AnnualStorage_solveGen_output$Technology=="Biomass")])
if ( length(ind_NoStorage)>0 ) {
  biomass_2017USD_per_MMBtu_NoStorage <- sum( biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu[ind_NoStorage]*biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cost_2017USD_per_MMBtu[ind_NoStorage] ) / ( biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu_cumulative[max(ind_NoStorage)] )
} else {
  biomass_2017USD_per_MMBtu_NoStorage <- biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cost_2017USD_per_MMBtu[1]
}
if ( length(ind_AnnualStorage)>0 ) {
  biomass_2017USD_per_MMBtu_AnnualStorage <- sum( biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu[ind_AnnualStorage]*biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cost_2017USD_per_MMBtu[ind_AnnualStorage] ) / ( biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cap_MMBtu_cumulative[max(ind_AnnualStorage)] )
} else {
  biomass_2017USD_per_MMBtu_AnnualStorage <- biomass_CostSupplyCurve_byEIoF[[RegionNumber]]$cost_2017USD_per_MMBtu[1]
}
BiomassPrice <- data.frame(biomass_2017USD_per_MMBtu_NoStorage,biomass_2017USD_per_MMBtu_AnnualStorage)
rm(ind_AnnualStorage,ind_NoStorage)

## Solve for average geothermal capital cost and FOM cost based on ReEDS biomass supply-price curves
## Assume geothermal plants are ranked from cheapest to most expensive by CAPITAL COST ($/kW) and not FOM cost
max_geo_capacity_tolerance = 1.005
geo_supply_curve_cumulative_MW <- cumsum(geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW)
ind_NoStorage <-     which(geo_supply_curve_cumulative_MW < max_geo_capacity_tolerance*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="Geothermal")])
ind_AnnualStorage <- which(geo_supply_curve_cumulative_MW < max_geo_capacity_tolerance*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="Geothermal")])
ind_NoStorage <- c(ind_NoStorage,(ind_NoStorage[length(ind_NoStorage)]+1))
ind_AnnualStorage <- c(ind_AnnualStorage,(ind_AnnualStorage[length(ind_AnnualStorage)]+1))
if ( length(ind_NoStorage)>0 ) {
  geo_2017USD_per_kW_NoStorage <- sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_NoStorage]*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cost_2017USD_per_kW[ind_NoStorage] ) / sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_NoStorage] )
  geo_FOM_2017USD_per_kWyr_NoStorage <- sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_NoStorage]*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$FOM_2017USD_per_kWyear[ind_NoStorage] ) / sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_NoStorage] )
} else {
  geo_2017USD_per_kW_NoStorage <- geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cost_2017USD_per_kW[1]
  geo_FOM_2017USD_per_kWyr_NoStorage <- geo_CostSupplyCurve_byEIoF[[RegionNumber]]$FOM_2017USD_per_kWyear[1]
}
if ( length(ind_AnnualStorage)>0 ) {
  geo_2017USD_per_kW_AnnualStorage <- sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_AnnualStorage]*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cost_2017USD_per_kW[ind_AnnualStorage] ) / sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_AnnualStorage] )
  geo_FOM_2017USD_per_kWyr_AnnualStorage <- sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_AnnualStorage]*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$FOM_2017USD_per_kWyear[ind_AnnualStorage] ) / sum( 1000*geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cap_MW[ind_AnnualStorage] )
} else {
  geo_2017USD_per_kW_AnnualStorage <- geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cost_2017USD_per_kW[1]
  geo_FOM_2017USD_per_kWyr_AnnualStorage <- geo_CostSupplyCurve_byEIoF[[RegionNumber]]$FOM_2017USD_per_kWyear[1]
}
GeothermalCosts <- data.frame(c(geo_2017USD_per_kW_NoStorage,geo_FOM_2017USD_per_kWyr_NoStorage,geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cost_2017USD_per_kW[1],geo_CostSupplyCurve_byEIoF[[RegionNumber]]$FOM_2017USD_per_kWyear[1]),
                              c(geo_2017USD_per_kW_AnnualStorage,geo_FOM_2017USD_per_kWyr_AnnualStorage,geo_CostSupplyCurve_byEIoF[[RegionNumber]]$Cost_2017USD_per_kW[1],geo_CostSupplyCurve_byEIoF[[RegionNumber]]$FOM_2017USD_per_kWyear[1]))
names(GeothermalCosts) <- c("NoStorage","AnnualStorage")
rownames(GeothermalCosts) <-c("CAPEX_2017USD_per_kW","FOM_2017USD_per_kWyr","CAPEX_Lowest_2017USD_per_kW","FOM_Lowest_2017USD_per_kwyr")
#browser()
rm(ind_AnnualStorage,ind_NoStorage)

## Land area calculation for wind, PV, and CSP (no storage)
PPdata_NoStorage_solveGen_output$Land_1000acres <- rep(0,dim(PPdata_NoStorage_solveGen_output)[1])
PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="PV")] <- (1/1000)*acre_perMW_PV_totalarea*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="PV")]
PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="CSP")] <- (1/1000)*acre_perMW_CSP_totalarea*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="CSP")]
PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="Wind")] <- (1/1000)*acre_perMW_wind_totalarea*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="Wind")]
PPdata_NoStorage_solveGen_output$Land_1000acres_direct <- rep(0,dim(PPdata_NoStorage_solveGen_output)[1])
PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="PV")] <- (1/1000)*acre_perMW_PV_direct*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="PV")]
PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="CSP")] <- (1/1000)*acre_perMW_CSP_direct*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="CSP")]
PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="Wind")] <- (1/1000)*acre_perMW_wind_direct*PPdata_NoStorage_solveGen_output$MW_needed[which(PPdata_NoStorage_solveGen_output$Technology=="Wind")]
levels(PPdata_NoStorage_solveGen_output$Technology)[length(levels(PPdata_NoStorage_solveGen_output$Technology))+1] <- c("LandTotal")
levels(PPdata_NoStorage_solveGen_output$Technology)[length(levels(PPdata_NoStorage_solveGen_output$Technology))+1] <- c("LandDirect")
PPdata_NoStorage_solveGen_output[dim(PPdata_NoStorage_solveGen_output)[1]+1,] <- PPdata_NoStorage_solveGen_output[dim(PPdata_NoStorage_solveGen_output)[1],]  ## Create a new row by copying an existing row
PPdata_NoStorage_solveGen_output$Technology[dim(PPdata_NoStorage_solveGen_output)[1]] <- c("LandTotal")   ## relable the "Technology" of the row to "LandTotal"
PPdata_NoStorage_solveGen_output[(dim(PPdata_NoStorage_solveGen_output)[1]),(2:dim(PPdata_NoStorage_solveGen_output)[2])] <- rep(0,(dim(PPdata_NoStorage_solveGen_output)[2]-1))  ## Make add data for this "LandTotal" Technology = 0
PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="LandTotal")] <- sum(PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology!="LandTotal")])  ## Now add the total land area for all power plants, in acres, to this "LandArea" row
PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="LandTotal")] <- sum(PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology!="LandTotal")])  ## Now add the total land area for all power plants, in acres, to this "LandArea" row
## Land area calculation for wind, PV, and CSP (annual storage)
PPdata_AnnualStorage_solveGen_output$Land_1000acres <- rep(0,dim(PPdata_AnnualStorage_solveGen_output)[1])
PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV")] <- (1/1000)*acre_perMW_PV_totalarea*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV")]
PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP")] <- (1/1000)*acre_perMW_CSP_totalarea*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP")]
PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind")] <- (1/1000)*acre_perMW_wind_totalarea*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind")]
PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct <- rep(0,dim(PPdata_AnnualStorage_solveGen_output)[1])
PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV")] <- (1/1000)*acre_perMW_PV_direct*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV")]
PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP")] <- (1/1000)*acre_perMW_CSP_direct*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP")]
PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind")] <- (1/1000)*acre_perMW_wind_direct*PPdata_AnnualStorage_solveGen_output$MW_needed[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind")]
levels(PPdata_AnnualStorage_solveGen_output$Technology)[length(levels(PPdata_AnnualStorage_solveGen_output$Technology))+1] <- c("LandTotal")
PPdata_AnnualStorage_solveGen_output[dim(PPdata_AnnualStorage_solveGen_output)[1]+1,] <- PPdata_AnnualStorage_solveGen_output[dim(PPdata_AnnualStorage_solveGen_output)[1],]  ## Create a new row by copying an existing row
PPdata_AnnualStorage_solveGen_output$Technology[dim(PPdata_AnnualStorage_solveGen_output)[1]] <- c("LandTotal")   ## relable the "Technology" of the row to "LandTotal"
PPdata_AnnualStorage_solveGen_output[(dim(PPdata_AnnualStorage_solveGen_output)[1]),(2:dim(PPdata_AnnualStorage_solveGen_output)[2])] <- rep(0,(dim(PPdata_AnnualStorage_solveGen_output)[2]-1))  ## Make add data for this "LandTotal" Technology = 0
PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="LandTotal")] <- sum(PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology!="LandTotal")])  ## Now add the total land area for all power plants, in acres, to this "LandArea" row
PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="LandTotal")] <- sum(PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology!="LandTotal")])  ## Now add the total land area for all power plants, in acres, to this "LandArea" row

## Calculate the percent of "applicable" land (= includes regions that import electricity to RegionNumber) taken by PV, wind, and CSP
LandAcres_byTech_NoStorage <- 0*MaxCapacity_perRegion_perTech_MW
LandAcres_byTech_NoStorage[,6:14] <- c(LandAcres_byTech_NoStorage[,1:5],LandAcres_byTech_NoStorage[,1:4])  ## add 9 new columns
names(LandAcres_byTech_NoStorage) <- c("WindTotal","PVTotal","CSPTotal","BiomassTotal","GeothermalTotal","WindDirect","PVDirect","CSPDirect","BiomassDirect","GeothermalDirect","LandAcresTotal","PctLandTotal","LandAcresDirect","PctLandDirect")
LandAcres_byTech_AnnualStorage <- LandAcres_byTech_NoStorage
## Nostorage land use
LandAcres_byTech_NoStorage$WindTotal <- 1000*PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="Wind")]*Tranfer_RegionFromTo_Wind[,RegionNumber]
LandAcres_byTech_NoStorage$PVTotal <- 1000*PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="PV")]*Tranfer_RegionFromTo_PV[,RegionNumber]
LandAcres_byTech_NoStorage$CSPTotal <- 1000*PPdata_NoStorage_solveGen_output$Land_1000acres[which(PPdata_NoStorage_solveGen_output$Technology=="CSP")]*Tranfer_RegionFromTo_CSP[,RegionNumber]
LandAcres_byTech_NoStorage$WindDirect <- 1000*PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="Wind")]*Tranfer_RegionFromTo_Wind[,RegionNumber]
LandAcres_byTech_NoStorage$PVDirect <- 1000*PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="PV")]*Tranfer_RegionFromTo_PV[,RegionNumber]
LandAcres_byTech_NoStorage$CSPDirect <- 1000*PPdata_NoStorage_solveGen_output$Land_1000acres_direct[which(PPdata_NoStorage_solveGen_output$Technology=="CSP")]*Tranfer_RegionFromTo_CSP[,RegionNumber]
LandAcres_byTech_NoStorage$LandAcresTotal <- rowSums(LandAcres_byTech_NoStorage[,1:5])
LandAcres_byTech_NoStorage$PctLandTotal <- 100*LandAcres_byTech_NoStorage$LandAcresTotal/land_area_EIoF$LandArea_Acre
LandAcres_byTech_NoStorage$LandAcresDirect <- rowSums(LandAcres_byTech_NoStorage[,6:10])
LandAcres_byTech_NoStorage$PctLandDirect <- 100*LandAcres_byTech_NoStorage$LandAcresDirect/land_area_EIoF$LandArea_Acre
## AnnualStorage land use
LandAcres_byTech_AnnualStorage$WindTotal <- 1000*PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind")]*Tranfer_RegionFromTo_Wind[,RegionNumber]
LandAcres_byTech_AnnualStorage$PVTotal <- 1000*PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV")]*Tranfer_RegionFromTo_PV[,RegionNumber]
LandAcres_byTech_AnnualStorage$CSPTotal <- 1000*PPdata_AnnualStorage_solveGen_output$Land_1000acres[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP")]*Tranfer_RegionFromTo_CSP[,RegionNumber]
LandAcres_byTech_AnnualStorage$WindDirect <- 1000*PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="Wind")]*Tranfer_RegionFromTo_Wind[,RegionNumber]
LandAcres_byTech_AnnualStorage$PVDirect <- 1000*PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="PV")]*Tranfer_RegionFromTo_PV[,RegionNumber]
LandAcres_byTech_AnnualStorage$CSPDirect <- 1000*PPdata_AnnualStorage_solveGen_output$Land_1000acres_direct[which(PPdata_AnnualStorage_solveGen_output$Technology=="CSP")]*Tranfer_RegionFromTo_CSP[,RegionNumber]
LandAcres_byTech_AnnualStorage$LandAcresTotal <- rowSums(LandAcres_byTech_AnnualStorage[,1:5])
LandAcres_byTech_AnnualStorage$PctLandTotal <- 100*LandAcres_byTech_AnnualStorage$LandAcresTotal/land_area_EIoF$LandArea_Acre
LandAcres_byTech_AnnualStorage$LandAcresDirect <- rowSums(LandAcres_byTech_AnnualStorage[,6:10])
LandAcres_byTech_AnnualStorage$PctLandDirect <- 100*LandAcres_byTech_AnnualStorage$LandAcresDirect/land_area_EIoF$LandArea_Acre

## Weighted sum of land use into a total percentage of land use across all used regions (i.e., including regions that might be importing wind/PV/CSP into RegionNumber)
PPdata_NoStorage_solveGen_output$PctLand <- rep(0,13)
PPdata_NoStorage_solveGen_output$PctLandDirect <- rep(0,13)
PPdata_AnnualStorage_solveGen_output$PctLand <- rep(0,13)
PPdata_AnnualStorage_solveGen_output$PctLandDirect <- rep(0,13)
PPdata_NoStorage_solveGen_output$PctLand[which(PPdata_NoStorage_solveGen_output$Technology=="LandTotal")] <- round(sum(LandAcres_byTech_NoStorage$LandAcresTotal*LandAcres_byTech_NoStorage$PctLandTotal)/sum(LandAcres_byTech_NoStorage$LandAcresTotal),1)
PPdata_NoStorage_solveGen_output$PctLandDirect[which(PPdata_NoStorage_solveGen_output$Technology=="LandTotal")] <- round(sum(LandAcres_byTech_NoStorage$LandAcresDirect*LandAcres_byTech_NoStorage$PctLandDirect)/sum(LandAcres_byTech_NoStorage$LandAcresDirect),1)
PPdata_AnnualStorage_solveGen_output$PctLand[which(PPdata_AnnualStorage_solveGen_output$Technology=="LandTotal")] <- round(sum(LandAcres_byTech_AnnualStorage$LandAcresTotal*LandAcres_byTech_AnnualStorage$PctLandTotal)/sum(LandAcres_byTech_AnnualStorage$LandAcresTotal),1)
PPdata_AnnualStorage_solveGen_output$PctLandDirect[which(PPdata_AnnualStorage_solveGen_output$Technology=="LandTotal")] <- round(sum(LandAcres_byTech_AnnualStorage$LandAcresDirect*LandAcres_byTech_AnnualStorage$PctLandDirect)/sum(LandAcres_byTech_AnnualStorage$LandAcresDirect),1)
#browser()

print("In solveGEN.R: end of all calculations.")

## Final output list from this function
output_list <- list("Hourly_MW_NoStorage"=hourly_MWOutput_NoStorage[hrs,],
                        "Hourly_MW_AnnualStorage"=hourly_MWOutput_AnnualStorage[hrs,],
                        "PPdata_NoStorage"=PPdata_NoStorage_solveGen_output,
                        "PPdata_AnnualStorage"=PPdata_AnnualStorage_solveGen_output,
                        "BiomassPrice" = BiomassPrice,
                        "GeothermalCosts" = GeothermalCosts,
                        "WindSolar_InputIntoStorage_AnnualTWh" = curtailed_WindSolar_WithAnnualStorage_AnnualTWh)

print("In solveGEN.R: just before returning final output.")
return(output_list)
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
} ## END FUNCTION CALL
