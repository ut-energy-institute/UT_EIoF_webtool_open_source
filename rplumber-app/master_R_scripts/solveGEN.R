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
## 
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
## Hourly data
# wind_8760=read.csv(paste0("solveGen_data/Wind_Profiles_",year,".csv"))  ## Data must be input as "MW output per hour per installed MW of capacity"
# PV_8760=read.csv(paste0("solveGen_data/PV_Profiles_",year,".csv"))      ## Data must be input as "MW output per hour per installed MW of capacity"
# CSP_8760=read.csv(paste0("solveGen_data/CSP_Profiles_",year,".csv"))    ## Data must be input as "MW output per hour per installed MW of capacity"
# save(wind_8760,file="solveGen_data/wind_8760.Rdata")
# save(PV_8760,file="solveGen_data/PV_8760.Rdata")
# save(CSP_8760,file="solveGen_data/CSP_8760.Rdata")
# load("solveGen_data/wind_8760.Rdata")
# load("solveGen_data/PV_8760.Rdata")
# load("solveGen_data/CSP_8760.Rdata")
#save(wind_8760,PV_8760,CSP_8760,file="solveGen_data/renewables_2016profiles_8760.Rdata")
if (year == 2016) {
  load("solveGen_data/renewables_2016profiles_8760.Rdata")
  } else {
  stop("Error: You chose to use an incompatible 'year' for weather and renewable profiles to use as a basis of analysis.")
}

## ++++++
## Data indicating were renewables (wind, PV, and CSP) can be built yet serve load in another region
## ++++++
# Tranfer_RegionFromTo_CSP=read.csv("solveGen_data/FractionFromRegionToRegion_Matrix_CSP.csv")   ## An input-output matrix: For all of the user's desired CSP electricity TO region on the COLUMN, what fraction comes FROM the region of the ROW
# Tranfer_RegionFromTo_PV=read.csv("solveGen_data/FractionFromRegionToRegion_Matrix_PV.csv")     ## An input-output matrix: For all of the user's desired PV electricity TO region on the COLUMN, what fraction comes FROM the region of the ROW
# Tranfer_RegionFromTo_Wind=read.csv("solveGen_data/FractionFromRegionToRegion_Matrix_Wind.csv") ## An input-output matrix: For all of the user's desired (onshore) Wind electricity TO region on the COLUMN, what fraction comes FROM the region of the ROW
# rownames(Tranfer_RegionFromTo_CSP) <- Tranfer_RegionFromTo_CSP[,1]
# Tranfer_RegionFromTo_CSP <- Tranfer_RegionFromTo_CSP[,-1]
# rownames(Tranfer_RegionFromTo_PV) <- Tranfer_RegionFromTo_PV[,1]
# Tranfer_RegionFromTo_PV <- Tranfer_RegionFromTo_PV[,-1]
# rownames(Tranfer_RegionFromTo_Wind) <- Tranfer_RegionFromTo_Wind[,1]
# Tranfer_RegionFromTo_Wind <- Tranfer_RegionFromTo_Wind[,-1]
# save(Tranfer_RegionFromTo_CSP,Tranfer_RegionFromTo_PV,Tranfer_RegionFromTo_Wind,file="solveGen_data/Tranfer_RegionFromTo.Rdata")
load("solveGen_data/Tranfer_RegionFromTo.Rdata")

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
# NewPPcost = read.csv("solveGen_data/NewTechnologyCosts_solveGEN.csv")  ## A proxy set of cost input data for newly installed power plants
# NewPPcost <- NewPPcost[order(NewPPcost$X),]
# assign('NewPPcost',NewPPcost,envir=.GlobalEnv)
# StorageCost = read.csv("solveGen_data/StorageTechnologyCosts_solveGEN.csv") ## A proxy set of cost for electricity storage technologies
# assign('StorageCost',StorageCost,envir=.GlobalEnv)
# save(NewPPcost,StorageCost,file="solveGen_data/PowerPlantCosts.Rdata")
load("solveGen_data/PowerPlantCosts.Rdata")

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
#load("generate8760_output.Rdata")  ## Loads the hourly generation profile saved by "generate8760.R"
#load("solveGen_data/Total_Hourly_MW_8760_CurrentRegion.Rdata")  ## Loads the hourly generation profile saved by "generate8760.R"
data=data.frame(seq(1,8760),Total_Hourly_MW_8760_CurrentRegion,rowSums(wind_8760_profile[,]),rowSums(PV_8760_profile[,]),rowSums(CSP_8760_profile[,]))
names(data)=c("Hour.ending","Load_MW","Wind_MW","SolarPV_MW","SolarCSP_MW")
#assign('data',data,envir=.GlobalEnv)


## +++++++++
## User desired fractions of electricity from each power plant type
## +++++++++
# Initialize "Frac_MWhDesired" for dispatchable technologies
## +++++++++++++++
## THe assumption for user inputs for hydro is that:
## 1. The user's desired fraction for TOTAL Hydro goes into "Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))]"
## 2. Then HydroNonDispatch is solved, and any remaining MWh from hydro is the assigned to: Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("HydroDispatch"))]
## +++++++++++++++
#cat(paste0("User input goes into Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired for `HydroNonDispatch'."),sep="\n")
#cat(paste0("Then any remaining MWh for total hydro is later assigned to Frac_MWhDesired_dispatchable$Fraction_MWhDesired for `HydroDispatch'."),sep="\n")
Frac_MWhDesired_dispatchable <- NewPPcost
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
Frac_MWhDesired_Nondispatchable <- NewPPcost
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
  ##break
}

## +++++++++++++++
## Calculate the variable cost of each new power plant technology for operating 1 hour.
## Costs are calculated in total $ per MW installed per hour.
## +++++++++++++++
PPcost_Variable <- rbind(Frac_MWhDesired_dispatchable,Frac_MWhDesired_Nondispatchable)
PPcost_Variable <- PPcost_Variable[order(PPcost_Variable$Technology),]
PPcost_Variable[,2] <- 0
names(PPcost_Variable) <- c("Technology","Cost_Variable")
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Coal"))] <-  (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("Coal"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("Coal"))])  ## Coal
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Nuclear"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("Nuclear"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("Nuclear"))])  ## Nuclear
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCC"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("NGCC"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("NGCC"))])  ## NGCC
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("NGCT"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("NGCT"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("NGCT"))])  ## NGCT
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("UserTech1"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("UserTech1"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("UserTech1"))])  ## UserTech1
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("UserTech2"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("UserTech2"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("UserTech2"))])  ## UserTech2
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Biomass"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("Biomass"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("Biomass"))])  ## Biomass
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("HydroDispatch"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("HydroDispatch"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("HydroDispatch"))])  ## Dispatchable Hydropower
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("PetroleumCC"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("PetroleumCC"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("PetroleumCC"))])  ## Petroleum CombinedCycle
PPcost_Variable$Cost_Variable[which(PPcost_Variable$Technology==c("Geothermal"))] <- (NewPPcost$VariableOMCost_..MWh[which(PPcost_Variable$Technology==c("Geothermal"))]+NewPPcost$VariableFuelCost_..MWh[which(PPcost_Variable$Technology==c("Geothermal"))])  ## Geothermal
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
PP_MWneeded <- cbind(PP_MWneeded,PPcost_Variable)  ## Just add 2 more columns to have as empty data
names(PP_MWneeded)[4]<-c("MW_needed")
PP_MWneeded <- PP_MWneeded[,-c(5)]
PP_MWneeded[,c(4)] <- 0
PP_MWneeded <- PP_MWneeded[order(PP_MWneeded$Cost_Variable),]

## Add data columns for the hourly MW generation from each type of generator
num_cols_now <- dim(data)[2]
zeros <- rep(0,8760)
data <- cbind(data,matrix(zeros , length(zeros) , (dim(Frac_MWhDesired_dispatchable)[1]+1) ))  ## add a column for generation each hour from each disptachable generator PLUS a column for nuclear generation

## ++++++++++++
## Determine Nuclear Capacity as running at a constant capacity each hour.
## ++++++++++++
PPindex <- num_cols_now+1  ## This is the column in data.frame "data" to add the hourly generation for the current power plant type
names(data)[PPindex] <- paste0("Nuclear_MW")
PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")] <- sum(data$Load_MW)/8760*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Nuclear")]
data$Nuclear_MW <- rep(1,8760)*PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]
if (max(data$Nuclear_MW) > min(data$Load_MW)) {
  Nuclear.MaxPct <- min(data$Load_MW)*8760/sum(data$Load_MW)
  cat(paste0("Your choice leads to ",sprintf("%.1f", max(data$Nuclear_MW))," MW of nuclear.",sep="\n"))
  cat(paste0("The maximum allowed capacity for nuclear is ",sprintf("%.1f", min(data$Load_MW))," MW to achieve up to ",sprintf("%.1f", Nuclear.MaxPct*100)," % of generation.",sep="\n"))
  cat(paste0("Reduce your desired % of MWh from nuclear."))
  break
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
# cat("The HydroDispatch works if the user specifies MORE HydroDispatch than is possible.",sep="\n")
# cat("The HydroNonDispatch works if the user specifies MORE HydroNonDispatch than is possible.",sep="\n")
# cat("HydroNonDispatch and HydroDispatch works if user wants LESS than total possible hydro MWh.",sep="\n")
cat("Hydro programming does not allow ADDING hydro capacity (MW) and energy (MWh).",sep="\n")

##  First check if user wants more total hydro than allowed by existing energy budget.
if ( (sum(NewPPcost[which(NewPPcost$X=="HydroNonDispatch"),9:13])+sum(NewPPcost[which(NewPPcost$X=="HydroDispatch"),9:13]))/sum(data$Load_MW) < (Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology==c("HydroDispatch"))] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("HydroNonDispatch"))] ) ) {
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
HydroNonDispatch_hourly_max[1] <- NewPPcost$MWh_max_WinterJanFeb[which(NewPPcost$X=="HydroNonDispatch")]/(hour_per_season[1])  ## This is Winter for Jan/Feb ONLY
HydroNonDispatch_hourly_max[2] <- NewPPcost$MWh_max_Spring[which(NewPPcost$X=="HydroNonDispatch")]/hour_per_season[2]
HydroNonDispatch_hourly_max[3] <- NewPPcost$MWh_max_Summer[which(NewPPcost$X=="HydroNonDispatch")]/hour_per_season[3]
HydroNonDispatch_hourly_max[4] <- NewPPcost$MWh_max_Fall[which(NewPPcost$X=="HydroNonDispatch")]/hour_per_season[4]
HydroNonDispatch_hourly_max[5] <- NewPPcost$MWh_max_WinterNovDec[which(NewPPcost$X=="HydroNonDispatch")]/(hour_per_season[5])  ## This is Winter for Nov/Dec ONLY
start.ind <- 1
for (i in 1:5) {
  end.ind <- start.ind + hour_per_season[i] - 1
  data$HydroNonDispatch_MW[start.ind:end.ind] <- rep(HydroNonDispatch_hourly_max[i],hour_per_season[i])
  start.ind <- end.ind + 1
}
## Calculate the maximum capacity factors (CFmax) for the existing hydropower fleet. 
# index.temp<-which(NewPPcost$X=="HydroNonDispatch")
# CFmax_HydroNonDispatch <- c(NewPPcost$MWh_max_WinterJanFeb[index.temp],NewPPcost$MWh_max_Spring[index.temp],NewPPcost$MWh_max_Summer[index.temp],NewPPcost$MWh_max_Fall[index.temp],NewPPcost$MWh_max_WinterNovDec[index.temp])/NewPPcost$MW_existing[index.temp]/hour_per_season
# data$CFmax_HydroNonDispatch <- c(rep(CFmax_HydroNonDispatch[1],hour_per_season[1]),rep(CFmax_HydroNonDispatch[2],hour_per_season[2]),rep(CFmax_HydroNonDispatch[3],hour_per_season[3]),rep(CFmax_HydroNonDispatch[4],hour_per_season[4]),rep(CFmax_HydroNonDispatch[5],hour_per_season[5]))
# index.temp<-which(NewPPcost$X=="HydroDispatch")
# CFmax_HydroDispatch <- c(NewPPcost$MWh_max_WinterJanFeb[index.temp],NewPPcost$MWh_max_Spring[index.temp],NewPPcost$MWh_max_Summer[index.temp],NewPPcost$MWh_max_Fall[index.temp],NewPPcost$MWh_max_WinterNovDec[index.temp])/NewPPcost$MW_existing[index.temp]/hour_per_season
# data$CFmax_HydroDispatch <- c(rep(CFmax_HydroDispatch[1],hour_per_season[1]),rep(CFmax_HydroDispatch[2],hour_per_season[2]),rep(CFmax_HydroDispatch[3],hour_per_season[3]),rep(CFmax_HydroDispatch[4],hour_per_season[4]),rep(CFmax_HydroDispatch[5],hour_per_season[5]))

PP_MWneeded_HydroNonDispatch <- min(HydroNonDispatch_hourly_max,NewPPcost$MW_existing[which(NewPPcost$X=="HydroNonDispatch")])  ## maximum hourly MW output from HydroNonDispatch
cat(paste0("Here still need to solve for whether hourly MW of HydroNonDispatch can increase with new capacity.",sep="\n"))
## Account for any possible curtailment. Here, there is NO adjustment of hydro capacity if there is curtailment.
## There is only a reduction in the hourly output as needed to ensure "Nuclear + HydroNonDispatch" generation <= total load.
data$Load_minus_Nuclear_MW <- data$Load_MW - data$Nuclear_MW
data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW
if ( length(which(data$Load_minus_Nuclear_MW_and_NonDispatchHydro_MW<0)) != 0 )  {
  # if (max(data$HydroNonDispatch_MW) > min(data$Load_minus_Nuclear_MW)) {
  #   cat(paste0("Your choice leads to ",sprintf("%.1f", max(data$HydroNonDispatch_MW))," MW of non-dispatchable hydro.",sep="\n"))
  #   cat(paste0("The maximum amount of non-dispatchable hydro is ",sprintf("%.1f", min(data$Load_minus_Nuclear_MW))," MW.",sep="\n"))
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

## Check if HydroNonDispatch hourly MW output exceeds the maximum MW of capacity allowed to be installed.
# if (max(data$HydroNonDispatch_MW/data$CFmax_HydroNonDispatch) > NewPPcost$MW_maximum[which(NewPPcost$X=="HydroNonDispatch")]) {
#   cat(paste0("Your choice leads to ",sprintf("%.1f", max(data$HydroNonDispatch_MW/data$CFmax_HydroNonDispatch))," MW of non-dispatchable hydro.",sep="\n"))
#   cat(paste0("The maximum amount of non-dispatchable hydro capacity is ",sprintf("%.1f", NewPPcost$MW_maximum[which(NewPPcost$X=="HydroNonDispatch")])," MW.",sep="\n"))
#   cat(paste0("Reduce your desired % of MWh from non-dispatchable hydro."))
#   break
# }

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
## Solve for HydroDispatch later ...
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
  ## calculate the fraction of total load served by wind and solar
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
  # MWgeneration_to_subtract <- net_load_minus_MWgeneration
  # MWgeneration_to_subtract[MWgeneration_to_subtract>0] <- 0
  # MWgeneration <- MWgeneration_temp + MWgeneration_to_subtract
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
      
      ## Calculate area2
      # hr_end_area2 <- min(which(net_load_duration_input$MW<(net_load_duration_input$MW[hr_transition] - NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")])) )  ## This returns "hr_end_area2 = Inf" if there are no hours when any of "net_load_duration_input$MW" are less than the quantity in question
      # if ( (is.infinite(hr_end_area2)==TRUE) | (hr_end_area2>hours_in_season) ) {
      #   hr_end_area2 = hours_in_season
      # }
      #hr_end_area2_vector <- which(net_load_duration_input$MW<(net_load_duration_input$MW[hr_transition] - max_capacity))
      hr_end_area2_vector <- which(net_load_duration_input$MW<(net_load_duration_input$MW[hr_transition] - max_hydro_dispatch_possible))
      #hr_end_area2_vector <- 
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
        # net_load_duration_input$HydroDispatch_MW[1:hr_transition] <- NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]
        #net_load_duration_input$HydroDispatch_MW[1:hr_transition] <- max_capacity
        net_load_duration_input$HydroDispatch_MW[1:hr_transition] <- pmin(net_load_duration_input$MW[1:hr_transition],rep(max_hydro_dispatch_possible,hr_transition))
        if ( hr_end_area2 > (hr_transition+1) ) {
          ## Need to figure out how to detrmine dispatch each hour from "(hr_transition+1)" to "hr_end_area2"
          net_load_duration_input$HydroDispatch_MW[(hr_transition+1):hr_end_area2] <- net_load_duration_input$MW[(hr_transition+1):hr_end_area2] - net_load_duration_input$MW[hr_end_area2]
        } else {
          ## Have to individually specify the HydroDispatch for the last hour (lowest net load) for this season.
          # if ( (hydro_budget-(area1+area2)) > NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")] ) {
          #if ( (hydro_budget-(area1+area2)) > max_capacity ) {
          if ( (hydro_budget-(area1+area2)) > max_hydro_dispatch_possible ) {
            ## If this is the case, then there is more energy budget than can be dispatched at the maximum capacity.
            ## Thus, the HydroDispatch for this last hour of net load is dispatched at the exising MW cacpacity for HydroDispatch.
            # net_load_duration_input$HydroDispatch_MW[hr_end_area2] <- NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]
            #net_load_duration_input$HydroDispatch_MW[hr_end_area2] <- max_capacity
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
        # rm(net_load_storage_tiny,storage_budget_tiny)
        # net_load_duration_input$HydroDispatch_MW[i] <- hydro_budget  ## This is a simple answer that dispatches all of "hydro_budget" in the peak hour of net load (and no other hours)
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


# ## ++++++++++++
# ## BEGIN: FUNCTION function_Wind_PV_CSP_daily_storage
# ## FUNCTION for evaluationg the objective function to determine wind and solar capacity (both PV and CSP)
# ## when assuming THERE IS DAILY DISPATCH OF STORAGE OF WIND AND SOLAR ELECTRICITY.
# ## ++++++++++++
# function_Wind_PV_CSP_daily_storage <- function(x){
#   multiplier.wind <- x[1]
#   multiplier.PV <- x[2]
#   multiplier.CSP <- x[3]
#   MW_capacity_DailyStorage <- x[4]
#   SOC_initial <- x[5]     ## intial state of charge (charge on December 31 of previous year), in units of MWh
# 
#   multiplied.wind <- multiplier.wind*data$Wind_MW
#   multiplied.PV <- multiplier.PV*data$SolarPV_MW
#   multiplied.CSP <- multiplier.CSP*data$SolarCSP_MW
#   ## "net load" here is = Load - Nuclear - HydroNonDispatch - Wind - PV
#   net_load <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW - multiplied.wind - multiplied.PV - multiplied.CSP
#   ind <- which(net_load<0)  ## These are the indices of the net load vector that are negative
# 
#   # ## +++++++++++++++++++++++++
#   # ## Subroutine for storage dispatch - BEGIN
#   # ## +++++++++++++++++++++++++
#   # StorageDispatchDaily_bottom <- rep(0,dim(data)[1])  ## nrow = maximum number of days in any season
#   # StorageDispatchDaily <- StorageDispatchDaily_bottom
#   # ## Go through each day and assume that storage can distpatch to take the peak net load
#   # ## that is left, but limited by (1) capacity (MW output at any hour) and (2) energy (MWh) state of charge (SOC).
#   # StorageDispatchDaily_bottom <- rep((max(net_load[net_load>0])-MW_capacity_storage),length(net_load))
#   # ## Start at peak net_load and add up all MWh generated if adding up the "area"
#   # ## of the net_load curve from the top down, or between the net_load values as
#   # ## bottom to top == (peak net_load - multiplier) to (peak net_load).
#   # StorageDispatchDaily <- net_load - StorageDispatchDaily_bottom
#   # ## calculate the fraction of total load served by dispatched storage
#   # StorageDispatchDaily[StorageDispatchDaily<0] <- 0
#   # frac_storage.MWh <- sum(StorageDispatchDaily)/sum(data$Load_MW)
#   # ## +++++++++++++++++++++++++
#   # ## Subroutine for storage dispatch - END
#   # ## +++++++++++++++++++++++++
# 
#   ## +++++++++++++++++
#   ## First: Calculate curtailed wind and solar (both PV and CSP) generation.
#   ## This curtailment is what is potentially stored.
#   ## Subtract a portion of curtailment from the current amount of wind and PV in "multiplied.wind"
#   ## "multiplied.PV", and "multiplied.CSP".  The wind and solar are curtailed in proportion to how much they are generating.
#   ## So if you have to curtail 1000 MW, and at that hour there is 10,000 MW of wind and 5,000 MW of PV,
#   ## then PV is 33% of the total and wind is 67% of total "wind + solar", and wind will be curtailed
#   ## 1000MW*67% = 667 MW and PV will be curtailed 1000MW*33% = 333 MW.
#   ## +++++++++++++++++
#   curtailed_WindSolar <- net_load
#   curtailed_WindSolar[curtailed_WindSolar>0]=0
#   curtailed_WindSolar <- -curtailed_WindSolar
#   noncurtailed_WindSolar <- multiplied.wind+multiplied.PV+multiplied.CSP
#   wind_fraction = multiplied.wind/noncurtailed_WindSolar
#   PV_fraction = multiplied.PV/noncurtailed_WindSolar
#   CSP_fraction = multiplied.CSP/noncurtailed_WindSolar
#   curtailed_Wind <- curtailed_WindSolar*wind_fraction
#   curtailed_PV   <- curtailed_WindSolar*PV_fraction
#   curtailed_CSP  <- curtailed_WindSolar*CSP_fraction
#   curtailed_Wind[is.na(curtailed_Wind)==TRUE] <- 0  ## ensure there are no NA from dividing by zero
#   curtailed_PV[is.na(curtailed_PV)==TRUE] <- 0      ## ensure there are no NA from dividing by zero
#   curtailed_CSP[is.na(curtailed_CSP)==TRUE] <- 0    ## ensure there are no NA from dividing by zero
# 
#   ## Second: calculate how much wind and PV is put onto grid after removing curtailed MW from their non-curtailed MW generation
#   multiplied.wind_DirectToGrid <- multiplied.wind
#   multiplied.wind_DirectToGrid <- multiplied.wind - curtailed_Wind
#   # multiplied.wind_DirectToGrid[ind] <- multiplied.wind[ind] + net_load[ind]*wind_fraction[ind]
#   multiplied.PV_DirectToGrid <- multiplied.PV
#   multiplied.PV_DirectToGrid <- multiplied.PV - curtailed_PV
#   multiplied.CSP_DirectToGrid <- multiplied.CSP
#   multiplied.CSP_DirectToGrid <- multiplied.CSP - curtailed_CSP
# 
#   ## +++++++++++++++++
#   ## Assume that we can neglect the initial state of charge (SOC_initial) by assuming that 
#   ## 1. the storage system stores all curtailed wind and solar (= "curtailed_WindSolar")
#   ## 2. the storage system, over the course of the year, cannot dispatches more than "curtailed_WindSolar*efficiency_OneWay_DailyStorage"
#   ## 3. if the system needed an initial state of charge (say on January 1 at midnight), then this could be provided by carrying over 
#   ##    storage from the previous year as long as it is not larger than the maximum total MWh storage capacity needed on any other day point during the year.
#   ## The storage algorithm works as follows:
#   ## 1. Assume that "charging" only occurs for wind and solar generation that would otherwise be curtailed.
#   ##    Thus, storage "charging" cannot occur when net load is >= 0.
#   ## 2. Storage "discharging" can only occur when net load is >= 0.
#   ## 3. The MW capacity for "charging" equals the MW capacity for "discharging".
#   ## 4. The MW capacity for "charging" is an optimized variable defined as the maximum amount of "otherwise curtailed wind and solar" (or negative net load) experienced.
#   ## +++++++++++++++++
#   capacity.wind <- MW_data_Wind*multiplier.wind
#   capacity.PV <- MW_data_PV*multiplier.PV
#   capacity.CSP <- MW_data_CSP*multiplier.CSP
#   StoredWindSolar <- curtailed_WindSolar*efficiency_OneWay_DailyStorage^2                ## First, assume all curtailed wind and solar is stored, subtracting MWh due to efficiency loss
#   StoredWindSolar[StoredWindSolar>MW_capacity_DailyStorage] <- MW_capacity_DailyStorage  ## Next, this says I can't store energy faster in any given hour, due to power constraint, than the MW power capacity of the storage system
#   StoredWind <- StoredWindSolar*curtailed_Wind/(curtailed_Wind+curtailed_PV+curtailed_CSP)
#   StoredPV <- StoredWindSolar*curtailed_PV/(curtailed_Wind+curtailed_PV+curtailed_CSP)
#   StoredCSP <- StoredWindSolar*curtailed_CSP/(curtailed_Wind+curtailed_PV+curtailed_CSP)
#   StoredWind[is.nan(StoredWind)=="TRUE"]=0
#   StoredPV[is.nan(StoredPV)=="TRUE"]=0
#   StoredCSP[is.nan(StoredCSP)=="TRUE"]=0
#   
#   ## Go through each day "day_now" to determine storage dispatch that can only come from electricity
#   ## that was already stored on day "day_now - 1".  This mimics the idea of day-ahead committment of generation.
#   # SOC_initial <- StoredWindSolar[length(StoredWindSolar)]  ## assume final storage left on Dec. 31 is carried over to be used on January 1.
#   # StoredWindSolar_cumulative <- SOC_initial + cumsum(StoredWindSolar)
#   # Dispatched_StoredWindSolar <- positive_net_load  ## This is all times when net load is positive, and thus during which storage can potentially be dispatched
#   # Dispatched_StoredWindSolar[Dispatched_StoredWindSolar>MW_capacity_DailyStorage] <- MW_capacity_DailyStorage  ## This limits discharge any hour to the MW capacity of the storage technology
#   ##
#   ## Dispatch storage on "day_now" using storage existing on previous "day_now - 1" to reduce the peak load hours on "day_now"
#   ## using as much energy as is stored or until all net_load has been dispatched using storage.
#   ## Use the function "function_solve_hydro_dispatch" that was derived for dispatching seasonal HydroDispatch
#   ## because we can use the same algorithm (e.g., a limited storage budget dispatched ideally for a given net load)
#   hrs_per_day = 24
#   positive_net_load <- net_load
#   positive_net_load[positive_net_load<0] <- 0
#   Dispatched_StoredWindSolar <- rep(0,dim(data)[1])  ## Initialize a data frame of hourly storage dispatch
#   Dispatched_StoredWindSolar <- matrix(0, ncol = 1, nrow = dim(data)[1])  
#   Dispatched_StoredWindSolar <- data.frame(Dispatched_StoredWindSolar)
#   names(Dispatched_StoredWindSolar) <- c("MW")
#   Dispatched_StoredWindSolar$Hrs <- seq(1,dim(data)[1])
#   StorageBudget_day_now = rep(0,length(positive_net_load)/hrs_per_day)  ## Initialize a data frame of daily storage budget (same idea as seasonal storage budget for HydroDispatch)
#   StorageBudget_day_now[1] = SOC_initial
#   net_load_duration_day <- matrix(0, ncol = 1, nrow = hrs_per_day)  ## Initialize a data frame that will be the daily net load duration curve that is updated as the dispatch function is called for each day of the year
#   net_load_duration_day <- data.frame(net_load_duration_day)
#   names(net_load_duration_day) <- c("MW")
#   net_load_duration_day$Hrs <- seq(1,hrs_per_day)
#   net_load_duration_day$HydroDispatch_MW <- rep(0,dim(net_load_duration_day)[1])  ## here the term "HydroDispatch" is used becuase we use the "function_solve_hydro_dispatch" that already assumed that terminology
#   net_load_duration_day$MW <- positive_net_load[1:hrs_per_day]
#   net_load_duration_day <- net_load_duration_day[order(-net_load_duration_day$MW),]
#   hr_max <- min((hrs_per_day-1),ceiling(StorageBudget_day_now[1]/MW_capacity_DailyStorage)) ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hours in the day
#   ## Solve storage dispatch each day
#   for (i in 2:(length(positive_net_load)/hrs_per_day)) {  ## Go through each day
#     ## Determine dispatch in day "i-1"
#     if (StorageBudget_day_now[i-1]>0) {
#       storage_day_now_outputs <- function_solve_hydro_dispatch(hr_max,MW_capacity_DailyStorage,StorageBudget_day_now[i-1],net_load_duration_day,hrs_per_day)
#       Dispatched_StoredWindSolar$MW[((i-2)*hrs_per_day+1):((i-1)*hrs_per_day)] <- storage_day_now_outputs$HydroDispatch_MW
#       ## Now items are being updated to determine dispatch in day "i"
#       StorageBudget_day_now[i] = StorageBudget_day_now[i-1] + sum(StoredWindSolar[((i-1)*hrs_per_day+1):(i*hrs_per_day)]) - sum(storage_day_now_outputs$HydroDispatch_MW) ## budget from yesterdaty + what you stored today - what you dispatched today
#     } else {
#       Dispatched_StoredWindSolar$MW[((i-2)*hrs_per_day+1):((i-1)*hrs_per_day)] <- rep(0,hrs_per_day)
#       ## Now items are being updated to determine dispatch in day "i"
#       StorageBudget_day_now[i] = StorageBudget_day_now[i-1] + sum(StoredWindSolar[((i-1)*hrs_per_day+1):(i*hrs_per_day)]) ## budget from yesterdaty + what you stored today - what you dispatched today
#     } ##END: else with ... if (StorageBudget_day_now[i]>0) {
#       ## Create next day's net load duration curve
#       net_load_duration_day$MW <- positive_net_load[((i-1)*hrs_per_day+1):(i*hrs_per_day)]
#       net_load_duration_day$Hrs <- seq(((i-1)*hrs_per_day+1),(i*hrs_per_day))
#       net_load_duration_day$HydroDispatch_MW <- rep(0,dim(net_load_duration_day)[1])  ## Reset this to zeros. Here the term "HydroDispatch" is used becuase we use the "function_solve_hydro_dispatch" that already assumed that terminology
#       net_load_duration_day <- net_load_duration_day[order(-net_load_duration_day$MW),]
#       hr_max <- max((hrs_per_day-1),ceiling(StorageBudget_day_now[i]/MW_capacity_DailyStorage)) ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hours in the day
#   } ##END: for (i in 2:(length(positive_net_load)/hrs_per_day)) {
#   ## Figure out how much of wind, PV, and CSP were dispatched as storage.
#   ## For simplicity, assume storage dispatched from each of wind, PV, and CSP
#   ## occurs at the same fraction each was curtailed each day. This should be close enough.
#   Dispatched_StoredWindSolar_cumulative <- cumsum(Dispatched_StoredWindSolar$MW)
#   StoredWind_cumulative <- cumsum(StoredWind)
#   StoredPV_cumulative <- cumsum(StoredPV)
#   StoredCSP_cumulative <- cumsum(StoredCSP)
#   last_yr <- length(Dispatched_StoredWindSolar_cumulative)
#   Dispatched_StoredWind <- StoredWind_cumulative[last_yr]*(StoredWind_cumulative[last_yr]+StoredPV_cumulative[last_yr]+StoredCSP_cumulative[last_yr])/Dispatched_StoredWindSolar_cumulative[last_yr]
#   Dispatched_StoredPV <- StoredPV_cumulative[last_yr]*(StoredWind_cumulative[last_yr]+StoredPV_cumulative[last_yr]+StoredCSP_cumulative[last_yr])/Dispatched_StoredWindSolar_cumulative[last_yr]
#   Dispatched_StoredCSP <- StoredCSP_cumulative[last_yr]*(StoredWind_cumulative[last_yr]+StoredPV_cumulative[last_yr]+StoredCSP_cumulative[last_yr])/Dispatched_StoredWindSolar_cumulative[last_yr]
# 
#   # ## +++++++++++++++++++++
#   # ## TESTING STORAGE FUNCTION
#   # ## +++++++++++++++++++++
#   # plot(cumsum(StorageDispatch),type = "l",lty=1,col="black")
#   # #  plot(cumsum(StorageDispatch),type = "l",ylim = c(0,1e6),lty=1,col="black")
#   # par(new=TRUE)
#   # plot(cumsum(curtailed_WindSolar),type = "l",ylim = c(0,1e6),lty=1,col="blue")
#   # par(new=TRUE)
#   # plot(cumsum(StoredElectricity_max),type = "l",ylim = c(0,1e6),lty=2,col="red")
# 
#   ## +++++++++++++++++
#   ## Calculate the fraction of total load served by wind and solar to use
#   ## in the objective function.
#   ## +++++++++++++++++
#   frac.wind <- sum(multiplied.wind_DirectToGrid,Dispatched_StoredWind)/sum(data$Load_MW)
#   frac.PV <- sum(multiplied.PV_DirectToGrid,Dispatched_StoredPV)/sum(data$Load_MW)
#   frac.CSP <- sum(multiplied.CSP_DirectToGrid,Dispatched_StoredCSP)/sum(data$Load_MW)
#   ## Calculate capital cost of wind, PV, CSP, and storage to target minimum capital cost alongside meeting the user target for the % of wind, PV, and CSP
#   capital_cost <- max(MW_capacity_DailyStorage*StorageCost$CAPEX_DollarsPerkW[which(StorageCost$X=="DailyStorage")],sum(Dispatched_StoredWindSolar$MW)*StorageCost$CAPEX_DollarsPerkWh[which(StorageCost$X=="DailyStorage")]) + 
#     capacity.wind*NewPPcost$AnnualizedCapitalCost_k..MWyear[which(NewPPcost$X=="Wind")]+capacity.PV*NewPPcost$AnnualizedCapitalCost_k..MWyear[which(NewPPcost$X=="PV")]+capacity.CSP*NewPPcost$AnnualizedCapitalCost_k..MWyear[which(NewPPcost$X=="CSP")]
#   return(1e-8*capital_cost + sum(1e7*( (frac.wind-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Wind")])^2 + (frac.PV-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="PV")])^2 + (frac.CSP-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="CSP")])^2 ) ))
# }
# ## ++++++++++++
# ## END: FUNCTION function_Wind_PV_CSP_daily_storage
# ## FUNCTION for evaluationg the objective function to determine wind and solar capacity (both PV and CSP)
# ## when assuming THERE IS DAILY DISPATCH OF STORAGE OF WIND AND SOLAR ELECTRICITY.
# ## ++++++++++++

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
  #MW_capacity_AnnualStorage <- x[4]
  #SOC_initial <- x[5]     ## intial state of charge (charge on December 31 of previous year), in units of MWh
  
  multiplied.wind <- multiplier.wind*data$Wind_MW
  multiplied.PV <- multiplier.PV*data$SolarPV_MW
  multiplied.CSP <- multiplier.CSP*data$SolarCSP_MW
  ## "net load" here is = Load - Nuclear - HydroNonDispatch - Wind - PV
  net_load <- data$Load_MW - data$Nuclear_MW - data$HydroNonDispatch_MW - multiplied.wind - multiplied.PV - multiplied.CSP
  ind <- which(net_load<0)  ## These are the indices of the net load vector that are negative
  
  # ## +++++++++++++++++++++++++
  # ## Subroutine for storage dispatch - BEGIN
  # ## +++++++++++++++++++++++++
  # StorageDispatchDaily_bottom <- rep(0,dim(data)[1])  ## nrow = maximum number of days in any season
  # StorageDispatchDaily <- StorageDispatchDaily_bottom
  # ## Go through each day and assume that storage can distpatch to take the peak net load
  # ## that is left, but limited by (1) capacity (MW output at any hour) and (2) energy (MWh) state of charge (SOC).
  # StorageDispatchDaily_bottom <- rep((max(net_load[net_load>0])-MW_capacity_storage),length(net_load))
  # ## Start at peak net_load and add up all MWh generated if adding up the "area"
  # ## of the net_load curve from the top down, or between the net_load values as
  # ## bottom to top == (peak net_load - multiplier) to (peak net_load).
  # StorageDispatchDaily <- net_load - StorageDispatchDaily_bottom
  # ## calculate the fraction of total load served by dispatched storage
  # StorageDispatchDaily[StorageDispatchDaily<0] <- 0
  # frac_storage.MWh <- sum(StorageDispatchDaily)/sum(data$Load_MW)
  # ## +++++++++++++++++++++++++
  # ## Subroutine for storage dispatch - END
  # ## +++++++++++++++++++++++++
  
  ## +++++++++++++++++
  ## First: Calculate curtailed wind and solar (both PV and CSP) generation.
  ## This curtailment is what is potentially stored.
  ## Subtract a portion of curtailment from the current amount of wind and PV in "multiplied.wind"
  ## "multiplied.PV", and "multiplied.CSP".  The wind and solar are curtailed in proportion to how much they are generating.
  ## So if you have to curtail 1000 MW, and at that hour there is 10,000 MW of wind and 5,000 MW of PV,
  ## then PV is 33% of the total and wind is 67% of total "wind + solar", and wind will be curtailed
  ## 1000MW*67% = 667 MW and PV will be curtailed 1000MW*33% = 333 MW.
  ## +++++++++++++++++
  curtailed_WindSolar <- net_load
  curtailed_WindSolar[curtailed_WindSolar>0]=0
  curtailed_WindSolar <- -curtailed_WindSolar
  noncurtailed_WindSolar <- multiplied.wind+multiplied.PV+multiplied.CSP
  wind_fraction = multiplied.wind/noncurtailed_WindSolar
  PV_fraction = multiplied.PV/noncurtailed_WindSolar
  CSP_fraction = multiplied.CSP/noncurtailed_WindSolar
  curtailed_Wind <- curtailed_WindSolar*wind_fraction
  curtailed_PV   <- curtailed_WindSolar*PV_fraction
  curtailed_CSP  <- curtailed_WindSolar*CSP_fraction
  curtailed_Wind[is.na(curtailed_Wind)==TRUE] <- 0  ## ensure there are no NA from dividing by zero
  curtailed_PV[is.na(curtailed_PV)==TRUE] <- 0      ## ensure there are no NA from dividing by zero
  curtailed_CSP[is.na(curtailed_CSP)==TRUE] <- 0    ## ensure there are no NA from dividing by zero
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
  ## 4. The MW capacity for "charging" is an optimized variable defined as the maximum amount of "otherwise curtailed wind and solar" (or negative net load) experienced.
  ## +++++++++++++++++
  capacity.wind <- MW_data_Wind*multiplier.wind
  capacity.PV <- MW_data_PV*multiplier.PV
  capacity.CSP <- MW_data_CSP*multiplier.CSP
  StoredWindSolar <- curtailed_WindSolar*efficiency_OneWay_DailyStorage^2                ## First, assume all curtailed wind and solar is stored, subtracting MWh due to efficiency loss
  StoredWindSolar[StoredWindSolar>MW_capacity_AnnualStorage] <- MW_capacity_AnnualStorage  ## Next, this says I can't store energy faster in any given hour, due to power constraint, than the MW power capacity of the storage system
  StoredWindSolar_cumulative <- cumsum(StoredWindSolar) ## Cumulative storage of wind and solar MWh
  #StoredWindSolar_cumulative <- SOC_initial + StoredWindSolar_cumulative        ## Set intial cumulative storage as the initial State of Charge
  StoredWind <- StoredWindSolar*curtailed_Wind/(curtailed_Wind+curtailed_PV+curtailed_CSP)
  StoredPV <- StoredWindSolar*curtailed_PV/(curtailed_Wind+curtailed_PV+curtailed_CSP)
  StoredCSP <- StoredWindSolar*curtailed_CSP/(curtailed_Wind+curtailed_PV+curtailed_CSP)
  StoredWind[is.nan(StoredWind)=="TRUE"]=0
  StoredPV[is.nan(StoredPV)=="TRUE"]=0
  StoredCSP[is.nan(StoredCSP)=="TRUE"]=0
  
  ## Go through each day "day_now" to determine storage dispatch that can only come from electricity
  ## that was already stored on day "day_now - 1".  This mimics the idea of day-ahead committment of generation.
  # SOC_initial <- StoredWindSolar[length(StoredWindSolar)]  ## assume final storage left on Dec. 31 is carried over to be used on January 1.
  # StoredWindSolar_cumulative <- SOC_initial + cumsum(StoredWindSolar)
  # Dispatched_StoredWindSolar <- positive_net_load  ## This is all times when net load is positive, and thus during which storage can potentially be dispatched
  # Dispatched_StoredWindSolar[Dispatched_StoredWindSolar>MW_capacity_AnnualStorage] <- MW_capacity_AnnualStorage  ## This limits discharge any hour to the MW capacity of the storage technology
  ##
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
  net_load_duration$HydroDispatch_MW <- rep(0,dim(net_load_duration)[1])  ## here the term "HydroDispatch" is used becuase we use the "function_solve_hydro_dispatch" that already assumed that terminology
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
  
  ## +++++++++++++++++
  ## Calculate the fraction of total load served by wind and solar to use
  ## in the objective function.
  ## +++++++++++++++++
  ## Need the fraction of generation from wind, PV, and CSP
  frac.wind <- sum(multiplied.wind_DirectToGrid,Dispatched_StoredWind)/sum(data$Load_MW)
  frac.PV <- sum(multiplied.PV_DirectToGrid,Dispatched_StoredPV)/sum(data$Load_MW)
  frac.CSP <- sum(multiplied.CSP_DirectToGrid,Dispatched_StoredCSP)/sum(data$Load_MW)
  if (Dispatched_StoredWind>0 | Dispatched_StoredPV>0 | Dispatched_StoredCSP>0) {
    hey = 1
  }
  if (StoredWind_cumulative[last_yr]>0 | StoredPV_cumulative[last_yr]>0 | StoredCSP_cumulative[last_yr]>0) {
    hey = 1
  }
  aa<- sum(1e7*( (frac.wind-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="Wind")])^2 + (frac.PV-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="PV")])^2 + (frac.CSP-Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="CSP")])^2 ) )
  if (aa<1) {
    hey=1
  }
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
ub = rep(1e9,3)   ## lower bound on multipliers for wind and solar profiles
init_guesses <- c(multiplier_init.wind,multiplier_init.PV,multiplier_init.CSP)
max_iters <- 200
tolerance_NoStorage <- 1e10
##wind_solar_multipliers <- Rcgmin(fn=function_Wind_PV_CSP,lower=lb,par=init_guesses,control=list(dowarn=FALSE,maxit=max_iters))
#wind_solar_multipliers <- optimr(par=init_guesses,fn=function_Wind_PV_CSP,gr=NULL,lower=lb,upper=ub,method=NULL,hessian=NULL,control=list(dowarn=FALSE,maxit=max_iters,abstol=tolerance_NoStorage))
wind_solar_multipliers <- optim(par=init_guesses,fn=function_Wind_PV_CSP,lower=lb,upper=ub,method = c("L-BFGS-B"),control=list(maxit=max_iters,factr=tolerance_NoStorage))
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
for (i in 1:(dim(Frac_MWhDesired_dispatchable)[1]-2)) {  ## Subtract 2 for 1) NGCC and 2) NGCT
  names(data)[PPindex+i] <- paste0(PP_MWneeded$Technology[i],"_MW")
  assign("PPnumber",i) ## Find capacity of the 1st cheapest dispatchable generator (i=1) with Frac_MWhDesired > 0
  
  ## ++++++++++++++
  ## Call optimization to determine the MW capacity of dispatchable technologies 
  ## chosen in order from cheapest to most expensive technology to run at 100% capacity.
  ## ++++++++++++++
  multiplier_init.dispatch <- 0.1  ## initial guess at how much to multiply existing PV profile
  lb = c(0)   ## lower bound on multipliers for wind and solar profiles
  init_guesses <- c(multiplier_init.dispatch)
  if (names(data)[PPindex+i] != "HydroDispatch_MW") {
    ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible later in the code.
    MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_generators,lower=lb,par=init_guesses,control=list(dowarn=FALSE))
    
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
  data$net_load_WithCurtailment <- data$net_load_WithCurtailment - MWgeneration
  data[,i+PPindex] <- MWgeneration
  rm(MWgeneration_temp,MWgeneration_to_subtract,MWgeneration)
} ## for (i in 1:dim(Frac_MWhDesired_dispatchable)[1]) {
## +++++++++++++++++
## END
## Perform optimization to find the capacity of dispatchables in the actual time series of net_load.
## This for loop solves for dispatchable generation when there is NO STORAGE of any kind.
## +++++++++++++++++


## +++++++++++++++++
## Input the MW of installed capacity needed for Wind and PV into "PP_MWneeded".
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
## output from all power plants .
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
HydroDispatch_budget_winter <- NewPPcost$MWh_max_WinterJanFeb[which(NewPPcost$X=="HydroDispatch")]+NewPPcost$MWh_max_WinterNovDec[which(NewPPcost$X=="HydroDispatch")]
hr_max_winter <- ceiling(HydroDispatch_budget_winter/NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")])
if (hr_max_winter>sum(hour_per_season[c(1,5)])) {
  hr_max_winter = sum(hour_per_season[c(1,5)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_winter <- NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]
## Spring
HydroDispatch_budget_spring <- NewPPcost$MWh_max_Spring[which(NewPPcost$X=="HydroDispatch")]
hr_max_spring <- ceiling(HydroDispatch_budget_spring/NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")])
if (hr_max_spring >sum(hour_per_season[c(2)])) {
  hr_max_spring  = sum(hour_per_season[c(2)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_spring <- NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]
## Summer
HydroDispatch_budget_summer <- NewPPcost$MWh_max_Summer[which(NewPPcost$X=="HydroDispatch")]
hr_max_summer <- ceiling(HydroDispatch_budget_summer/NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")])
if (hr_max_summer >sum(hour_per_season[c(3)])) {
  hr_max_summer  = sum(hour_per_season[c(3)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_summer <- NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]
## Fall
HydroDispatch_budget_fall <- NewPPcost$MWh_max_Fall[which(NewPPcost$X=="HydroDispatch")]
hr_max_fall <- ceiling(HydroDispatch_budget_fall/NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")])
if (hr_max_fall >sum(hour_per_season[c(4)])) {
  hr_max_fall  = sum(hour_per_season[c(4)]) - 1  ## Here we need to make the function "function_solve_hydro_dispatch" start at an hour that is lower than the maximum hour in the season
}
max_capacity_fall <- NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")]

#if (NotEnoughHydro == 1) { 
## NotEnoughHydro == 1.
## Here we solve for hydro dispatch by reducing the peak net loads each season as much as possible usign HydroDispatch.
## This is done by analyzing the net load duration curve for each season.

## Third:  Go season by season and find the hours and quantity of dispatch to use up the entire energy budget
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
  # ## Then solve for some level of HydroDispatch that is < total energy budget possible
  # init_guesses <- rep(0.5*NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")],5)  ## initial guess at how much to multiply existing PV profile
  # lb = rep(0,5)   ## lower bound on MW capacity for dispatchable hydro
  # ub = rep(NewPPcost$MW_maximum[which(NewPPcost$X=="HydroDispatch")],5)   ## upper bound on MW capacity for dispatchable hydro
  # ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible.
  # MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_hydro_seasonally,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE))
  
  ## Here, if the user does not want to have as much total hydro MWh, HydroDispatch + HydroNonDispatch,
  ## then to meet the user's requirements, we subtract some amount of hydro generation from that already 
  ## solved as if the total amount of hydro energy (MWh) budget were used.
  user_hydro_budget <- sum(data$Load_MW)*(Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology=="HydroDispatch")] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")])
  HydroNonDispatch_budget <- NewPPcost$MWh_max_WinterJanFeb[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_WinterNovDec[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_Spring[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_Summer[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_Fall[which(NewPPcost$X=="HydroNonDispatch")]
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
    for (i in 1:hr_end) { ## 
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
ind.NGCC <- which(NewPPcost$X==c("NGCC"))
ind.NGCT <- which(NewPPcost$X==c("NGCT"))
data$NGCC_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCC] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.NGCC]+NewPPcost$VariableFuelCost_..MWh[ind.NGCC])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
data$NGCT_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCT] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.NGCT]+NewPPcost$VariableFuelCost_..MWh[ind.NGCT])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
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
# multiplier_init.wind <- 0.5*multiplier.wind
# multiplier_init.PV <- 0.5*multiplier.PV
# multiplier_init.CSP <- 0.5*multiplier.CSP
multiplier_init.wind <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("Wind"))]  ## Wind
multiplier_init.PV <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("PV"))] ## Solar PV
multiplier_init.CSP <- 1e3*Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology==c("CSP"))] ## Solar CSP
assign('efficiency_OneWay_DailyStorage',0.95)  ## Make the storage efficiency a global variable
lb = c(0,0,0)   ## lower bound on multipliers for wind and solar profiles and storage parameters
# ub = c(3*max(data$Load_MW)/MW_data_Wind,10*max(data$Load_MW)/MW_data_PV,10*max(data$Load_MW)/MW_data_CSP)   ## upper bound on multipliers for wind and solar profiles and storage parameters
#ub = c(multiplier.wind,multiplier.PV,multiplier.CSP)   ## upper bound on multipliers for wind and solar profiles and storage parameters
#ub = c(multiplier.wind,multiplier.PV,multiplier.CSP)   ## upper bound on multipliers for wind and solar profiles and storage parameters
ub = rep(1e9,3)   ## lower bound on multipliers for wind and solar profiles
init_guesses <- c(multiplier_init.wind,multiplier_init.PV,multiplier_init.CSP)
max_iters <- 20
tolerance_AnnualStorage <- 1e12
cat("As of 4/3/19 I think `function_Wind_PV_CSP_annual_storage' works properly in combination with `hydro_dispatch' function.",sep="\n")
# ####wind_solar_AnnualStorage_multipliers <- Rcgmin(fn=function_Wind_PV_CSP_annual_storage,lower=lb,par=init_guesses,control=list(dowarn=FALSE,maxit=max_iters))
# wind_solar_AnnualStorage_multipliers <- Rcgmin(fn=function_Wind_PV_CSP_annual_storage,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE,maxit=max_iters))
#wind_solar_AnnualStorage_multipliers <- optimr(fn=function_Wind_PV_CSP_annual_storage,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE,maxit=max_iters,abstol=tolerance_AnnualStorage))
wind_solar_AnnualStorage_multipliers <- optim(par=init_guesses,fn=function_Wind_PV_CSP_annual_storage,lower=lb,upper=ub,method = c("L-BFGS-B"),control=list(maxit=max_iters,factr=tolerance_AnnualStorage))
multiplier_WithAnnualStorage.wind <- wind_solar_AnnualStorage_multipliers$par[1]
multiplier_WithAnnualStorage.PV   <- wind_solar_AnnualStorage_multipliers$par[2]
multiplier_WithAnnualStorage.CSP  <- wind_solar_AnnualStorage_multipliers$par[3]
# multiplier_WithAnnualStorage.wind <- 0  ## If you don't run annual grid storage algorithm
# multiplier_WithAnnualStorage.PV <- 0  ## If you don't run annual grid storage algorithm
# multiplier_WithAnnualStorage.CSP <- 0  ## If you don't run annual grid storage algorithm
## TEST inputs
#objective function value = 1.05
# multiplier_WithAnnualStorage.wind <- 28512.50 ## For 20% wind
# multiplier_WithAnnualStorage.PV   <- 48327.00 ## for 20% PV
# multiplier_WithAnnualStorage.CSP  <- 53316.02 ## for 20% CSP
  
# ## ++++
# ## Solve for PV, CSP and Wind WITH ANNUAL STORAGE using Genetic Algorithm
# ## ++++
# iter = 100
# GAmodel <- rbga(stringMin=lb,stringMax=ub,popSize = 100, iters = iter, elitism = T, evalFunc = function_Wind_PV_CSP_annual_storage)
# ###GAmodel <- rbga(stringMin=lb,stringMax=ub,popSize = 100, iters = iter, mutationChance = 0.1,elitism = T, evalFunc = function_Wind_and_PV)
# class(GAmodel)
# summary(GAmodel, echo=TRUE)
# solution_ind = min(which(GAmodel$evaluations==min(GAmodel$best)))           ## This extracts the values of the optimized variables
# solution = GAmodel$population[solution_ind,]                ## This extracts the values of the optimized variables
# #ymax <- max(GAmodel$mean[!is.infinite(GAmodel$mean)])   ## GAmodel$mean sometimes has "inf" values that we need to ignore to set the plotting limits
# ymax <- max(GAmodel$best[!is.infinite(GAmodel$best)])   ## GAmodel$mean sometimes has "inf" values that we need to ignore to set the plotting limits
# yrange1 <- c(0,ymax)
# plot(GAmodel$best,type = "l",col="red",ylim = yrange1)
# #par(new=TRUE)
# #plot(GAmodel$mean,type = "l",col="blue",ylim = yrange1)
# multiplier_WithAnnualStorage.wind <- solution[1]
# multiplier_WithAnnualStorage.PV <- solution[2]
# multiplier_WithAnnualStorage.CSP <- solution[3]
# # MW_capacity_AnnualStorage <- solution[4]

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

###SOC_initial_init <- 1e5                   ## Assumed initial state of charge (MWh) of the daily storage system
###MW_capacity_WithAnnualStorage_init <- 2e3  ## Assumed initial guess of the MW capacity of the daily storage system
###xx <- c(multiplier_WithAnnualStorage.wind,multiplier_WithAnnualStorage.PV,multiplier_WithAnnualStorage.CSP,MW_capacity_WithAnnualStorage,SOC_initial_WithAnnualStorage)
###output_temp <- function_Wind_PV_CSP_annual_storage(xx)
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
print(c(frac.wind_WithAnnualStorage,frac.PV_WithAnnualStorage,frac.CSP_WithAnnualStorage))
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
  init_guesses <- c(multiplier_init.dispatch)
  
  if (names(data)[new_column_number] != "HydroDispatch_MW") {
    ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible later in the code.
    MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_generators_AnnualStorage,lower=lb,par=init_guesses,control=list(dowarn=FALSE))
    
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
  data$net_load_WithAnnualStorageWithCurtailment <- data$net_load_WithAnnualStorageWithCurtailment - MWgeneration
  data[,new_column_number] <- MWgeneration
  rm(MWgeneration_temp,MWgeneration_to_subtract,MWgeneration)
} ## for (i in 1:dim(Frac_MWhDesired_dispatchable)[1]) {
## +++++++++++++++++
## END
## Perform optimization to find the capacity of dispatchables in the actual time series of net_load.
## This for loop solves for dispatchable generation when there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++++



## +++++++++++++++++
## Solving for data$HydroDispatch_AnnualStorage_MW -- BEGIN
## +++++++++++++++++
data$HydroDispatch_AnnualStorage_MW <- 0*data$HydroDispatch_MW
cat("CODE NEEDS TO BE INSERTED: SOLVING FOR `HYDRODISPATCH' WITH ANNUAL ELECTRICITY STORAGE.",sep="\n")
cat("REALLY? IT SEEMS THAT HYDRODISPATCH' WITH ANNUAL ELECTRICITY STORAGE WORKS (6/24/19).",sep="\n")
cat("NOTE THAT data$HydroDispatch_AnnualStorage_MW = data$HydroDispatch_MW, as it is only based on NUCLEAR and water availability.",sep="\n")
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
## Here this assumes there is no electricity storage of any kind.
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

## Third:  Go season by season and find the hours and quantity of dispatch to use up the entire energy budget
## Call function for determining disptach of hydro.
HydroOutputs_winter[,] <- 0  ## reset this variable for when it was solved without assuming Annual Electricity Storage
HydroOutputs_spring[,] <- 0  ## reset this variable for when it was solved without assuming Annual Electricity Storage
HydroOutputs_summer[,] <- 0  ## reset this variable for when it was solved without assuming Annual Electricity Storage
HydroOutputs_fall[,] <- 0    ## reset this variable for when it was solved without assuming Annual Electricity Storage
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
  # init_guesses <- rep(0.5*NewPPcost$MW_existing[which(NewPPcost$X=="HydroDispatch")],5)  ## initial guess at how much to multiply existing PV profile
  # lb = rep(0,5)   ## lower bound on MW capacity for dispatchable hydro
  # ub = rep(NewPPcost$MW_maximum[which(NewPPcost$X=="HydroDispatch")],5)   ## upper bound on MW capacity for dispatchable hydro
  # ## Run optimization to solve each dispatchable capacity except for HydroDispatch which is assumed to remove peaks as much as possible.
  # MWcapacity_multiplier <- Rcgmin(fn=function_dispatchable_hydro_seasonally,lower=lb,upper=ub,par=init_guesses,control=list(dowarn=FALSE))
  
  ## Here, if the user does not want to have as much total hydro MWh, HydroDispatch + HydroNonDispatch,
  ## then to meet the user's requirements, we subtract some amount of hydro generation from that already 
  ## solved as if the total amount of hydro energy (MWh) budget were used.
  user_hydro_budget <- sum(data$Load_MW)*(Frac_MWhDesired_dispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_dispatchable$Technology=="HydroDispatch")] + Frac_MWhDesired_Nondispatchable$Fraction_MWhDesired[which(Frac_MWhDesired_Nondispatchable$Technology=="HydroNonDispatch")])
  HydroNonDispatch_budget <- NewPPcost$MWh_max_WinterJanFeb[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_WinterNovDec[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_Spring[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_Summer[which(NewPPcost$X=="HydroNonDispatch")]+NewPPcost$MWh_max_Fall[which(NewPPcost$X=="HydroNonDispatch")]
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
    for (i in 1:hr_end) { ## 
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
cat("NEED TO ADD MORE HYDRO DISPATCH WITH STORAGE PROGRAMMING HERE ... BUT as of 6/24/19 I DON'T RECALL WHY THIS NOTE. WHAT IS MISSING?",sep="\n")
## +++++++++++++++++
## Solving for data$HydroDispatch_AnnualStorage_MW -- END
## +++++++++++++++++


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
ind.NGCC <- which(NewPPcost$X==c("NGCC"))
ind.NGCT <- which(NewPPcost$X==c("NGCT"))
data$NGCC_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCC] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.NGCC]+NewPPcost$VariableFuelCost_..MWh[ind.NGCC])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
data$NGCT_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCT] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.NGCT]+NewPPcost$VariableFuelCost_..MWh[ind.NGCT])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
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
#rm(PP_MWneeded_temp)


## +++++++++++++++
## calculate actual % of total generation from dispatchable generators.
## Here this assumes there is ANNUAL ELECTRICITY STORAGE.
## +++++++++++++++
cat("Come back to check if ``PP_MWneeded_AnnualStorage$Fraction_MWhActual'' sums to 1.",sep="\n")
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
ind.Coal <- which(NewPPcost$X==c("Coal"))
ind.Nuclear <- which(NewPPcost$X==c("Nuclear"))
ind.UserTech1 <- which(NewPPcost$X==c("UserTech1"))
ind.UserTech2 <- which(NewPPcost$X==c("UserTech2"))
ind.Wind <- which(NewPPcost$X==c("Wind"))
ind.PV <- which(NewPPcost$X==c("PV"))
ind.CSP <- which(NewPPcost$X==c("CSP"))
ind.Biomass <- which(NewPPcost$X==c("Biomass"))
ind.Geothermal <- which(NewPPcost$X==c("Geothermal"))
ind.Petroleum <- which(NewPPcost$X==c("PetroleumCC"))
ind.HydroDispatch <- which(NewPPcost$X==c("HydroDispatch"))
ind.HydroNonDispatch <- which(NewPPcost$X==c("HydroNonDispatch"))
# data$Coal_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Coal] + NewPPcost$FixedOMCost_k..MWyear[ind.Coal] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.Coal]+NewPPcost$VariableFuelCost_..MWh[ind.Coal])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$Nuclear_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Nuclear] + NewPPcost$FixedOMCost_k..MWyear[ind.Nuclear] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.Nuclear]+NewPPcost$VariableFuelCost_..MWh[ind.Nuclear])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$UserTech1_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.UserTech1] + NewPPcost$FixedOMCost_k..MWyear[ind.UserTech1] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.UserTech1]+NewPPcost$VariableFuelCost_..MWh[ind.UserTech1])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$UserTech2_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.UserTech2] + NewPPcost$FixedOMCost_k..MWyear[ind.UserTech2] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.UserTech2]+NewPPcost$VariableFuelCost_..MWh[ind.UserTech2])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$Wind_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Wind] + NewPPcost$FixedOMCost_k..MWyear[ind.Wind] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.Wind]+NewPPcost$VariableFuelCost_..MWh[ind.Wind])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$PV_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.PV] + NewPPcost$FixedOMCost_k..MWyear[ind.PV] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.PV]+NewPPcost$VariableFuelCost_..MWh[ind.PV])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$CSP_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.CSP] + NewPPcost$FixedOMCost_k..MWyear[ind.CSP] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.CSP]+NewPPcost$VariableFuelCost_..MWh[ind.CSP])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$Biomass_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Biomass] + NewPPcost$FixedOMCost_k..MWyear[ind.Biomass] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.Biomass]+NewPPcost$VariableFuelCost_..MWh[ind.Biomass])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$Petroleum_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Petroleum] + NewPPcost$FixedOMCost_k..MWyear[ind.Petroleum] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.Petroleum]+NewPPcost$VariableFuelCost_..MWh[ind.Petroleum])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr
# data$Hydro_Cost_8760 <- NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.HydroDispatch] + NewPPcost$FixedOMCost_k..MWyear[ind.HydroDispatch] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.HydroDispatch]+NewPPcost$VariableFuelCost_..MWh[ind.HydroDispatch])/1000) + 
#                         NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.HydroNonDispatch] + NewPPcost$FixedOMCost_k..MWyear[ind.HydroNonDispatch] + data$Hour.ending*((NewPPcost$VariableOMCost_..MWh[ind.HydroNonDispatch]+NewPPcost$VariableFuelCost_..MWh[ind.HydroNonDispatch])/1000)  ## cost [$1000/MWh] to operate power plant for a given number of hours/yr

## +++++++++++++++++
## Calculate Variable operating cost for each generator type for each hour
## "Cost_VariableOnly_8760$..." = MW generated per hour * $k/MWh in variable costs = 1000s of dollars per each hour
## Here the costs are without any type of electricity storage.
## +++++++++++++++++
Cost_VariableOnly_8760 <- data.frame(rep(0,8760))
names(Cost_VariableOnly_8760)=c("Coal")
Cost_VariableOnly_8760$Coal <- data$Coal_MW*(NewPPcost$VariableOMCost_..MWh[ind.Coal]+NewPPcost$VariableFuelCost_..MWh[ind.Coal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Nuclear <- data$Nuclear_MW*(NewPPcost$VariableOMCost_..MWh[ind.Nuclear]+NewPPcost$VariableFuelCost_..MWh[ind.Nuclear])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$UserTech1 <- data$UserTech1_MW*(NewPPcost$VariableOMCost_..MWh[ind.UserTech1]+NewPPcost$VariableFuelCost_..MWh[ind.UserTech1])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$UserTech2 <- data$UserTech2_MW*(NewPPcost$VariableOMCost_..MWh[ind.UserTech2]+NewPPcost$VariableFuelCost_..MWh[ind.UserTech2])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Wind <- data$Wind_MW_total*(NewPPcost$VariableOMCost_..MWh[ind.Wind]+NewPPcost$VariableFuelCost_..MWh[ind.Wind])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$PV <- data$PV_MW_total*(NewPPcost$VariableOMCost_..MWh[ind.PV]+NewPPcost$VariableFuelCost_..MWh[ind.PV])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$NGCC <- data$NGCC_MW*(NewPPcost$VariableOMCost_..MWh[ind.NGCC]+NewPPcost$VariableFuelCost_..MWh[ind.NGCC])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$NGCT <- data$NGCT_MW*(NewPPcost$VariableOMCost_..MWh[ind.NGCT]+NewPPcost$VariableFuelCost_..MWh[ind.NGCT])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$CSP <- data$CSP_MW_total*(NewPPcost$VariableOMCost_..MWh[ind.CSP]+NewPPcost$VariableFuelCost_..MWh[ind.CSP])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Biomass <- data$Biomass_MW*(NewPPcost$VariableOMCost_..MWh[ind.Biomass]+NewPPcost$VariableFuelCost_..MWh[ind.Biomass])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Petroleum <- data$PetroleumCC_MW*(NewPPcost$VariableOMCost_..MWh[ind.Petroleum]+NewPPcost$VariableFuelCost_..MWh[ind.Petroleum])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760$Hydro <- data$HydroDispatch_MW*(NewPPcost$VariableOMCost_..MWh[ind.HydroDispatch]+NewPPcost$VariableFuelCost_..MWh[ind.HydroDispatch])/1000+
  data$HydroNonDispatch_MW*(NewPPcost$VariableOMCost_..MWh[ind.HydroNonDispatch]+NewPPcost$VariableFuelCost_..MWh[ind.HydroNonDispatch])/1000
Cost_VariableOnly_8760$Geothermal <- data$Geothermal_MW*(NewPPcost$VariableOMCost_..MWh[ind.Geothermal]+NewPPcost$VariableFuelCost_..MWh[ind.Geothermal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed

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
Cost_VariableOnly_8760_AnnualStorage$Coal <- data$Coal_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.Coal]+NewPPcost$VariableFuelCost_..MWh[ind.Coal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Nuclear <- data$Nuclear_MW*(NewPPcost$VariableOMCost_..MWh[ind.Nuclear]+NewPPcost$VariableFuelCost_..MWh[ind.Nuclear])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$UserTech1 <- data$UserTech1_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.UserTech1]+NewPPcost$VariableFuelCost_..MWh[ind.UserTech1])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$UserTech2 <- data$UserTech2_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.UserTech2]+NewPPcost$VariableFuelCost_..MWh[ind.UserTech2])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Wind <- data$Wind_MW_WithAnnualStorage_total*(NewPPcost$VariableOMCost_..MWh[ind.Wind]+NewPPcost$VariableFuelCost_..MWh[ind.Wind])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$PV <- data$PV_MW_WithAnnualStorage_total*(NewPPcost$VariableOMCost_..MWh[ind.PV]+NewPPcost$VariableFuelCost_..MWh[ind.PV])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$NGCC <- data$NGCC_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.NGCC]+NewPPcost$VariableFuelCost_..MWh[ind.NGCC])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$NGCT <- data$NGCT_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.NGCT]+NewPPcost$VariableFuelCost_..MWh[ind.NGCT])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$CSP <- data$CSP_MW_WithAnnualStorage_total*(NewPPcost$VariableOMCost_..MWh[ind.CSP]+NewPPcost$VariableFuelCost_..MWh[ind.CSP])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Biomass <- data$Biomass_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.Biomass]+NewPPcost$VariableFuelCost_..MWh[ind.Biomass])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Petroleum <- data$PetroleumCC_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.Petroleum]+NewPPcost$VariableFuelCost_..MWh[ind.Petroleum])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed
Cost_VariableOnly_8760_AnnualStorage$Hydro <- data$HydroDispatch_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.HydroDispatch]+NewPPcost$VariableFuelCost_..MWh[ind.HydroDispatch])/1000+
  data$HydroNonDispatch_MW*(NewPPcost$VariableOMCost_..MWh[ind.HydroNonDispatch]+NewPPcost$VariableFuelCost_..MWh[ind.HydroNonDispatch])/1000
Cost_VariableOnly_8760_AnnualStorage$Geothermal <- data$Geothermal_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.Geothermal]+NewPPcost$VariableFuelCost_..MWh[ind.Geothermal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed

cat("Still need to calculate variable cost of Annual Storage. Based on charging and discharging?",sep="\n")
Cost_VariableOnly_8760_AnnualStorage$AnnualStorage <- 0*Cost_VariableOnly_8760_AnnualStorage$Geothermal
#Cost_VariableOnly_8760_AnnualStorage$AnnualStorage <- data$Geothermal_AnnualStorage_MW*(NewPPcost$VariableOMCost_..MWh[ind.Geothermal]+NewPPcost$VariableFuelCost_..MWh[ind.Geothermal])/1000  ## cost [$1000/MWh] to operate power plant for each hour of the year based on the MW needed

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

## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## PLOTTING - BEGIN
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
# library(ggplot2)
# start.hour <- 4350 ## minimum is 1, maximum is 8760
# end.hour <- start.hour+24*5   ## minimum is 1, maximum is 8760
# num_PPtech <- dim(PP_MWneeded)[1]-1-dim(PP_MWneeded_temp)[1]  ##  Total number of Power Plant technologies. Subract 1 (for 2 types of hydro plotted as one). Subtract those associated with plotting Annual Storage information
# 
# ## +++++++++++++++
# ## Stacked Area Chart of Variable Cost each Hour ($/MWh) (NO ELECTRICITY STORAGE OF ANY KIND)
# ## +++++++++++++++
# Generator_Cost <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro"),times=(end.hour-start.hour+1)))
# Generator_Cost = factor(Generator_Cost,levels=levels(Generator_Cost)[c(5,12,11,7,6,9,2,1,4,3,10,13,8)]) #reorder
# Hour_Cost <- rep(data$Hour.ending[start.hour:end.hour],each=num_PPtech)
# Cost_Variable <- rep(0,(end.hour-start.hour+1)*num_PPtech)
# for (i in 1:(end.hour-start.hour+1)) {
#   ## Plot method 1
#   Cost_Variable[(num_PPtech*(i-1)+1):(num_PPtech*i)] <- c(Cost_VariableOnly_8760$Nuclear[start.hour+i],Cost_VariableOnly_8760$Wind[start.hour+i],Cost_VariableOnly_8760$PV[start.hour+i],Cost_VariableOnly_8760$CSP[start.hour+i],Cost_VariableOnly_8760$Coal[start.hour+i],Cost_VariableOnly_8760$NGCC[start.hour+i],Cost_VariableOnly_8760$NGCT[start.hour+i],Cost_VariableOnly_8760$UserTech1[start.hour+i],Cost_VariableOnly_8760$UserTech2[start.hour+i],Cost_VariableOnly_8760$Biomass[start.hour+i],Cost_VariableOnly_8760$Geothermal[start.hour+i],Cost_VariableOnly_8760$Petroleum[start.hour+i],Cost_VariableOnly_8760$Hydro[start.hour+i])*1000/data$Load_MW[start.hour+i]
#   ## Plot method 2
#   #### Cost_Variable[(8*(i-1)+1):(8*i)] <- c(Cost_VariableOnly_8760$Nuclear[i],Cost_VariableOnly_8760$Wind[i],Cost_VariableOnly_8760$PV[i],Cost_VariableOnly_8760$Coal[i],Cost_VariableOnly_8760$NGCC[i],Cost_VariableOnly_8760$NGCT[i],Cost_VariableOnly_8760$UserTech1[i],Cost_VariableOnly_8760$UserTech2[i])*1000
#   #### Cost_Variable[(8*(i-1)+1):(8*i)] <- c(Cost_VariableOnly_8760$Nuclear[i]/data$Nuclear_MW[i],Cost_VariableOnly_8760$Wind[i]/data$Wind_MW_total[i],Cost_VariableOnly_8760$PV[i]/data$PV_MW_total[i],Cost_VariableOnly_8760$Coal[i]/data$Coal_MW[i],Cost_VariableOnly_8760$NGCC[i]/data$NGCC_MW[i],Cost_VariableOnly_8760$NGCT[i]/data$NGCT_MW[i],Cost_VariableOnly_8760$UserTech1[i]/data$UserTech1_MW[i],Cost_VariableOnly_8760$UserTech2[i]/data$UserTech2_MW[i])*1000
#   ## Plot method 3
#   #Cost_Variable[(8*(i-1)+1):(8*i)] <- c(Cost_VariableOnly_8760$Nuclear[i],Cost_VariableOnly_8760$Wind[i],Cost_VariableOnly_8760$PV[i],Cost_VariableOnly_8760$Coal[i],Cost_VariableOnly_8760$NGCC[i],Cost_VariableOnly_8760$NGCT[i],Cost_VariableOnly_8760$UserTech1[i],Cost_VariableOnly_8760$UserTech2[i])*1000
#   #Cost_Variable[(8*(i-1)+1):(8*i)] <- Cost_Variable[(8*(i-1)+1):(8*i)]/c(data$Nuclear_MW[i],data$Wind_MW_total[i],data$PV_MW_total[i],data$Coal_MW[i],data$NGCC_MW[i],data$NGCT_MW[i],data$UserTech1_MW[i],data$UserTech2_MW[i])
# }
# Cost_Variable[is.infinite(Cost_Variable)=="TRUE"]=0
# Cost_Variable[is.na(Cost_Variable)=="TRUE"]=0
# data_stacked_area2 <- data.frame(Cost_Variable,Hour_Cost,Generator_Cost)
# p2<-ggplot(data_stacked_area2, aes(x=Hour_Cost, y=Cost_Variable, fill=Generator_Cost)) +     geom_area() +
#   labs(x = "Hour") + labs(y = "Variable Cost ($/MWh)") + guides(fill=guide_legend(title="Generator"))
# print(p2)
# 
# 
# ## +++++++++++++++
# ## Stacked Area Chart of Variable Cost each Hour ($/MWh) (WITH ANNUAL ELECTRICITY STORAGE)
# ## +++++++++++++++
# Generator_Cost2a <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro","Annual Storage"),times=(end.hour-start.hour+1)))
# Generator_Cost2a = factor(Generator_Cost2a,levels=levels(Generator_Cost2a)[c(1,6,13,12,8,7,10,3,2,5,4,11,14,9)]) #reorder
# num_PPtech1a <- dim(PP_MWneeded)[1]-1-dim(PP_MWneeded_temp)[1]+1  ##  Total number of Power Plant technologies. Subract 1 (for 2 types of hydro plotted as one). Subtract those associated with plotting Annual Storage information + 1 to add back storage
# Hour_Cost <- rep(data$Hour.ending[start.hour:end.hour],each=num_PPtech1a)
# Cost_Variable2a <- rep(0,(end.hour-start.hour+1)*num_PPtech1a)
# for (i in 1:(end.hour-start.hour+1)) {
#   ## Plot method 1
#   Cost_Variable2a[(num_PPtech1a*(i-1)+1):(num_PPtech1a*i)] <- c(Cost_VariableOnly_8760_AnnualStorage$Nuclear[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$Wind[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$PV[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$CSP[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$Coal[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$NGCC[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$NGCT[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$UserTech1[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$UserTech2[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$Biomass[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$Geothermal[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$Petroleum[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$Hydro[start.hour+i],Cost_VariableOnly_8760_AnnualStorage$AnnualStorage[start.hour+i])*1000/data$Load_MW[start.hour+i]
# }
# # Generator1a <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro","Annual Storage"),times=(end.hour-start.hour+1)))
# # Generator1a = factor(Generator1a,levels=levels(Generator1a)[c(1,6,13,12,8,7,10,3,2,5,4,11,14,9)]) #reorder
# # data_stacked_area1a <- data.frame(Generation_MW,Hour,Generator1a)
# Cost_Variable2a[is.infinite(Cost_Variable)=="TRUE"]=0
# Cost_Variable2a[is.na(Cost_Variable)=="TRUE"]=0
# data_stacked_area2a <- data.frame(Cost_Variable2a,Hour_Cost,Generator_Cost2a)
# p2a<-ggplot(data_stacked_area2a, aes(x=Hour_Cost, y=Cost_Variable2a, fill=Generator_Cost2a)) +     geom_area() +
#   labs(x = "Hour") + labs(y = "Variable Cost ($/MWh)") + guides(fill=guide_legend(title="Generator"))
# print(p2a)
# 
# 
# ## +++++++++++++++
# ## Stacked Area Chart of MW generation each hour -- NO ANNUAL STORAGE CONSIDERED
# ## +++++++++++++++
# #library(ggplot2)
# Hour <- rep(data$Hour.ending[start.hour:end.hour],each=(num_PPtech))  ## 
# Generation_MW <- rep(0,(end.hour-start.hour+1)*(num_PPtech))
# for (i in 1:(end.hour-start.hour+1)) {
#   # Generation_MW[((num_PPtech)*(i-1)+1):((num_PPtech)*i)] <- c(data$Nuclear_MW[i],(data$Wind_MW_total[i]-curtailed_Wind[i]),(data$PV_MW_total[i]-curtailed_PV[i]),(data$CSP_MW_total[i]-curtailed_CSP[i]),data$Coal_MW[i],data$NGCC_MW[i],data$NGCT_MW[i],data$UserTech1_MW[i],data$UserTech2_MW[i],data$Biomass_MW[i],data$Geothermal_MW[i],data$PetroleumCC_MW[i],data$HydroDispatch_MW[i])
#   Generation_MW[((num_PPtech)*(i-1)+1):((num_PPtech)*i)] <- c(data$Nuclear_MW[start.hour+i],(data$Wind_MW_total[start.hour+i]-curtailed_Wind[start.hour+i]),(data$PV_MW_total[start.hour+i]-curtailed_PV[start.hour+i]),(data$CSP_MW_total[start.hour+i]-curtailed_CSP[start.hour+i]),data$Coal_MW[start.hour+i],data$NGCC_MW[start.hour+i],data$NGCT_MW[start.hour+i],data$UserTech1_MW[start.hour+i],data$UserTech2_MW[start.hour+i],data$Biomass_MW[start.hour+i],data$Geothermal_MW[start.hour+i],data$PetroleumCC_MW[start.hour+i],(data$HydroDispatch_MW[start.hour+i]+data$HydroNonDispatch_MW[start.hour+i]))
# }
# ##Generator1 <- factor(rep(c("Nuclear","Wind","PV","Coal","NGCC","NGCT","UserTech1","UserTech2"),times=(end.hour-start.hour+1)))
# ##Generator1 = factor(Generator1,levels=levels(Generator1)[c(7,6,3,2,1,8,5,4)]) #reorder
# Generator1 <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro"),times=(end.hour-start.hour+1)))
# Generator1 = factor(Generator1,levels=levels(Generator1)[c(5,12,11,7,6,9,2,1,4,3,10,13,8)]) #reorder
# data_stacked_area1 <- data.frame(Generation_MW,Hour,Generator1)
# p1<-ggplot(data_stacked_area1, aes(x=Hour, y=Generation_MW, fill=Generator1)) +     geom_area() +
#   labs(x = "Hour") + labs(y = "Generation (MW)") + guides(fill=guide_legend(title="Generator"))
# print(p1)
# 
# ## +++++++++++++++
# ## Stacked Area Chart of MW generation each hour -- WITH ANNUAL STORAGE CONSIDERED
# ## +++++++++++++++
# #start.hour <- start.hour#3500 ## minimum is 1, maximum is 8760
# #end.hour <- start.hour+24*7   ## minimum is 1, maximum is 8760
# Hour <- rep(data$Hour.ending[start.hour:end.hour],each=(num_PPtech1a))  ## 
# Generation_MW <- rep(0,(end.hour-start.hour+1)*(num_PPtech1a))
# for (i in 1:(end.hour-start.hour+1)) {
#   # Generation_MW[((num_PPtech)*(i-1)+1):((num_PPtech)*i)] <- c(data$Nuclear_MW[i],(data$Wind_MW_total[i]-curtailed_Wind[i]),(data$PV_MW_total[i]-curtailed_PV[i]),(data$CSP_MW_total[i]-curtailed_CSP[i]),data$Coal_MW[i],data$NGCC_MW[i],data$NGCT_MW[i],data$UserTech1_MW[i],data$UserTech2_MW[i],data$Biomass_MW[i],data$Geothermal_MW[i],data$PetroleumCC_MW[i],data$HydroDispatch_MW[i])
#   Generation_MW[((num_PPtech1a)*(i-1)+1):((num_PPtech1a)*i)] <- c(data$Nuclear_MW[start.hour+i],(data$Wind_MW_DirectToGrid_AnnualStorage[start.hour+i]),(data$PV_MW_DirectToGrid_AnnualStorage[start.hour+i]),(data$CSP_MW_DirectToGrid_AnnualStorage[start.hour+i]),data$Coal_AnnualStorage_MW[start.hour+i],data$NGCC_AnnualStorage_MW[start.hour+i],data$NGCT_AnnualStorage_MW[start.hour+i],data$UserTech1_AnnualStorage_MW[start.hour+i],data$UserTech2_AnnualStorage_MW[start.hour+i],data$Biomass_AnnualStorage_MW[start.hour+i],data$Geothermal_AnnualStorage_MW[start.hour+i],data$PetroleumCC_AnnualStorage_MW[start.hour+i],(data$HydroDispatch_AnnualStorage_MW[start.hour+i]+data$HydroNonDispatch_MW[start.hour+i]),data$Dispatched_StoredWindSolar_MW[start.hour+i])
# }
# Generator1a <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro","Annual Storage"),times=(end.hour-start.hour+1)))
# #Generator1a = factor(Generator1,levels=levels(Generator1)[c(5,12,11,7,6,9,2,1,4,3,10,13,8)]) #reorder
# Generator1a = factor(Generator1a,levels=levels(Generator1a)[c(1,6,13,12,8,7,10,3,2,5,4,11,14,9)]) #reorder
# data_stacked_area1a <- data.frame(Generation_MW,Hour,Generator1a)
# p1a<-ggplot(data_stacked_area1a, aes(x=Hour, y=Generation_MW, fill=Generator1a)) +     geom_area() +
#   labs(x = "Hour") + labs(y = "Generation (MW)") + guides(fill=guide_legend(title="Generator"))
# print(p1a)
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
## PLOTTING - END
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++


## +++++++++++++++++
## Calculate start-up (and shut-down?) costs for dispatchable generators.
## What assumption to make for nuclear?????  
## 1) Nuclear is always on so we have to add the cost of keeping it going at some minimum level even when it is not needed?
## 2) It shuts down for part of the year and is on 100% Capacity Factor for a continuous time of the year ...
## 3) ???
## +++++++++++++++++
# Cost_Startup_8760 <- data.frame(rep(0,8760))
# data$Startup_Coal <- rep(0,8760)
# data$Startup_Total <- rep(0,8760)

## +++++++++++++++++
## Calcualte fixed cost in $/MWh for the mix of generators chosen by the user in the final year for which the user has chosen the mix
## "AnnualGridCost_fixed" = MW of capacity * $k/MW-year = 1000s of $ per year to pay fixed costs
## +++++++++++++++++
cat("Still need to assess capital and O&M cost for HydroDispatch & HydroNonDispatch to AnnualGridCost_fixed based on",sep="\n")
cat("if adding new hydro or using existing hydro, since exsiting hydro likely has no annual capital cost.",sep="\n")
AnnualGridCost_fixed = PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Coal")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Coal] + NewPPcost$FixedOMCost_k..MWyear[ind.Coal]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="NGCC")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCC]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="NGCT")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCT]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Nuclear] + NewPPcost$FixedOMCost_k..MWyear[ind.Nuclear]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.PV] + NewPPcost$FixedOMCost_k..MWyear[ind.PV]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Wind] + NewPPcost$FixedOMCost_k..MWyear[ind.Wind]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="UserTech1")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.UserTech1] + NewPPcost$FixedOMCost_k..MWyear[ind.UserTech1]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="UserTech2")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.UserTech2] + NewPPcost$FixedOMCost_k..MWyear[ind.UserTech2]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.CSP] + NewPPcost$FixedOMCost_k..MWyear[ind.CSP]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Biomass")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Biomass] + NewPPcost$FixedOMCost_k..MWyear[ind.Biomass]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PetroleumCC")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Petroleum] + NewPPcost$FixedOMCost_k..MWyear[ind.Petroleum]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="HydroDispatch")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.HydroDispatch] + NewPPcost$FixedOMCost_k..MWyear[ind.HydroDispatch]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="HydroNonDispatch")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.HydroNonDispatch] + NewPPcost$FixedOMCost_k..MWyear[ind.HydroNonDispatch]) +
  PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Geothermal")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Geothermal] + NewPPcost$FixedOMCost_k..MWyear[ind.Geothermal])

## Load PV, CSP, and Wind capacities needed when using Annual Storage technology
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Wind")] <- PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind_DirectToGrid_WithAnnualStorage")]
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="PV")] <- PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV_DirectToGrid_WithAnnualStorage")]
PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="CSP")] <- PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP_DirectToGrid_WithAnnualStorage")]
## Calculate annualized Annual Storage costs (capex, O&M, and other variable); units of $1000/yr
AnnualStorage_AnnualizedCAPEX <- max(StorageCost$AnnualizedCapitalCost_k..MWyear[which(StorageCost$X=="AnnualStorage")]*PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="AnnualStorage_Total")],
                                     StorageCost$AnnualizedCapitalCost_k..MWhyear[which(StorageCost$X=="AnnualStorage")]*PP_MWneeded$MWh_Needed_StorageCapacity[which(PP_MWneeded$Technology=="AnnualStorage_Total")])

AnnualGridCost_fixed_AnnualStorage = PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Coal")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Coal] + NewPPcost$FixedOMCost_k..MWyear[ind.Coal]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCC")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCC] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCC]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCT")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.NGCT] + NewPPcost$FixedOMCost_k..MWyear[ind.NGCT]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Nuclear] + NewPPcost$FixedOMCost_k..MWyear[ind.Nuclear]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="PV")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.PV] + NewPPcost$FixedOMCost_k..MWyear[ind.PV]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Wind")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Wind] + NewPPcost$FixedOMCost_k..MWyear[ind.Wind]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="UserTech1")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.UserTech1] + NewPPcost$FixedOMCost_k..MWyear[ind.UserTech1]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="UserTech2")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.UserTech2] + NewPPcost$FixedOMCost_k..MWyear[ind.UserTech2]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="CSP")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.CSP] + NewPPcost$FixedOMCost_k..MWyear[ind.CSP]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Biomass")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Biomass] + NewPPcost$FixedOMCost_k..MWyear[ind.Biomass]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="PetroleumCC")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Petroleum] + NewPPcost$FixedOMCost_k..MWyear[ind.Petroleum]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="HydroDispatch")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.HydroDispatch] + NewPPcost$FixedOMCost_k..MWyear[ind.HydroDispatch]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="HydroNonDispatch")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.HydroNonDispatch] + NewPPcost$FixedOMCost_k..MWyear[ind.HydroNonDispatch]) +
  PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Geothermal")]*(NewPPcost$AnnualizedCapitalCost_k..MWyear[ind.Geothermal] + NewPPcost$FixedOMCost_k..MWyear[ind.Geothermal])+
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
## PRINT SUMMARY RESULTS
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
cat(paste0("  "),sep="\n")
cat(paste0("NO STORAGE:"),sep="\n")
cat(paste0("Total cost ($/MWh): ",sprintf("%.2f", AnnualGridCost_Total_MWh),", Variable cost ($/MWh): ",sprintf("%.2f", AnnualGridCost_variable_MWh),sep="\n"))
cat(paste0("% Wind: ",sprintf("%.2f", PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Wind")]*100),", % PV: ",sprintf("%.2f", PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="PV")]*100),", % CSP: ",sprintf("%.2f", PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="CSP")]*100)),sep="\n")
cat(paste0("% Coal: ",sprintf("%.2f", PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Coal")]*100),", % Nuke: ",sprintf("%.2f", PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="Nuclear")]*100),", % NG: ",sprintf("%.2f", (PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="NGCC")]*100+PP_MWneeded$Fraction_MWhActual[which(PP_MWneeded$Technology=="NGCT")]*100))),sep="\n")
cat(paste0("MW Wind: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind")]), 0), nsmall=0, big.mark=","),", MW PV: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV")]), 0), nsmall=0, big.mark=","),", MW CSP: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP")]), 0), nsmall=0, big.mark=","),", MW NGCC: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="NGCC")]), 0), nsmall=0, big.mark=","),", MW NGCT: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="NGCT")]), 0), nsmall=0, big.mark=","),", MW Nuclear: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]), 0), nsmall=0, big.mark=","),", MW Coal: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Coal")]), 0), nsmall=0, big.mark=",") ),sep="\n")
cat(paste0("  "),sep="\n")

cat(paste0("WITH ANNUAL STORAGE:"),sep="\n")
#cat(paste0("WITH ANNUAL STORAGE --> MW Wind: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind_DirectToGrid_WithAnnualStorage")]), 0), nsmall=0, big.mark=","),", MW PV: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV_DirectToGrid_WithAnnualStorage")]), 0), nsmall=0, big.mark=","),", MW CSP: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP_DirectToGrid_WithAnnualStorage")]), 0), nsmall=0, big.mark=","),", MW NGCC: ",format(round(as.numeric(PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCC")]), 0), nsmall=0, big.mark=","),", MW NGCT: ",format(round(as.numeric(PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCT")]), 0), nsmall=0, big.mark=","),", MW Nuclear: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]), 0), nsmall=0, big.mark=","),", MW Coal: ",format(round(as.numeric(PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Coal")]), 0), nsmall=0, big.mark=",") ),sep="\n")
cat(paste0("Total cost ($/MWh): ",sprintf("%.2f", AnnualGridCost_Total_MWh_AnnualStorage),", Variable cost ($/MWh): ",sprintf("%.2f", AnnualGridCost_variable_MWh_AnnualStorage),sep="\n"))
cat(paste0("MW Wind: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Wind_DirectToGrid_WithAnnualStorage")]), 0), nsmall=0, big.mark=","),", MW PV: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="PV_DirectToGrid_WithAnnualStorage")]), 0), nsmall=0, big.mark=","),", MW CSP: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="CSP_DirectToGrid_WithAnnualStorage")]), 0), nsmall=0, big.mark=",") ),sep="\n")
cat(paste0("MW NGCC: ",format(round(as.numeric(PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCC")]), 0), nsmall=0, big.mark=","),", MW NGCT: ",format(round(as.numeric(PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="NGCT")]), 0), nsmall=0, big.mark=","),", MW Nuclear: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="Nuclear")]), 0), nsmall=0, big.mark=","),", MW Coal: ",format(round(as.numeric(PP_MWneeded_AnnualStorage$MW_needed[which(PP_MWneeded$Technology=="Coal")]), 0), nsmall=0, big.mark=",") ),sep="\n")
cat(paste0("% Wind: ",sprintf("%.2f", frac.wind_WithAnnualStorage*100),", % PV: ",sprintf("%.2f", frac.PV_WithAnnualStorage*100),", % CSP: ",sprintf("%.2f", frac.CSP_WithAnnualStorage*100)),sep="\n")
cat(paste0("MW Storage Capacity: ",format(round(as.numeric(PP_MWneeded$MW_needed[which(PP_MWneeded$Technology=="AnnualStorage_Total")]), 0), nsmall=0, big.mark=","),", TWh Storage Capacity: ",format(round(as.numeric(PP_MWneeded$MWh_Needed_StorageCapacity[which(PP_MWneeded$Technology=="AnnualStorage_Total")]/1e6), 1), nsmall=0, big.mark=",")),sep="\n")

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
## Use "PP_MWhneeded", but remove information that is not needed and that relates to the "annual storage" solutions
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
#[which(PP_MWneeded_temp$Technology=="PV_StoredToGrid_WithAnnualStorage")]

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
## Use "PP_MWhneeded", but remove information that is not needed and that relates to the "annual storage" solutions
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

cat(paste0("Need to save data for (1) total TWh from each type of generator."),sep="\n")
cat(paste0("Need to save data for (2) total TWh of generation for the region."),sep="\n")

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

## Final output list from this function
output_list <- list("Hourly_MW_NoStorage"=hourly_MWOutput_NoStorage[hrs,],
                        "Hourly_MW_AnnualStorage"=hourly_MWOutput_AnnualStorage[hrs,],
                        "PPdata_NoStorage"=PPdata_NoStorage_solveGen_output,
                        "PPdata_AnnualStorage"=PPdata_AnnualStorage_solveGen_output)
return(output_list)
## ++++++++++++++++++++++++++++
## ++++++++++++++++++++++++++++
} ## END FUNCTION CALL