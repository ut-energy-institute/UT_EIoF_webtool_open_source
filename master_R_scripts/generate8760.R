## This script calls the function that generates hourly LOAD profiles (8760 hours per year) needed as inputs
## into "solveGEN.R".  The output is total hourly generation per the 1 
## Energy Infrastructure of the Future (EIoF) project regions chosen by the user.

## +++++++++++
## These lines are removed when converting to a function
## +++++++++++

generate8760 <- function(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV,LDVmiles_per_region_2050,LDV_miles_per_kwh_2050){

## +++++++++++
## Inputs (that will be) coming from the user to be input as a function
## +++++++++++
UserFraction.HeatPump = percent_ResidentialHeatPump/100 ## This is ultimately the user input
UserFraction.NG = percent_ResidentialNG/100             ## This is ultimately the user input
UserFraction.electricLDV = percent_ElectricLDV/100      ## fraction of light duty vehicle miles diven on electricity

## +++++++++++
## Load the input "Baseline_Fraction_HeatingTypes_byEIoF" that is fraction of homes,
## as simulated in ResStock (using historical, ~ 2009 housing stock vintage) that use
## a certain type of space heating fuel/technology.
## +++++++++++
load("generate8760_data/Baseline_ResStock_Fraction_HeatingTypes_byEIoF.Rdata")
Base.Fraction.HeatPump = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracHeatPump[RegionNumber]
Base.Fraction.NG = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracNG[RegionNumber]

## +++++++++++
## Potential global variables
## +++++++++++
# Assume vehicles are charged with Tranmission & Distribution Losses
T_loss <- 0.02
D_loss <- 0.05  ## This "D_loss" could be lowered for considering losses for charging vehicles since some will occur at central stations at relatively high voltages only part way down to the distribution level (not at resdiential level voltages)
TandD_loss <- T_loss + D_loss
region_names <- c("R1_NW","R2_CA", "R3_MN", "R4_SW", "R5_CE", "R6_TX", "R7_MW", "R8_AL", "R9_MA", "R10_SE", "R11_FL", "R12_NY", "R13_NE")  ## specify the region names as they appear as the column names of the "8760" input profiles

## +++++++++++
## Internal variables
## +++++++++++
region_now <- region_names[RegionNumber]  ## sets the current region name as determined by the user

## +++++++++++
## Load the input baseline and "generic" hourly profiles for electricity demand
## These have been arranged to be in units of "MW" over each hour (so MWh/h = average MW)
## These data are calculated in file "generate8760_2050_baseline.R"
## +++++++++++
load("generate8760_data/Baseline_8760MW_Generation_2050.Rdata")

## +++++++++++
## Load fixed electric vehicle charging profile for each EIoF region
## These have been arranged to be in units of "MW" over each hour (so MWh/h = average MW)
## +++++++++++

load("generate8760_data/SingleEVChargeProfiles_MWh_8760_Input_to_generate8760_and_solveGEN.RData")  ## This .Rdata file is created in file = ""


## EV profiles will modify: EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now]
cat(paste0("Need to add electric ldv miles per kwh variation per region's temperature??"),sep="\n")
Annual_MWh_forEV = UserFraction.electricLDV*LDVmiles_per_region_2050/(LDV_miles_per_kwh_2050*1e3)
EV_profile_multiplier = (1/(1-TandD_loss))*Annual_MWh_forEV/sum(SingleEVChargeProfiles_MWh_8760[names(SingleEVChargeProfiles_MWh_8760) == region_now])
EV_profile_multiplier = as.numeric(EV_profile_multiplier)
EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now] <- EV_profile_multiplier*SingleEVChargeProfiles_MWh_8760[names(SingleEVChargeProfiles_MWh_8760) == region_now]

## +++++++++++
## Take user inputs for the fraction of residential homes heated by (1) heat pumps and (2) natural gas furnaces
## and create the 8760 profile of electricity generation required to meet "Residential Heating" demand.
## +++++++++++
Base.Fraction.other = 1 - Base.Fraction.HeatPump - Base.Fraction.NG  ## Heating that occurs via technologies that are NOT (1) electric heat pumps or (2) NG furnaces
cat(paste0("Base.Fraction.other=",Base.Fraction.other,", Base.Fraction.HeatPump=",Base.Fraction.HeatPump,", Base.Fraction.NG=",Base.Fraction.NG),sep="\n")
cat(paste0("UserFraction.HeatPump=",UserFraction.HeatPump,", UserFraction.NG=",UserFraction.NG),sep="\n")
if ((UserFraction.HeatPump + UserFraction.NG) < (Base.Fraction.HeatPump+Base.Fraction.NG)) {
  stop("Error: the user is not allowed to specify heating methods such that the fracton of homes using (1) natural gas and (2) heat pumps for heating is lower than in the baseline assumed housing stock.")
}
weight.base = (1-UserFraction.HeatPump-UserFraction.NG)/Base.Fraction.other
weight.HeatPump = UserFraction.HeatPump - Base.Fraction.HeatPump*weight.base
weight.NG = UserFraction.NG - Base.Fraction.NG*weight.base

HH_heating_MWh_8760 <- weight.base*EIoF_8760MW_GenerationResidential_Heating_ResStockBase_2050[names(EIoF_8760MW_GenerationResidential_Heating_ResStockBase_2050) == region_now] + 
  weight.NG*EIoF_8760MW_GenerationResidential_Heating_ResStock100pctNG_2050[names(EIoF_8760MW_GenerationResidential_Heating_ResStock100pctNG_2050) == region_now] + 
  weight.HeatPump*EIoF_8760MW_GenerationResidential_Heating_ResStock100pctHeatPump_2050[names(EIoF_8760MW_GenerationResidential_Heating_ResStock100pctHeatPump_2050) == region_now]

## Calculate all other electricity generation required to meet demands that are not Household heating ("NotHH_Heating")
## to have the total electricity generation needed for the region.
ElectricityGeneration_NotHH_Heating_MWh_8760 <- EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now] + 
  EIoF_8760MW_GenerationIndustrial_2050[names(EIoF_8760MW_GenerationIndustrial_2050) == region_now] + 
  EIoF_8760MW_GenerationCommercial_2050[names(EIoF_8760MW_GenerationCommercial_2050) == region_now] + 
  EIoF_8760MW_GenerationResidential_NonHeating_ResStockBase_2050[names(EIoF_8760MW_GenerationResidential_NonHeating_ResStockBase_2050) == region_now]
Total_MWh_8760 <- HH_heating_MWh_8760[,names(HH_heating_MWh_8760) == region_now] + ElectricityGeneration_NotHH_Heating_MWh_8760[,names(ElectricityGeneration_NotHH_Heating_MWh_8760) == region_now]

## Final output list from this function
#output_list <- list("Total_Hourly_MW_8760_CurrentRegion"=Total_MWh_8760)
output_list <- list("Total_Hourly_MW_8760_CurrentRegion"=Total_MWh_8760,
                    "Total_AnnualMWh_LDV_EVs"=sum(EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now]))
return(output_list)

} ## end of function