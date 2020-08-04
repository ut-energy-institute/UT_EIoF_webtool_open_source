## This script calls the function that generates hourly LOAD profiles (8760 hours per year) needed as inputs
## into "solveGEN.R".  The output is total hourly generation per the 1 
## Energy Infrastructure of the Future (EIoF) project regions chosen by the user.

## +++++++++++
## These lines are removed when converting to a function
## +++++++++++
#setwd('/Carey/Research/UT-Projects/EnergyInstitute/Reports/CostOfEnergy/Infrastructure/Contributors/CareyKing/generate8760/')

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
# Baseline_ResStock_Fraction_HeatingTypes_byEIoF = read.csv("Baseline_Fraction_HeatingTypes_byEIoF.csv")  # This holds the fraction of homes heated by electricity, natural gas, and "other" (propane, fuel oil) as run in the "base" run of ResStock using 2016 weather
# save(Baseline_ResStock_Fraction_HeatingTypes_byEIoF,file = "generate8760_data/Baseline_ResStock_Fraction_HeatingTypes_byEIoF.Rdata")
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
## colnames(EIoF_8760Generation_2050) <- c("Hour.ending", "R1_NW","R2_CA", "R3_MN", "R4_SW", "R5_CE", "R6_TX", "R7_MW", "R8_AL", "R9_MA", "R10_SE", "R11_FL", "R12_NY", "R13_NE")  ## rename the columns
## These data are calculated in file "generate8760_2050_baseline.R" or "generate8760_2050_baseline_WORKING.R"
## +++++++++++
# EIoF_8760MW_GenerationTransportation_2050 = read.csv("EIoF_8760GenerationTransportation_2050_BaseResStock.csv") ## Baseline electricity profile to meet Transportation demand + "Transmission & Distribution" losses (assuming no electric light-duty vehicles)
# EIoF_8760MW_GenerationIndustrial_2050 = read.csv("EIoF_8760GenerationIndustrial_2050_BaseResStock.csv")  ## Baseline electricity profile to meet Industrial demand + "Transmission" losses
# EIoF_8760MW_GenerationCommercial_2050 = read.csv("EIoF_8760GenerationCommercial_2050_BaseResStock.csv")  ## Baseline electricity profile to meet Industrial demand + "Transmission & Distribution" losses
# EIoF_8760MW_GenerationResidential_Heating_ResStockBase_2050 = read.csv("EIoF_8760GenerationResidential_Heating_2050_BaseResStock.csv") ## Baseline electricity profile to meet "Residential Heating" demand + "Transmission & Distribution" losses
# EIoF_8760MW_GenerationResidential_NonHeating_ResStockBase_2050 = read.csv("EIoF_8760GenerationResidential_NonHeating_2050_BaseResStock.csv") ## Baseline electricity profile to meet "Residential demand other than heating" + "Transmission & Distribution" losses
# EIoF_8760MW_GenerationResidential_Heating_ResStock100pctNG_2050 = read.csv("EIoF_8760GenerationResidential_Heating_2050_NGResStock.csv") ## Electricity profile to meet "Residential heating demand" + "Transmission & Distribution" losses if using 100% Natural Gas furnaces in homes per ResStock simulation
# EIoF_8760MW_GenerationResidential_Heating_ResStock100pctHeatPump_2050 = read.csv("EIoF_8760GenerationResidential_Heating_2050_HeatPumpResStock.csv") ## Electricity profile to meet "Residential heating demand" + "Transmission & Distribution" losses if using 100% electric Heat Pumps in homes per ResStock simulation
# save(EIoF_8760MW_GenerationTransportation_2050,EIoF_8760MW_GenerationIndustrial_2050,EIoF_8760MW_GenerationCommercial_2050,EIoF_8760MW_GenerationResidential_Heating_ResStockBase_2050,EIoF_8760MW_GenerationResidential_NonHeating_ResStockBase_2050,EIoF_8760MW_GenerationResidential_Heating_ResStock100pctNG_2050,EIoF_8760MW_GenerationResidential_Heating_ResStock100pctHeatPump_2050,file = "generate8760_data/Baseline_8760MW_Generation_2050.Rdata")
load("generate8760_data/Baseline_8760MW_Generation_2050.Rdata")

## +++++++++++
## Load fixed electric vehicle charging profile for each EIoF region
## These have been arranged to be in units of "MW" over each hour (so MWh/h = average MW)
## +++++++++++
## -----------
## TEST EV CHARGE PROFILE #0  - BEGIN
## -----------
#EIoF_8760MW_Demand_LDV_EVCharging_2050 = read.csv("EIoF_8760Demand_LDV_EVCharging_2050.csv")  ## Baseline electricity profile for charging electric light-duty vehicles, does NOT assume "Transmission & Distribution" losses 
#EIoF_8760MW_Demand_LDV_EVCharging_2050 = read.csv("EIoF_8760Demand_LDV_EVCharging_2050_NOT_REAL_YET.csv") ### Baseline electricity profile for charging electric light-duty vehicles, does NOT assume "Transmission & Distribution" losses 
#EIoF_8760MW_GenerationTransportation_2050 <- 0e0*(1/(1-TandD_loss))*EIoF_8760MW_Demand_LDV_EVCharging_2050[names(EIoF_8760MW_Demand_LDV_EVCharging_2050) == region_now]
## -----------
## TEST EV CHARGE PROFILE #0  - END
## -----------
##
## -----------
## TEST EV CHARGE PROFILE #1  - BEGIN
## -----------
# DOE2017_Daily_EVChargeProfile_kW_per_EV = read.csv("C:/Carey/Research/UT-Projects/EnergyInstitute/Reports/CostOfEnergy/Infrastructure/Contributors/CareyKing/generate8760/DOE2017_DailyAveragePVChargeProfile.csv")
# DOE2017_8760_EVChargeProfile_kW_per_EV = rep(DOE2017_Daily_EVChargeProfile_kW_per_EV$kW_charge_per_EV,365)
# SingleEVChargeProfiles_MWh_8760_GEN_NOT_REAL_YET = data.frame(seq(1,8760),DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV,DOE2017_8760_EVChargeProfile_kW_per_EV)
# names(SingleEVChargeProfiles_MWh_8760_GEN_NOT_REAL_YET) = c("Hour",region_names)
# save(SingleEVChargeProfiles_MWh_8760_GEN_NOT_REAL_YET,file="generate8760_data/SingleEVChargeProfiles_MWh_8760_Input_to_solveGEN_NOT_REAL_YET.RData")
# NOTE: The loaded data frame is "SingleEVChargeProfiles_MWh_8760" with one column of 8760 profile for each region AS IF charging 1 PEV "on location" (not accounting for T&D losses)
# load("generate8760_data/SingleEVChargeProfiles_MWh_8760_Input_to_solveGEN_NOT_REAL_YET.RData")  ## This .Rdata file is created in file = "XXX"
# SingleEVChargeProfiles_MWh_8760 <- SingleEVChargeProfiles_MWh_8760_GEN_NOT_REAL_YET
## -----------
## TEST EV CHARGE PROFILE #1  - END
## -----------
## -----------
## FINAL EV CHARGE PROFILE - BEGIN
## -----------
##  CONFIGURE 8760 EV CHARGING PROFILE FROM HOURLY 1-WEEK PROFILE INPUT
# EIoF_Weekly_EVChargeProfile_kW_per_EV = read.csv("generate8760_data/EIoF_HourlyEVChargingProfile_1Week.csv")  ## Read the 1 week profile for EV charging.  The first data point is the local first hour of the day (hour ending = 1) on Monday.  This is defined in file "EIoF_EV_ChargingProfile.xlsx"
# time_zone_shift = c(7,7,6,6,5,5,5,5,4,4,4,4,4)  ## Values represent local time relative to UTC time of the hourly load data (e.g., Pacific Time Zone is "UTC-7" or where 7 am in UTC occurs simultaneous with 12 am midnight in Pacific Time)
# SingleEVChargeProfiles_MWh_8760 <- data.frame(seq(1,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760),rep(0,8760))
# names(SingleEVChargeProfiles_MWh_8760) = c("Hour_ending_UTC",region_names)
# ## NOTE: January 1, 2016 is a Friday. Thus, January 1, 2050 is also assumed to be a Friday. This is important to match the EV charge profiles, which vary from weekend to weekday, to the load profiles.
# EIoF_Thursday_EVChargeProfile_kW_per_EV <- EIoF_Weekly_EVChargeProfile_kW_per_EV[which(EIoF_Weekly_EVChargeProfile_kW_per_EV$Day=="Thursday"),]
# for (i in 1:13) { # cycle through each EIoF region
#   colnumber = which(names(SingleEVChargeProfiles_MWh_8760)==region_names[i])
#   # Set up the initial days before the first Monday
#   SingleEVChargeProfiles_MWh_8760[(1:time_zone_shift[i]),colnumber] = (1/1000)*EIoF_Thursday_EVChargeProfile_kW_per_EV$kW_avg_per_EV[(24-time_zone_shift[i]+1):24]   ## this is the last few hours of local time on a Thursday
#   # Copy the data for Friday, Saturday, and Sunday
#   Friday_start = min(which(EIoF_Weekly_EVChargeProfile_kW_per_EV$Day=="Friday"))
#   SingleEVChargeProfiles_MWh_8760[((time_zone_shift[i]+1):(time_zone_shift[i]+0+24*3)),colnumber] = (1/1000)*EIoF_Weekly_EVChargeProfile_kW_per_EV$kW_avg_per_EV[Friday_start:dim(EIoF_Weekly_EVChargeProfile_kW_per_EV)[1]]
#   # Now copy the entire week, starting from the first hour of Monday, until you can no longer copy the hourly 1-week profile before running past 8760 hours
#   weeks_to_copy = 51
#   first_hour = time_zone_shift[i]+1+24*3
#   last_hour = first_hour+24*7*weeks_to_copy-1
#   SingleEVChargeProfiles_MWh_8760[(first_hour:last_hour),colnumber] = (1/1000)*rep(EIoF_Weekly_EVChargeProfile_kW_per_EV$kW_avg_per_EV,weeks_to_copy)
#   # Now fill in last days of the year, which by definition start with the first hour of Monday due to how we copied the EV charge profile.
#   SingleEVChargeProfiles_MWh_8760[((last_hour+1):8760),colnumber] = (1/1000)*EIoF_Weekly_EVChargeProfile_kW_per_EV$kW_avg_per_EV[1:(8760-last_hour)]
# }
#save(SingleEVChargeProfiles_MWh_8760,file="generate8760_data/SingleEVChargeProfiles_MWh_8760_Input_to_generate8760_and_solveGEN.RData")
#save(SingleEVChargeProfiles_MWh_8760,file="solveGEN_data/SingleEVChargeProfiles_MWh_8760_Input_to_generate8760_and_solveGEN.RData")
load("generate8760_data/SingleEVChargeProfiles_MWh_8760_Input_to_generate8760_and_solveGEN.RData")  ## This .Rdata file is created in file = ""
## -----------
## FINAL EV CHARGE PROFILE - END
## -----------

## EV profiles will modify: EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now]
cat(paste0("Need to add electric ldv miles per kwh variation per region's temperature??"),sep="\n")
#EIA_AEO2019_LDVmiles_2050 = 3472.650879*1e9/10  ## miles driven all light duty vehicles (using liquid fuels) in 2050; value from AEO Ref. 2019 for U.S. is 3472.65087 billion miles
# LDV_miles_per_kwh_2050 = 3.5 ## CONVERTED TO AN INPUT TO THIS FUNCTION ...  assumed miles per kWh average for light duty vehicles miles driven on electricity in 2050. Assume this includes discharge and charge efficiency in battery, so this number represents total kWh into the car.
## LDVmiles_per_region_2050 -- This is now an input to "this function" = "generate8760.R"
Annual_MWh_forEV = UserFraction.electricLDV*LDVmiles_per_region_2050/(LDV_miles_per_kwh_2050*1e3)
#EV_profile_multiplier = (1/(1-TandD_loss))*Annual_MWh_forEV[names(Annual_MWh_forEV) == region_now]/sum(SingleEVChargeProfiles_MWh_8760[names(SingleEVChargeProfiles_MWh_8760) == region_now])
EV_profile_multiplier = (1/(1-TandD_loss))*Annual_MWh_forEV/sum(SingleEVChargeProfiles_MWh_8760[names(SingleEVChargeProfiles_MWh_8760) == region_now])
EV_profile_multiplier = as.numeric(EV_profile_multiplier)
EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now] <- EV_profile_multiplier*SingleEVChargeProfiles_MWh_8760[names(SingleEVChargeProfiles_MWh_8760) == region_now]

## +++++++++++
## Take user inputs for the fraction of residential homes heated by (1) heat pumps and (2) natural gas furnaces
## and create the 8760 profile of electricity generation required to meet "Residential Heating" demand.
## +++++++++++
# Baseline_ResStock_Fraction_HeatingTypes_byEIoF = read.csv("Baseline_Fraction_HeatingTypes_byEIoF.csv")  # This holds the fraction of homes heated by electricity, natural gas, and "other" (propane, fuel oil) as run in the "base" run of ResStock using 2016 weather
# Base.Fraction.HeatPump = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracHeatPump[RegionNumber]
# Base.Fraction.NG = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracNG[RegionNumber]
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


# HH_heating_MWh_8760 <- weight.base*EIoF_8760MW_GenerationResidential_Heating_ResStockBase_2050[names(EIoF_8760MW_GenerationResidential_Heating_ResStockBase_2050) == region_now] + 
#   weight.NG*EIoF_8760MW_GenerationResidential_Heating_ResStock100pctNG_2050[names(EIoF_8760MW_GenerationResidential_Heating_ResStock100pctNG_2050) == region_now] + 
#   weight.HeatPump*EIoF_8760MW_GenerationResidential_Heating_ResStock100pctHeatPump_2050[names(EIoF_8760MW_GenerationResidential_Heating_ResStock100pctHeatPump_2050) == region_now]
# 
# ## Calculate all other electricity generation required to meet demands that are not Household heating ("NotHH_Heating")
# ## to have the total electricity generation needed for the region.
# ElectricityGeneration_NotHH_Heating_MWh_8760 <- EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now] + 
#   EIoF_8760MW_GenerationIndustrial_2050[names(EIoF_8760MW_GenerationIndustrial_2050) == region_now] + 
#   EIoF_8760MW_GenerationCommercial_2050[names(EIoF_8760MW_GenerationCommercial_2050) == region_now] + 
#   EIoF_8760MW_GenerationResidential_NonHeating_ResStockBase_2050[names(EIoF_8760MW_GenerationResidential_NonHeating_ResStockBase_2050) == region_now] + 
# Total_MWh_8760 <- HH_heating_MWh_8760[,names(HH_heating_MWh_8760) == region_now] + ElectricityGeneration_NotHH_Heating_MWh_8760[,names(ElectricityGeneration_NotHH_Heating_MWh_8760) == region_now]


## +++++++++++
## Write total 8760 electricity generation profiles for the user's chosen EIoF region
## This will be an input into "solveGEN.R" that needs an input of the total hourly load for the region.
## +++++++++++
# write.csv(Total_MWh_8760,"generate8760_data/TotalElectricityGeneration_MW8760_CurrentRegion.csv")
# save(Total_MWh_8760, file = "generate8760_data/Generation8760_MWperHour_Input_to_solveGEN.RData")


## Final output list from this function
#output_list <- list("Total_Hourly_MW_8760_CurrentRegion"=Total_MWh_8760)
output_list <- list("Total_Hourly_MW_8760_CurrentRegion"=Total_MWh_8760,
                    "Total_AnnualMWh_LDV_EVs"=sum(EIoF_8760MW_GenerationTransportation_2050[names(EIoF_8760MW_GenerationTransportation_2050) == region_now]))
return(output_list)

} ## end of function