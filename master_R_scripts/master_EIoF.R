## This function will be the One EIoF function to rule them all, One function to source them, 
## One function to bring them all and in the virtual machine bind them.

## This function will act as the main function to be used via Plumber in the TACC VM.
## It waits for a URL API from the UX website and calls all of the pieces of the EIoF
## to generate and return a JSON file to teh website for plotting.

## master_EIoF.R function 
## Carey W. King, PhD
## 2020-03-11

## load up any needed libraries
library(lubridate)
library(jsonlite)
library(readr)
library(jsonlite)   ## for JSON
library(matsbyname) ## Matt Huen's package for matrix operation
library(matsindf)   ## Matt Huen's package for matrix operation
library(Recca)      ## Matt Huen's package for matrix operation
library(magrittr)   ## for pipe operation
library(tibble)
library(dplyr)      ## for data reshape
library(googlesheets4) 

require(Rcgmin)  ## pakcage for gradient based optimization
require(numDeriv)## Allows calculation of numerical derivative of a function within an evaluation function
require(optimr)  ## pakcage for gradient based optimization

## the inputs from the website website API URL GET (I have not idea) call will be:
## 1) Region considered (between 1 and 13, inclusive)
## 2) End-uses that we allow the user to change
##    - home heating energy use (natural gas, electricity, biomass/other)
##    - light-duty vehicle energy use (petroleum vs. electricity)
## 3) Percent electricity generation from primary fuels

## Use this to call the function after it is sourced as a function:
master_EIoF <- function(region_id = 1, coal_percent = 10, PV_percent = 15, CSP_percent = 0, wind_percent = 15, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 50, r_sh_e = 50, r_sh_ng = 50){

  print("Start of master_EIoF.R")
  cat(paste0("Running region number = ",region_id),sep="\n")
  
  #convert inputs to integers
  coal_percent = as.integer(coal_percent)
  PV_percent = as.integer(PV_percent)
  CSP_percent = as.integer(CSP_percent)
  wind_percent = as.integer(wind_percent)
  biomass_percent = as.integer(biomass_percent)
  hydro_percent = as.integer(hydro_percent)
  petroleum_percent = as.integer(petroleum_percent)
  nuclear_percent = as.integer(nuclear_percent)
  geothermal_percent = as.integer(geothermal_percent)
  #although natural gas is technically an input, it is calculated here as the difference of 100 minus the sum of all other technologies
  ng_percent = as.integer(100) - as.integer((coal_percent + PV_percent + CSP_percent + wind_percent + biomass_percent + hydro_percent + petroleum_percent + nuclear_percent + geothermal_percent))
  
  #inputs dataframe
  inputs <- as.data.frame(t(data.frame(
  'region_id' = region_id,
  'coal_percent' = coal_percent,
  'PV_percent' = PV_percent,
  'CSP_percent' = CSP_percent,
  'wind_percent' = wind_percent,
  'biomass_percent' = biomass_percent,
  'hydro_percent' = hydro_percent,
  'petroleum_percent' = petroleum_percent,
  'nuclear_percent' = nuclear_percent,
  'geothermal_percent' = geothermal_percent,
  'ng_percent' = ng_percent,
  'ldv_e' = ldv_e,
  'r_sh_e' = r_sh_e,
  'r_sh_ng' = r_sh_ng
  )))
  names(inputs) <- 'web_inputs'
  
  
  ## Initialize information and confirm inputs are valid
  year = 2016  ## This is the year of input data to use for 8760 hour profiles of load, wind, and PV output. This might or might not ever allow user selection to use a different baseline year of data for load, wind, PV, and weather (e.g., year = 2017).
  RegionNumber = region_id  ## Specify the region number to calculate (there are 13 defined U.S. regions)
  if ( (RegionNumber >= 1) & (RegionNumber <= 13)){
    region_valid = 1 ## all is OK
  } else {
    stop("You have not selected a valid RegionNumber.r.")
  } 
  
  ## source all the below functions
  
  ##############################################################
  

  ########################### BEGIN "generate8760" ###########################
  
  ## Load data associated with 8760 hour profiles of MW generation needed
  load("generate8760_data/Baseline_ResStock_Fraction_HeatingTypes_byEIoF.Rdata")
  Base.Fraction.HeatPump = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracHeatPump[RegionNumber]
  Base.Fraction.NG = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracNG[RegionNumber]
  percent_ResidentialHeatPump = r_sh_e ## The desired % of homes heated by electric heat pumps
  percent_ResidentialNG = r_sh_ng ##100 - r_sh_e ## The desired % of homes heated by NG furnaces
  percent_ElectricLDV = ldv_e  ## The desired % of light-duty vehicles (LDV) miles driven on electricity
  
  ## Load data of EV charging profiles and EV vehicle parameters
  cat("LDVmiles_per_region_2050 needs to be specified before calling 'generate8760.R' *(it is an input).",sep="\n")
  load("generate8760_data/EIOF_LDV_Data_2016_and_2050.Rdata") ## These data are generated in file "Generate_2050_DefaultElectricityGenerationMix_LDVMix_perEIoF.R"
  LDVmiles_current_region_2050 <- EIoF_LDV_Data$VMT_Millions_2050[RegionNumber]*1e6
  LDV_miles_per_kwh_2050 <- as.numeric(EIoF_LDV_Data$mile_per_kwh_EIoF2050[RegionNumber])
  
  source("generate8760.R")
  print("Generating 8760 data (generate8760.R)")
  #generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV)
  #generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV,LDVmiles_per_region_2050[RegionNumber])
  generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV,LDVmiles_current_region_2050,LDV_miles_per_kwh_2050)
  print("Done generating 8760 data")
  
  ## Save "generate8760.R" in form that can be used as inputs to other functions
  Total_Hourly_MW_8760_CurrentRegion = generate8760_output$Total_Hourly_MW_8760_CurrentRegion
  Total_AnnualMWh_LDV_EVs = generate8760_output$Total_AnnualMWh_LDV_EVs
  
  ########################### END "generate8760" ###########################
  
  
  ########################### BEGIN "solveGEN" ########################### 
  ## call Carey's solveGEN code to get needed power plant capacities
  ## inputs: 8760 hour profiles of electricity (all uses) demand, percent electricity generation from primary fuels, region
  ## outputs: capacities needed of each type of power plant to realize percent electricity generation from primary fuels, 8760 generation curves from each type of power plant 
  ## Note: The fraction of electric power from natural gas combined cycle and natural gas combustion turbines
  ## is assumed to be any generation not otherwise specified in the inputs. Thus, there is no input of 
  ## "natural_gas_percent" to solveGEN.r.
  
  #Notes:
  # code has been edited to add EV charging profile to Carey's code
  
  source("solveGEN.R")


  ## Call the function
  print("Solving for power plant generation capacities (solveGEN.R)")
  solveGEN_output <- solveGEN(RegionNumber = region_id, year = year, coal_percent = coal_percent, PV_percent = PV_percent, CSP_percent = CSP_percent, wind_percent = wind_percent, nuclear_percent = nuclear_percent, hydro_percent = hydro_percent, biomass_percent = biomass_percent, geothermal_percent = geothermal_percent, petroleum_percent = petroleum_percent,Total_Hourly_MW_8760_CurrentRegion)
  print("solveGEN.R is finished.")
  
  ## Specify certain data from solveGEN_outoput for use by "generate_FinalUVY_2050.R"
  PPdata_NoStorage <- solveGEN_output$PPdata_NoStorage
  PPdata_AnnualStorage <- solveGEN_output$PPdata_AnnualStorage
  Hourly_MW_NoStorage <- solveGEN_output$Hourly_MW_NoStorage
  Hourly_MW_AnnualStorage <- solveGEN_output$Hourly_MW_AnnualStorage

  ########################### END "solveGEN" ###########################
  
  
  
  ########################### BEGIN CAREY "generate_FinalUVY_2050.R" ###########################
  ## "generate_FinalUVY_2050.R" is the function that takes the results from the user's inputs and adds
  ## them to the baseline U and V matrices that are then required as inputs to Sankey diagram code.
  ## inputs: RegionNumber, r_sh_e = percent of heating desired from electric heat pumps, r_sh_ng = percent of heating desired from natural gas
  ## outputs: U and V matrices for both (1) "AnnualStorage" and (2) "NoStorage" solutions. They should be exactly the same.
  ## output_list <- list("U_NoStorage_2050_CurrentRegion"=U_NoStorage,
  ##                    "V_NoStorage_2050_CurrentRegion"=V_NoStorage,
  ##                    "U_AnnualStorage_2050_CurrentRegion"=U_AnnualStorage,
  ##                    "V_AnnualStorage_2050_CurrentRegion"=V_AnnualStorage)
  
  source("generate_FinalUVY_2050.R")
  generate_FinalUVY_2050_output <- generate_FinalUVY_2050(RegionNumber = region_id, percent_ResidentialHeatPump = percent_ResidentialHeatPump, percent_ResidentialNG = percent_ResidentialNG, Hourly_MW_NoStorage,Hourly_MW_AnnualStorage,PPdata_NoStorage,PPdata_AnnualStorage,percent_ElectricLDV,LDVmiles_current_region_2050,Total_AnnualMWh_LDV_EVs)
  save(generate_FinalUVY_2050_output,file="generate_FinalUVY_2050_output.Rdata")  ## I can just put "Total_Hourly_MW_8760_CurrentRegion" as an input into "solveGEN.R"
  
  ########################### END CAREY "generate_FinalUVY_2050.R"  ###########################



  ########################### BEGIN  Sankey Code ###########################
  
  ## call Jianwei's code (Sankey.R) to edit the Sankey data based on user input
  ## inputs: end use changes, percent electricity generation from primary fuels, region
  ## outputs : JSON file with new values used to create Sankey
  
  source('Sankey_Function.R')
  print('Calculating energy flows by fuel type, sector and end use (Sankey_Function.R)')
  
  ## Inputs to sankey code are as XXXXXXX ... 
  ## All of the inputs to the Sankey code are treated as "percentages of the total for flows through some node" (with values 0 - 100).
  ## Thus certain items need to sum to 100 (100 percent) as follows:
  ## 100 = p_solar + p_nuclear + p_hydro + p_wind + p_geo + p_ng + p_bio + p_coal + p_ng + p_petrol
  cat(paste0("Need to confirm how to incorporate Residential and Commerce 'rejected_energy' and 'XX_Other'."),sep="\n")
  cat(paste0("Maybe just get rid of categories of Electricity and NG consumption and just sum the energy?"),sep="\n")
  
  #r_sh_e = r_sh_e
  r_sh_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Resident_SpaceHeating_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Resident_SpaceHeating_Elec"])
  r_sh_e = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["Electricity_Flow","Resident_SpaceHeating_Elec"]/r_sh_totalenergy
  r_sh_ng = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["NaturalGas_Flow","Resident_SpaceHeating_NG"]/r_sh_totalenergy
  r_sh_other = 100 - r_sh_e - r_sh_ng  ## What can I do with this number? Currently 'r_sh_other' is not used as direct input.  Other fuels include (1) petroleum (propane and fuel oil), (2) electricity from other than heat pumps, and (3) biomass
  
  #r_wh_e = 100
  r_wh_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Resident_WaterHeating_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Resident_WaterHeating_Elec"])
  r_wh_e = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["Electricity_Flow","Resident_WaterHeating_Elec"]/r_wh_totalenergy
  r_wh_ng = 100 - r_wh_e
  
  #r_ck_e = 100
  r_ck_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Resident_Cooking_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Resident_Cooking_Elec"])
  r_ck_e = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["Electricity_Flow","Resident_Cooking_Elec"]/r_ck_totalenergy
  r_ck_ng = 100 - r_ck_e
  
  #c_sh_e = 100
  c_sh_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Commerce_SpaceHeating_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Commerce_SpaceHeating_Elec"])
  c_sh_e = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["Electricity_Flow","Commerce_SpaceHeating_Elec"]/c_sh_totalenergy
  c_sh_ng = 100 - c_sh_e
  
  #c_wh_e = 100
  c_wh_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Commerce_WaterHeating_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Commerce_WaterHeating_Elec"])
  c_wh_e = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["Electricity_Flow","Commerce_WaterHeating_Elec"]/c_wh_totalenergy
  c_wh_ng = 100 - c_wh_e
  
  #c_ck_e = 100
  c_ck_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Commerce_Cooking_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Commerce_Cooking_Elec"])
  c_ck_e = 100*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion["Electricity_Flow","Commerce_Cooking_Elec"]/c_ck_totalenergy
  c_ck_ng = 100 - c_ck_e
  
  ## EIA Annual Energy Outlook 2019, calculations in "Key Indicators" for "Travel Indicators", Table 7-AEO2019_ref2019-d111618a, reference case scenario
  cat(paste0("Need to add LDV miles driven per region (and per month or season in charging profile?)."),sep="\n")
  cat(paste0("Need to add electric ldv miles per kwh variation per region and/or temperature ... IF ADDING THIS, DO IT IN GENERATE8760."),sep="\n")
  ldv_totalquads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Petrol"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Elec"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Ethanol"])
  ldv_petrol_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Petrol"])
  ldv_elec_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Elec"])
  ldv_biofuel_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Ethanol"])
  percent_ldv_elec_quads = 100*ldv_elec_quads/ldv_totalquads
  percent_ldv_petrol_quads = 100*ldv_petrol_quads/ldv_totalquads
  percent_ldv_biofuel_quads = 100 - percent_ldv_elec_quads - percent_ldv_petrol_quads ## Calculating this way ensures these three percentages add exactly to 100  (and not off by some differnece like 1e-14)

  ## Other (non-LDV transportation)
  trans_other_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Petrol"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Other"])
  if (trans_other_totalenergy>0){
    trans_other_petrol = 100*sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Petrol"])/trans_other_totalenergy
    trans_other_ng = 100*sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_NG"])/trans_other_totalenergy
    trans_other_other = max(0,100 - trans_other_petrol - trans_other_ng)  ## sometimes "100 - trans_other_petrol - trans_other_ng" expression can result in small negative values within double precision, so need to make 0 if so
  } else {
    trans_other_petrol = 100
    trans_other_ng = 0
    trans_other_other = 0
  }
  
  solar_percent = PV_percent + CSP_percent
  
  ## Load blank Y matrix template for U, V, and Y Sankey calculations
  load("generate_FinalUVY_2050_data/UVY_templates.Rdata")
  
  ##function to check if percentages of LDV fuels equals exactly 100 (percent), with no double precision error/discrepancy
  check_ldv_percent <- function(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads) {
    cat(paste0("[Sankey_Function.R]: Check A#N if zero: sum(percent_ldv percentages of fuels) - 100 = ", (sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100)),sep="\n")
    percent_ldv_biofuel_quads <- percent_ldv_biofuel_quads - (sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100)
    cat(paste0("[Sankey_Function.R]: Check B#N if zero: sum(percent_ldv percentages of fuels) - 100 = ", (sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100)),sep="\n")
    return(percent_ldv_biofuel_quads)
  }
  
  ## Call to solve sankey with the "NonStorage" results of "solveGEN" and "generate_FinalUVY_2050"
  if (percent_ldv_elec_quads < 1) {
    percent_ldv_elec_quads = 0.01
    percent_ldv_petrol_quads = percent_ldv_petrol_quads
    percent_ldv_biofuel_quads = 100 - percent_ldv_elec_quads - percent_ldv_petrol_quads
  }  ## For purposes of plotting the Sankey Diagram consistently, if "ldv_e = 0" from user, we need some > 0 value for electricity to LDVs so that the "Transportation" node is displayed in alignment with the other "end use" sectors  
  ## Cycle through correcting if sum of LDV percentages is off by a factor near double precision, and correct it
  cat(paste0("[master_EIof.R]: Before Sankey_Function. R, check #0 if zero: sum(percent_ldv percentages of fuels) - 100 = ", (sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100)),sep="\n")
  if ((sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100) != 0) { ## Check #2
    check_ldv_percent_output <- check_ldv_percent(frac_ng,k_prime)
    check_ldv_percent <- check_ldv_percent_output[1]
    if ((sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100) != 0) { ## Check #3
      check_ldv_percent_output <- check_ldv_percent(frac_ng,k_prime)
      check_ldv_percent <- check_ldv_percent_output[1]
      if ((sum(percent_ldv_elec_quads,percent_ldv_petrol_quads,percent_ldv_biofuel_quads)-100) != 0) { ## Check #4
        check_ldv_percent_output <- check_ldv_percent(frac_ng,k_prime)
        check_ldv_percent<- check_ldv_percent_output[1]
      } } }

  ## Call Sankey function with "AnnualStorage" results of "solveGEN" and as input into "sankey_json"
  sankey_input_pct_AnnualStorage_Solar <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="PV")]+PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="CSP")])
  sankey_input_pct_AnnualStorage_Wind <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Wind")])
  sankey_input_pct_AnnualStorage_Biomass <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Biomass")])
  sankey_input_pct_AnnualStorage_Coal <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Coal")])
  sankey_input_pct_AnnualStorage_Nuclear <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Nuclear")])
  sankey_input_pct_AnnualStorage_Petroleum <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="PetroleumCC")])
  sankey_input_pct_AnnualStorage_Geothermal <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Geothermal")])
  sankey_input_pct_AnnualStorage_Hydro <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="HydroDispatch")])
  sankey_input_pct_AnnualStorage_NG <- 100 - sum(sankey_input_pct_AnnualStorage_Solar,sankey_input_pct_AnnualStorage_Wind,sankey_input_pct_AnnualStorage_Biomass,sankey_input_pct_AnnualStorage_Coal,sankey_input_pct_AnnualStorage_Nuclear,sankey_input_pct_AnnualStorage_Petroleum,sankey_input_pct_AnnualStorage_Geothermal,sankey_input_pct_AnnualStorage_Hydro)
  print("Starting sankey_json.R.")
  sankey_json_out <- sankey_json(region_id = region_id, p_solar = sankey_input_pct_AnnualStorage_Solar, p_nuclear = sankey_input_pct_AnnualStorage_Nuclear, p_hydro = sankey_input_pct_AnnualStorage_Hydro, p_wind = sankey_input_pct_AnnualStorage_Wind, p_geo = sankey_input_pct_AnnualStorage_Geothermal, p_ng = sankey_input_pct_AnnualStorage_NG, p_coal = sankey_input_pct_AnnualStorage_Coal, p_bio = sankey_input_pct_AnnualStorage_Biomass, p_petrol = sankey_input_pct_AnnualStorage_Petroleum, r_sh_e = r_sh_e, r_sh_ng = r_sh_ng, r_wh_e = r_wh_e, r_wh_ng = r_wh_ng, r_ck_e = r_ck_e, r_ck_ng = r_ck_ng, c_sh_e = c_sh_e, c_sh_ng = c_sh_ng, c_wh_e = c_wh_e, c_wh_ng = c_wh_ng, c_ck_e = c_ck_e, c_ck_ng = c_ck_ng, ldv_elec = percent_ldv_elec_quads, ldv_petrol = percent_ldv_petrol_quads, ldv_ethanol = percent_ldv_biofuel_quads, trans_other_petrol = trans_other_petrol, trans_other_ng = trans_other_ng, trans_other_other = trans_other_other,generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion,generate_FinalUVY_2050_output$V_AnnualStorage_2050_CurrentRegion,Y_template)
  print("sankey_json.R is finished.")
  
  ########################### END Sankey Code ###########################
  

  ########################### BEGIN GOOGLESHEET ACCESS ###########################
  print("Starting Google Sheets process.")
  
  source('EIoF_gs4_function.R')

  ## Create the "target" order of data to input into the Google Sheet
  target <- c("Coal", "Nuclear", "NGCC", "NGCT", "HydroDispatch", "PV", "Wind", "Geothermal", "Biomass", "Other", "PetroleumCC", "AnnualStorage_Total","CSP")
  load("solveGen_data/Tranfer_RegionFromTo.Rdata") ## Load data with the (1) miles of transmission to connect resources, per region and (2) % of electricity from resource coming from which region to the current RegionNumber
  
  ## +++++++++++
  ## CALL GOOGLE SHEET FOR "NO STORAGE" OPTION
  ## +++++++++++
  ## massage outputs from solveGEN to go into the googlesheets code for "No Storage" solvGEN outputs
  GS_inputs_NoStorage <- solveGEN_output$PPdata_NoStorage[c('Technology', 'MW_needed', 'TWhGeneration')]
  GS_inputs_NoStorage <- GS_inputs_NoStorage[-which(GS_inputs_NoStorage$Technology=="LandTotal"),]  ## remove the row "LandTotal" since that is not to be written into the Google Sheet
  ## FIRST: Power Plant (1) MW of capacity and (2) TWh of generation
  GS_inputs_NoStorage <- GS_inputs_NoStorage[match(target, GS_inputs_NoStorage$Technology),]
  GS_inputs_NoStorage$Technology <- target
  GS_inputs_NoStorage[is.na(GS_inputs_NoStorage)] <- 0
  ## SECOND: Residential NG demand for space heating and all other (non space heating) since that has been affected by user's choice of fuels for Residential Heating 
  NG_ResHeating_NoStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_SpaceHeating_NG']
  NG_NonResHeating_NoStorage <- (1/10^15)*(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_WaterHeating_NG'] + generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_Cooking_NG'] + generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_Other'])
  ## THRID: Petroleum consumption (quads) for LDv travel (since that has been affected by user's choice of fuels for LDVs)
  Petro_LDV_NoStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['Petroleum_Flow','Transport_LDV_Petrol']
  ## FOURTH: Miles of transmission lines needed specifically to connect wind and CSP plants
  Miles_Transmission_wind <- sum(Tranfer_RegionFromTo_Wind[,RegionNumber]*MilesTransmission_RegionFromTo_Wind[,RegionNumber])  ## Average Miles of transmission to connect wind farms to load centers
  Miles_Transmission_CSP <- sum(Tranfer_RegionFromTo_CSP[,RegionNumber]*MilesTransmission_RegionFromTo_CSP[,RegionNumber])  ## Average Miles of transmission to connect CSP farms to load centers
  ## FIFTH: Peak Geneation (MW) reached to meet user's scenario
  Peak_MW <- max(generate8760_output$Total_Hourly_MW_8760_CurrentRegion)
  ## SIXTH: arrange data input of price for biomass for biomass power plants
  biomass_price <- solveGEN_output$BiomassPrice$biomass_2017USD_per_MMBtu_NoStorage
  ## SEVENTH: arrange data input of CAPEX and FOM costs for geothermal power plants (for 2020 and 2050)
  geothermal_gs_inputs <- solveGEN_output$GeothermalCosts$NoStorage
  ## FINAL: Create data frame of inputs into the "gs4_function" (function that calls the google sheet)
  OtherInputs_Names <- c("Miles_Trans_wind","Miles_Trans_CSP","NG_ResNonHeating","NG_ResHeating","Peak_MW","Petroleum_for_LDVs","BiomassPrice_2017USD_per_MMBtu",rownames(solveGEN_output$GeothermalCosts))
  OtherInputs_data <- c(Miles_Transmission_CSP,Miles_Transmission_wind,NG_NonResHeating_NoStorage,NG_ResHeating_NoStorage,Peak_MW,Petro_LDV_NoStorage,biomass_price,geothermal_gs_inputs)
  OtherInputs_zeros <- rep(0,length(OtherInputs_data))
  OtherInputs_NoStorage <- data.frame(OtherInputs_Names,OtherInputs_data,OtherInputs_zeros)
  colnames(OtherInputs_NoStorage) <- colnames(GS_inputs_NoStorage)
  GS_inputs_NoStorage <- rbind(GS_inputs_NoStorage,OtherInputs_NoStorage)
  ## Finally call Google Sheet (only NoStorage case)
  # gg_out_NoStorage <- EIoF_gs4_function(GS_inputs = GS_inputs_NoStorage)
  # gg_out_NoStorage <- as.data.frame(gg_out_NoStorage[,-1])
  # print("End of calling Google Sheet for No Storage case.")
  
  ## +++++++++++
  ## CALL GOOGLE SHEET FOR "WITH ANNUAL STORAGE" OPTION
  ## +++++++++++
  ## massage outputs from solveGEN to go into the googlesheets code for "Annual Storage" solvGEN outputs
  GS_inputs_AnnualStorage <- solveGEN_output$PPdata_AnnualStorage[c('Technology', 'MW_needed', 'TWhGeneration')]
  GS_inputs_AnnualStorage <- GS_inputs_AnnualStorage[-which(GS_inputs_AnnualStorage$Technology=="LandTotal"),]  ## remove the row "LandTotal" since that is not to be written into the Google Sheet
  ## FIRST: Power Plant (1) MW of capacity and (2) TWh of generation
  GS_inputs_AnnualStorage <- GS_inputs_AnnualStorage[match(target, GS_inputs_AnnualStorage$Technology),]
  GS_inputs_AnnualStorage$Technology <- target
  GS_inputs_AnnualStorage[is.na(GS_inputs_AnnualStorage)] <- 0
  ## SECOND: Residential NG demand for space heating and all other (non space heating) since that has been affected by user's choice of fuels for Residential Heating 
  NG_ResHeating_AnnualStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_SpaceHeating_NG']
  NG_NonResHeating_AnnualStorage <- (1/10^15)*(generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_WaterHeating_NG'] + generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_Cooking_NG'] + generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_Other'])
  ## THRID: Petroleum consumption (quads) for LDv travel (since that has been affected by user's choice of fuels for LDVs)
  Petro_LDV_AnnualStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion['Petroleum_Flow','Transport_LDV_Petrol']
  ## FOURTH: Miles of transmission lines needed specifically to connect wind and CSP plants
  Miles_Transmission_wind <- sum(Tranfer_RegionFromTo_Wind[,RegionNumber]*MilesTransmission_RegionFromTo_Wind[,RegionNumber])  ## Average Miles of transmission to connect wind farms to load centers
  Miles_Transmission_CSP <- sum(Tranfer_RegionFromTo_CSP[,RegionNumber]*MilesTransmission_RegionFromTo_CSP[,RegionNumber])  ## Average Miles of transmission to connect CSP farms to load centers
  ## FIFTH: Peak Geneation (MW) reached to meet user's scenario
  Peak_MW <- max(generate8760_output$Total_Hourly_MW_8760_CurrentRegion)
  ## SIXTH: arrange data input of price for biomass for biomass power plants
  biomass_price <- solveGEN_output$BiomassPrice$biomass_2017USD_per_MMBtu_AnnualStorage
  ## SEVENTH: arrange data input of CAPEX and FOM costs for geothermal power plants (for 2020 and 2050)
  geothermal_gs_inputs <- solveGEN_output$GeothermalCosts$AnnualStorage
  ## FINAL: Create data frame of inputs into the "gs4_function" (function that calls the google sheet)
  OtherInputs_Names <- c("Miles_Trans_wind","Miles_Trans_CSP","NG_ResNonHeating","NG_ResHeating","Peak_MW","Petroleum_for_LDVs","BiomassPrice_2017USD_per_MMBtu",rownames(solveGEN_output$GeothermalCosts))
  OtherInputs_data <- c(Miles_Transmission_CSP,Miles_Transmission_wind,NG_NonResHeating_AnnualStorage,NG_ResHeating_AnnualStorage,Peak_MW,Petro_LDV_AnnualStorage,biomass_price,geothermal_gs_inputs)
  OtherInputs_zeros <- rep(0,length(OtherInputs_data))
  OtherInputs_AnnualStorage <- data.frame(OtherInputs_Names,OtherInputs_data,OtherInputs_zeros)
  colnames(OtherInputs_AnnualStorage) <- colnames(GS_inputs_AnnualStorage)
  GS_inputs_AnnualStorage <- rbind(GS_inputs_AnnualStorage,OtherInputs_AnnualStorage)
  ## Finally call Google Sheet (only AnnualStorage case)
  # gg_out_AnnualStorage <- EIoF_gs4_function(GS_inputs = GS_inputs_AnnualStorage)
  # gg_out_AnnualStorage <- as.data.frame(gg_out_AnnualStorage[,-1])  ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet
  # print("End of calling Google Sheet for Annual Storage case.")
  
  
  ## Call Google Sheet (with inputs for BTOH AnnualStorage & NoStorage cases)
  gg_out_all <- EIoF_gs4_function(RegionNumber,GS_inputs_AnnualStorage = GS_inputs_AnnualStorage,GS_inputs_NoStorage = GS_inputs_NoStorage)
  gg_out_AnnualStorage <- gg_out_all[[1]]
  gg_out_NoStorage <- gg_out_all[[2]]
  gg_out_NoStorage <- as.data.frame(gg_out_NoStorage[,-1])  ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet
  gg_out_AnnualStorage <- as.data.frame(gg_out_AnnualStorage[,-1]) ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet
  
  ## Rearrange Google Sheet output data
  gg_out2 <- lapply(split(gg_out_AnnualStorage,gg_out_AnnualStorage$type,drop = TRUE), function(x) split(x, x[['value']], drop = TRUE))
  gg_out3 <- lapply(split(gg_out_NoStorage,     gg_out_NoStorage$type,     drop = TRUE), function(x) split(x, x[['value']], drop = TRUE))
  print("End of calling Google Sheet.")


  ########################### END GOOGLESHEET ACCESS ###########################
  
  ########################### START CALCULATE SPECIFIC 2050 ELECTRICITY COST SUMMARY VALUES FOR WEBSITE DISPLAY ###########################
  ## No Storage: Calculate cents/kWh - CAPEX, OPEX, and TOTAL
  ## Make this equal to last 3 yrs of CAPEX and OPEX divided by last 3 yrs of electricity generation
  search_type1 = gg_out_NoStorage$type
  search_value1 = gg_out_NoStorage$value
  ## which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))  ## THis finds the value for the row of gg_out_NoStorage that has both the targeted phrase in column $type (e.g., "capex") and targeted phrase in column $value (e.g., "TandD")
  capex_3yr_TandD_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="TandD"))]] + gg_out_NoStorage$`2049`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="TandD"))]] + gg_out_NoStorage$`2050`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="TandD"))]]
  ## Note "capex_3yr_all_pp_NoStorage" must go from 2047-2049 since the Google Sheet costs force (by definition) 2050 capital spending for power plants to be zero (0)
  capex_3yr_all_pp_NoStorage <- gg_out_NoStorage$`2047`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))]] + gg_out_NoStorage$`2048`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))]] + gg_out_NoStorage$`2049`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))]]
  capex_3yr_AnnualNuke_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="annual_nuclear"))]] + gg_out_NoStorage$`2049`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="annual_nuclear"))]] + gg_out_NoStorage$`2050`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="annual_nuclear"))]]
  opex_3yr_TandD_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="opex")[which(which(search_type1=="opex") %in% which(search_value1=="TandD"))]] + gg_out_NoStorage$`2049`[which(search_type1=="opex")[which(which(search_type1=="opex") %in% which(search_value1=="TandD"))]] + gg_out_NoStorage$`2050`[which(search_type1=="opex")[which(which(search_type1=="opex") %in% which(search_value1=="TandD"))]] 
  opex_3yr_all_pp_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="opex_wfuel")[which(which(search_type1=="opex_wfuel") %in% which(search_value1=="all_pp"))]] + gg_out_NoStorage$`2049`[which(search_type1=="opex_wfuel")[which(which(search_type1=="opex_wfuel") %in% which(search_value1=="all_pp"))]] + gg_out_NoStorage$`2050`[which(search_type1=="opex_wfuel")[which(which(search_type1=="opex_wfuel") %in% which(search_value1=="all_pp"))]] 
  deprec_3yr_TandD_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="TandD"))]] + gg_out_NoStorage$`2049`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="TandD"))]] + gg_out_NoStorage$`2050`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="TandD"))]]
  deprec_3yr_all_pp_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="all_pp"))]] + gg_out_NoStorage$`2049`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="all_pp"))]] + gg_out_NoStorage$`2050`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="all_pp"))]]
  TWh_3yr_NoStorage <- gg_out_NoStorage$`2048`[which(search_type1=="generation")[which(which(search_type1=="generation") %in% which(search_value1=="total"))]] + gg_out_NoStorage$`2049`[which(search_type1=="generation")[which(which(search_type1=="generation") %in% which(search_value1=="total"))]] + gg_out_NoStorage$`2050`[which(search_type1=="generation")[which(which(search_type1=="generation") %in% which(search_value1=="total"))]] 
  cents_per_kwh_NoStorage_capex_2050 = 100*(capex_3yr_TandD_NoStorage + capex_3yr_all_pp_NoStorage)/TWh_3yr_NoStorage
  cents_per_kwh_NoStorage_opex_2050 = 100*(opex_3yr_TandD_NoStorage + opex_3yr_all_pp_NoStorage)/TWh_3yr_NoStorage
  ## "cents_per_kwh_NoStorage_deprec_interest_2050" includes the annual capital expenditures for existing nuclear power plants
  cents_per_kwh_NoStorage_deprec_interest_2050 = 100*(capex_3yr_AnnualNuke_NoStorage + deprec_3yr_TandD_NoStorage + deprec_3yr_all_pp_NoStorage)/TWh_3yr_NoStorage
  cents_per_kwh_NoStorage_total_2050 = cents_per_kwh_NoStorage_opex_2050 + cents_per_kwh_NoStorage_deprec_interest_2050
  load("Population_and_Electricity_Customers.rdata")
  load("FERC1_ResidentialRevenue_Data.Rdata") ## FERC1_ResidentialRevenue_Data.rdata is generated and saved in file "FERC1_ComapreIOURevenue_AllEIoFRegions.R". From this .rdata file we use the fraction of total IOU revenue from Residential customers
  EIoF_Regions_Population_Projections <- EIoF_Regions_Population_Projections[-which(is.na(EIoF_Regions_Population_Projections$EIoF.Region)=="TRUE"),]
  ElectricityCustomer_Population_Ratios <- ElectricityCustomer_Population_Ratios[-which(is.na(ElectricityCustomer_Population_Ratios$EIoF.Region)=="TRUE"),]
  regions <- c('NW','CA','MN','SW','CE','TX','MW','AL','MA','SE','FL','NY','NE')  ## EIoF regions
  EIoF_Regions_Population_Projections <- EIoF_Regions_Population_Projections[match(regions, EIoF_Regions_Population_Projections$EIoF.Region),]
  ElectricityCustomer_Population_Ratios <- ElectricityCustomer_Population_Ratios[match(regions, ElectricityCustomer_Population_Ratios$EIoF.Region),]
  ResidentialRevenueFraction <- mean(Residential_Revenue_PctOfTotal[1:8,(RegionNumber+1)])  ## Average the values from 1994-2001, before some regions restructured into wholesale markets wher IOUs no longer owned generation
  dollars_per_person_NoStorage_2050 = (1/(3*EIoF_Regions_Population_Projections$X2050[RegionNumber]))*1e9*(deprec_3yr_TandD_NoStorage + deprec_3yr_all_pp_NoStorage + opex_3yr_TandD_NoStorage + opex_3yr_all_pp_NoStorage + capex_3yr_AnnualNuke_NoStorage)
  dollars_per_residential_customer_NoStorage_2050 = ResidentialRevenueFraction*dollars_per_person_NoStorage_2050/ElectricityCustomer_Population_Ratios$X2018[RegionNumber]
  
  ## With Annual Storage: Calculate cents/kWh - CAPEX, OPEX, and TOTAL
  ## Make this equal to last 3 yrs of CAPEX and OPEX divided by last 3 yrs of electricity generation
  search_type1 = gg_out_AnnualStorage$type
  search_value1 = gg_out_AnnualStorage$value
  capex_3yr_TandD_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="TandD"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="TandD"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="TandD"))]]
  ## Note "capex_3yr_all_pp_AnnualStorage" must go from 2047-2049 since the Google Sheet costs force (by definition) 2050 capital spending for power plants to be zero (0)
  capex_3yr_all_pp_AnnualStorage <- gg_out_AnnualStorage$`2047`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))]] + gg_out_AnnualStorage$`2048`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="all_pp"))]]
  capex_3yr_AnnualNuke_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="annual_nuclear"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="annual_nuclear"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="capex")[which(which(search_type1=="capex") %in% which(search_value1=="annual_nuclear"))]]
  opex_3yr_TandD_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="opex")[which(which(search_type1=="opex") %in% which(search_value1=="TandD"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="opex")[which(which(search_type1=="opex") %in% which(search_value1=="TandD"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="opex")[which(which(search_type1=="opex") %in% which(search_value1=="TandD"))]] 
  opex_3yr_all_pp_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="opex_wfuel")[which(which(search_type1=="opex_wfuel") %in% which(search_value1=="all_pp"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="opex_wfuel")[which(which(search_type1=="opex_wfuel") %in% which(search_value1=="all_pp"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="opex_wfuel")[which(which(search_type1=="opex_wfuel") %in% which(search_value1=="all_pp"))]] 
  deprec_3yr_TandD_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="TandD"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="TandD"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="TandD"))]]
  deprec_3yr_all_pp_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="all_pp"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="all_pp"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="depreciation_int")[which(which(search_type1=="depreciation_int") %in% which(search_value1=="all_pp"))]]
  TWh_3yr_AnnualStorage <- gg_out_AnnualStorage$`2048`[which(search_type1=="generation")[which(which(search_type1=="generation") %in% which(search_value1=="total"))]] + gg_out_AnnualStorage$`2049`[which(search_type1=="generation")[which(which(search_type1=="generation") %in% which(search_value1=="total"))]] + gg_out_AnnualStorage$`2050`[which(search_type1=="generation")[which(which(search_type1=="generation") %in% which(search_value1=="total"))]] 
  cents_per_kwh_AnnualStorage_capex_2050 = 100*(capex_3yr_TandD_AnnualStorage + capex_3yr_all_pp_AnnualStorage)/TWh_3yr_AnnualStorage
  cents_per_kwh_AnnualStorage_opex_2050 = 100*(opex_3yr_TandD_AnnualStorage + opex_3yr_all_pp_AnnualStorage)/TWh_3yr_AnnualStorage
  ## "cents_per_kwh_AnnualStorage_deprec_interest_2050" includes the annual capital expenditures for existing nuclear power plants
  cents_per_kwh_AnnualStorage_deprec_interest_2050 = 100*(capex_3yr_AnnualNuke_AnnualStorage + deprec_3yr_TandD_AnnualStorage + deprec_3yr_all_pp_AnnualStorage)/TWh_3yr_AnnualStorage
  cents_per_kwh_AnnualStorage_total_2050 = cents_per_kwh_AnnualStorage_opex_2050 + cents_per_kwh_AnnualStorage_deprec_interest_2050
  dollars_per_person_AnnualStorage_2050 = (1/(3*EIoF_Regions_Population_Projections$X2050[RegionNumber]))*1e9*(deprec_3yr_TandD_AnnualStorage + deprec_3yr_all_pp_AnnualStorage + opex_3yr_TandD_AnnualStorage + opex_3yr_all_pp_AnnualStorage + capex_3yr_AnnualNuke_AnnualStorage)
  dollars_per_residential_customer_AnnualStorage_2050 = ResidentialRevenueFraction*dollars_per_person_AnnualStorage_2050/ElectricityCustomer_Population_Ratios$X2018[RegionNumber]
  elec_cost_summary_2050 <- data.frame(c("cents_kwh_total","cents_kwh_capex","cents_kwh_depreciation_interest","cents_kwh_opex","dollars_per_person","dollars_per_customer"),
                                       c(cents_per_kwh_NoStorage_total_2050,cents_per_kwh_NoStorage_capex_2050,cents_per_kwh_NoStorage_deprec_interest_2050,cents_per_kwh_NoStorage_opex_2050,dollars_per_person_NoStorage_2050,dollars_per_residential_customer_NoStorage_2050),
                                       c(cents_per_kwh_AnnualStorage_total_2050,cents_per_kwh_AnnualStorage_capex_2050,cents_per_kwh_AnnualStorage_deprec_interest_2050,cents_per_kwh_AnnualStorage_opex_2050,dollars_per_person_AnnualStorage_2050,dollars_per_residential_customer_AnnualStorage_2050))
  colnames(elec_cost_summary_2050) <- c("data_type","NoStorage","AnnualStorage")
  
  ########################### END CALCULATE SPECIFIC 2050 ELECTRICITY COST SUMMARY VALUES FOR WEBSITE DISPLAY ###########################
  
  
  ########################### START SUMMARIZE PRIMARY ENERGY FLOWS FOR "STANDARD REPORT GENERATOR"  ###########################
  
  Primary_energy_types <- c("Biomass","Coal","Geothermal","Hydro","Natural_Gas","Nuclear","Petroleum","Solar","Wind","Total")
  Primary_energy_2050 <- rep(0,length(Primary_energy_types))
  for (i in 1:length(Primary_energy_types)) {
    ## Some energy types don't show up in the list of "sankey_json_out$links$From" if they are not chosen by the user (e.g., "Hydro" does not show up if user chooses 0%)
    if (any(sankey_json_out$links$From==Primary_energy_types[i]) == FALSE) {  ## Thus the current primary energy type does not show up as a Sankey edge
      Primary_energy_2050[i] <- 0
    } else {
      Primary_energy_2050[i] <- sum(sankey_json_out$links$Value[which(sankey_json_out$links$From==Primary_energy_types[i])])
    }
  }
  Primary_energy_2050[which(Primary_energy_types=="Total")] <- sum(Primary_energy_2050[which(Primary_energy_types!="Total")])  ## Add the total primary energy to the data frame
  load("generate_FinalUVY_2050_data/Base_UV_Matrices.Rdata")  ## This loads baseline U, V, and Y matrices with values independent of user's inputs
  U2016_baseline = U_2016_list[[RegionNumber]]
  rownames(U2016_baseline) <- U2016_baseline[,1]
  U2016_baseline <- U2016_baseline[,-1]
  U_PrimaryEnergyNames <- c('Biomass_Flow','Coal_Flow','Geothermal_Flow','Hydro_Flow','NaturalGas_Flow','Nuclear_Flow','Petroleum_Flow','Solar_Flow','Wind_Flow','Total')
  Primary_energy_2016 <- rep(0,length(U_PrimaryEnergyNames))
  for (i in 1:(length(U_PrimaryEnergyNames)-1)) {
    Primary_energy_2016[i] <- sum(U2016_baseline[paste0(U_PrimaryEnergyNames[i]),])/10^15
  }
  Primary_energy_2016[which(U_PrimaryEnergyNames=="Total")] <- sum(Primary_energy_2016[which(U_PrimaryEnergyNames!="Total")])
  Primary_energy_2016_pct <- 100*Primary_energy_2016/Primary_energy_2016[which(U_PrimaryEnergyNames=="Total")]
  Primary_energy_2050_pct <- 100*Primary_energy_2050/Primary_energy_2050[which(Primary_energy_types=="Total")]
  ## Add the TWh from each fuel/technology for 2016 for purposes of input into JSON output for "Standard Report"
  btu_per_kwh <- 3412.14
  U_ElectricityNames <- c('Biomass_Electricity','Coal_Electricity','Geothermal_Electricity','Hydro_Electricity','NaturalGas_Electricity','Nuclear_Electricity','Petroleum_Electricity','Solar_Electricity','Wind_Electricity','Total')
  ElectricityGen_2016 <- rep(0,length(U_ElectricityNames))
  for (i in 1:(length(U_ElectricityNames)-1)) {
    ElectricityGen_2016[i] <- U2016_baseline[paste0(U_ElectricityNames[i]),'Electricity_Grid']/btu_per_kwh/10^9
  }
  ElectricityGen_2016[which(U_ElectricityNames=="Total")] <- sum(ElectricityGen_2016[which(U_ElectricityNames!="Total")])
  ElectricityGen_pct_2016 <- 100*ElectricityGen_2016/ElectricityGen_2016[which(U_ElectricityNames=="Total")]
  PrimaryEnergySummary <- data.frame(Primary_energy_types,Primary_energy_2016,Primary_energy_2050,Primary_energy_2016_pct,Primary_energy_2050_pct,ElectricityGen_2016,ElectricityGen_pct_2016)
  names(PrimaryEnergySummary) <- c("Fuel","2016_quads","2050_quads","2016_quads_pct","2050_quads_pct","2016_Elec_TWh","2016_Elec_TWh_pct")

  ## Add total TWh and a "reached correct percent flag" to PPdata_AnnualStorage and PPdata_NoStorage
  ## AnnualStorage case
  PPdata_newrow <- PPdata_AnnualStorage[1,]
  PPdata_newrow$Technology <- "Total"
  PPdata_newrow$MW_needed <- sum(PPdata_AnnualStorage$MW_needed) - PPdata_AnnualStorage$MW_needed[which(PPdata_AnnualStorage$Technology=="AnnualStorage_Total")]
  PPdata_newrow$Fraction_MWhActual <- sum(PPdata_AnnualStorage$Fraction_MWhActual) - PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="AnnualStorage_Total")]
  PPdata_newrow$TWhGeneration <- sum(PPdata_AnnualStorage$TWhGeneration) - PPdata_AnnualStorage$TWhGeneration[which(PPdata_AnnualStorage$Technology=="AnnualStorage_Total")]
  PPdata_AnnualStorage <- rbind(PPdata_AnnualStorage,PPdata_newrow)
  PPdata_AnnualStorage$Pct_MWhActual <- 100*PPdata_AnnualStorage$Fraction_MWhActual
  rm(PPdata_newrow)
  PPdata_AnnualStorage$Pct_MWh_flag <- rep(1,dim(PPdata_AnnualStorage)[1])
  tol_pct = .5  ## The "+/-" tolerance on what is acceptable in terms of the alogorithm solving to achieve the user's desired electricity mix
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Biomass")] < (inputs$web_inputs[which(row.names(inputs)=="biomass_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Biomass")] > (inputs$web_inputs[which(row.names(inputs)=="biomass_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="Biomass")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Coal")] < (inputs$web_inputs[which(row.names(inputs)=="coal_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Coal")] > (inputs$web_inputs[which(row.names(inputs)=="coal_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="Coal")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="CSP")] < (inputs$web_inputs[which(row.names(inputs)=="CSP_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="CSP")] > (inputs$web_inputs[which(row.names(inputs)=="CSP_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="CSP")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="PV")] < (inputs$web_inputs[which(row.names(inputs)=="PV_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="PV")] > (inputs$web_inputs[which(row.names(inputs)=="PV_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="PV")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Wind")] < (inputs$web_inputs[which(row.names(inputs)=="wind_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Wind")] > (inputs$web_inputs[which(row.names(inputs)=="wind_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="Wind")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="HydroDispatch")] < (inputs$web_inputs[which(row.names(inputs)=="hydro_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="HydroDispatch")] > (inputs$web_inputs[which(row.names(inputs)=="hydro_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="HydroDispatch")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Nuclear")] < (inputs$web_inputs[which(row.names(inputs)=="nuclear_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Nuclear")] > (inputs$web_inputs[which(row.names(inputs)=="nuclear_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="Nuclear")]=0
  }
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="PetroleumCC")] < (inputs$web_inputs[which(row.names(inputs)=="petroleum_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="PetroleumCC")] > (inputs$web_inputs[which(row.names(inputs)=="petroleum_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="PetroleumCC")]=0
  }
  NG_total_Pct <- PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="NGCC")] + PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="NGCT")] 
  if (NG_total_Pct < (inputs$web_inputs[which(row.names(inputs)=="ng_percent")] - tol_pct) |  NG_total_Pct > (inputs$web_inputs[which(row.names(inputs)=="ng_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="NGCC")]=0
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="NGCT")]=0
  }
  rm(NG_total_Pct)
  if (PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Geothermal")] < (inputs$web_inputs[which(row.names(inputs)=="geothermal_percent")] - tol_pct) |  PPdata_AnnualStorage$Pct_MWhActual[which(PPdata_AnnualStorage$Technology=="Geothermal")] > (inputs$web_inputs[which(row.names(inputs)=="geothermal_percent")] + tol_pct) ) {
    PPdata_AnnualStorage$Pct_MWh_flag[which(PPdata_AnnualStorage$Technology=="Geothermal")]=0
  }
  
  ## NoStorage case
  PPdata_newrow <- PPdata_NoStorage[1,]
  PPdata_newrow$Technology <- "Total"
  PPdata_newrow$MW_needed <- sum(PPdata_NoStorage$MW_needed) - PPdata_NoStorage$MW_needed[which(PPdata_NoStorage$Technology=="AnnualStorage_Total")]
  PPdata_newrow$Fraction_MWhActual <- sum(PPdata_NoStorage$Fraction_MWhActual) - PPdata_NoStorage$Fraction_MWhActual[which(PPdata_NoStorage$Technology=="AnnualStorage_Total")]
  PPdata_newrow$TWhGeneration <- sum(PPdata_NoStorage$TWhGeneration) - PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="AnnualStorage_Total")]
  PPdata_NoStorage <- rbind(PPdata_NoStorage,PPdata_newrow)
  PPdata_NoStorage$Pct_MWhActual <- 100*PPdata_NoStorage$Fraction_MWhActual
  rm(PPdata_newrow)
  PPdata_NoStorage$Pct_MWh_flag <- rep(1,dim(PPdata_NoStorage)[1])
  tol_pct = .5  ## The "+/-" tolerance on what is acceptable in terms of the alogorithm solving to achieve the user's desired electricity mix
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Biomass")] < (inputs$web_inputs[which(row.names(inputs)=="biomass_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Biomass")] > (inputs$web_inputs[which(row.names(inputs)=="biomass_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="Biomass")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Coal")] < (inputs$web_inputs[which(row.names(inputs)=="coal_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Coal")] > (inputs$web_inputs[which(row.names(inputs)=="coal_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="Coal")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="CSP")] < (inputs$web_inputs[which(row.names(inputs)=="CSP_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="CSP")] > (inputs$web_inputs[which(row.names(inputs)=="CSP_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="CSP")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="PV")] < (inputs$web_inputs[which(row.names(inputs)=="PV_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="PV")] > (inputs$web_inputs[which(row.names(inputs)=="PV_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="PV")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Wind")] < (inputs$web_inputs[which(row.names(inputs)=="wind_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Wind")] > (inputs$web_inputs[which(row.names(inputs)=="wind_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="Wind")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="HydroDispatch")] < (inputs$web_inputs[which(row.names(inputs)=="hydro_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="HydroDispatch")] > (inputs$web_inputs[which(row.names(inputs)=="hydro_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="HydroDispatch")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Nuclear")] < (inputs$web_inputs[which(row.names(inputs)=="nuclear_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Nuclear")] > (inputs$web_inputs[which(row.names(inputs)=="nuclear_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="Nuclear")]=0
  }
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="PetroleumCC")] < (inputs$web_inputs[which(row.names(inputs)=="petroleum_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="PetroleumCC")] > (inputs$web_inputs[which(row.names(inputs)=="petroleum_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="PetroleumCC")]=0
  }
  NG_total_Pct <- PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="NGCC")] + PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="NGCT")] 
  if (NG_total_Pct < (inputs$web_inputs[which(row.names(inputs)=="ng_percent")] - tol_pct) |  NG_total_Pct > (inputs$web_inputs[which(row.names(inputs)=="ng_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="NGCC")]=0
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="NGCT")]=0
  }
  rm(NG_total_Pct)
  if (PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Geothermal")] < (inputs$web_inputs[which(row.names(inputs)=="geothermal_percent")] - tol_pct) |  PPdata_NoStorage$Pct_MWhActual[which(PPdata_NoStorage$Technology=="Geothermal")] > (inputs$web_inputs[which(row.names(inputs)=="geothermal_percent")] + tol_pct) ) {
    PPdata_NoStorage$Pct_MWh_flag[which(PPdata_NoStorage$Technology=="Geothermal")]=0
  }
  
  ########################### END SUMMARIZE PRIMARY ENERGY FLOWS FOR "STANDARD REPORT GENERATOR"  ###########################
  
  
  ########################### START ARRANGE DATA FOR OUTPUT TO WEBSITE ###########################

  all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, PPdata_NoStorage, PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary,solveGEN_output$WindSolar_InputIntoStorage_AnnualTWh)
  names(all) <- c('sankey_links','sankey_nodes','Hourly_MW_AnnualStorage', 'Hourly_MW_NoStorage', 'PPdata_NoStorage', 'PPdata_AnnualStorage', 'ggsheets_output_AnnualStorage', 'ggsheets_output_NoStorage', 'website_inputs', 'elec_cost_summary_2050', 'PrimaryEnergySummary','WindSolar_InputIntoStorage_AnnualTWh')
  
  ########################### END ARRANGE DATA FOR OUTPUT TO WEBSITE ###########################
  
  print("master_EIoF.R -- just before return(all)")
  return(all)
  
}

# Inputs for testing function
#test = master_EIoF(
#  region_id = 6,
#  coal_percent = 0,
#  PV_percent = 50,
#  CSP_percent = 0,
#  wind_percent = 50,
#  biomass_percent = 0,
#  hydro_percent = 0,
#  petroleum_percent = 0,
#  nuclear_percent = 0,
#  geothermal_percent = 0,
#  ng_percent = 0,
#  ldv_e = 70,
#  r_sh_e = 45,
#  r_sh_ng = 50)
