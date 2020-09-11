## This function will be the One EIoF function to rule them all, One function to source them, 
## One function to bring them all and in the virtual machine bind them.

## This function will act as the main function to be used via Plumber in the TACC VM.
## It waits for a URL API from the UX website and calls all of the pieces of the EIoF
## to generate and return a JSON file to the website for plotting.

## master_EIoF.R function 
## Carey W. King, PhD
## 2020-07-30

# setwd('/scripts')

## the inputs from the website website API URL GET (I have not idea) call will be:
## 1) Region considered (between 1 and 13, inclusive)
## 2) End-uses that we allow the user to change
##    - home heating energy use (natural gas, electricity, biomass/other)
##    - light-duty vehicle energy use (petroleum vs. electricity)
## 3) Percent electricity generation from primary fuels

rm(list=ls(all=TRUE))
## Use this to call the function after it is sourced as a function:
start_time <- Sys.time()  ## This is just to know how long it took to run the code
# eiof_out <- master_EIoF(region_id = 6, coal_percent = 10, PV_percent = 10, CSP_percent = 0, wind_percent = 10, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 20, r_sh_e = 0, r_sh_ng = 100)
# end_time <- Sys.time()
# code_time=end_time - start_time
# print(code_time)
  
# master_EIoF <- function(region_id = 1, coal_percent = 10, PV_percent = 15, CSP_percent = 0, wind_percent = 15, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 50, r_sh_e = 50, r_sh_ng = 50){

## BASELINE RESIDENTIAL HEATING FRACTIONS 
## FracOther must be <= to the stated values
## This must hold true:  (FracHeatPump + FracNG) >= (1 - FracOther)
# EIoF_region	FracHeatPump	FracNG	FracOther
# NW	0.14	0.48	0.38
# CA	0.02	0.71	0.27
# MN	0.03	0.75	0.22
# SW	0.22	0.53	0.25
# CE	0.09	0.63	0.28
# TX	0.13	0.58	0.29
# MW	0.04	0.72	0.24
# AL	0.13	0.47	0.4
# MA	0.12	0.5	0.38
# SE	0.18	0.41	0.41
# FL	0.29	0.08	0.63
# NY	0.02	0.61	0.37
# NE	0.01	0.22	0.77

# ## inputs to make testing easier when running as a script and not a function
# ## TEXAS DEFAULT INPUTS
# region_id = 6
# coal_percent = 23
# PV_percent = 5
# CSP_percent = 1
# wind_percent = 10
# biomass_percent = 0
# hydro_percent = 0
# petroleum_percent = 0
# nuclear_percent = 8
# geothermal_percent = 0
# ng_percent = 53
# ldv_e = 13
# r_sh_e = 13
# r_sh_ng = 58

## Test inputs
region_id = 2
coal_percent = 0
PV_percent = 30
CSP_percent = 20
wind_percent = 30
biomass_percent = 0
hydro_percent = 10
petroleum_percent = 0
nuclear_percent = 0
geothermal_percent = 10
ng_percent = 0
ldv_e = 50
r_sh_ng = 0
r_sh_e = 100



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
  
  
  
  ##############################################################
  #### This could possibly be run outside of this function? ####
  ##############################################################
  
  ## load up any needed libraries
  library(lubridate)
  library(jsonlite)
  library(readr)
  
  ## pre calcualtions from inputs
  ng_percent = 100 - as.integer((coal_percent + PV_percent + CSP_percent + wind_percent + biomass_percent + hydro_percent + petroleum_percent + nuclear_percent + geothermal_percent))
  # ng_percent = 100 - (coal_percent + PV_percent + CSP_percent + wind_percent + biomass_percent + hydro_percent + petroleum_percent + nuclear_percent + geothermal_percent)
  
  ## Initialize information and confirm inputs are valid
  year = 2016  ## This is the year of input data to use for 8760 hour profiles of load, wind, and PV output. This might or might not ever allow user selection to use a different baseline year of data for load, wind, PV, and weather (e.g., year = 2017).
  RegionNumber = region_id  ## Specify the region number to calculate (there are 13 defined U.S. regions)
  if ( (RegionNumber >= 1) & (RegionNumber <= 13)){
    hey = 1 ## all is OK
  } else {
    ## PERHAPS HERE WE NEED TO OUTPUT A SET OF DEFAULT "NONSENSICAL VALUES" THAT ARE COMPATIBLE WITH THE WEBSITE
    ## TO MAKE IT OBVIOUS THAT THE SIMULATION DID NOT COMMENCE, YET A RESULT IS STILL RETURNED TO THE WEBSITE
    ## SO THAT THE WEBSITE IS NOT JUST HANGING UP WITHOUT THE USER KNOWING THERE WAS A PROBLEM???
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
    
  ## PREVIOUS SIMPLE ESTIAMTION TO RUN PROGRAM:
  # fraction_LDVmiles_per_region_2050 = (1/13)*rep(1,13)
  # LDVmiles_per_region_2050 <- data.frame(0)
  # assign("EIA_AEO2019_LDVmiles_2050",3472.650879*1e9)  ## miles driven all light duty vehicles (using liquid fuels) in 2050; value from AEO Ref. 2019 for U.S. is 3472.65087 billion miles
  # for (i in 1:13) {
  #   LDVmiles_per_region_2050[i] = fraction_LDVmiles_per_region_2050[i]*EIA_AEO2019_LDVmiles_2050
  # }
  # region_names <- c("R1_NW","R2_CA", "R3_MN", "R4_SW", "R5_CE", "R6_TX", "R7_MW", "R8_AL", "R9_MA", "R10_SE", "R11_FL", "R12_NY", "R13_NE")  ## specify the region names as they appear as the column names of the "8760" input profiles
  # names(LDVmiles_per_region_2050) <- c(region_names)
  
  source("generate8760.R")
  #generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV)
  #generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV,LDVmiles_per_region_2050[RegionNumber])
  generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV,LDVmiles_current_region_2050,LDV_miles_per_kwh_2050)
  
  ## Save "generate8760.R" in form that can be used as inputs to other functions
  Total_Hourly_MW_8760_CurrentRegion = generate8760_output$Total_Hourly_MW_8760_CurrentRegion
  Total_AnnualMWh_LDV_EVs = generate8760_output$Total_AnnualMWh_LDV_EVs
  #save(Total_Hourly_MW_8760_CurrentRegion,file="generate8760_output.Rdata")  ## I can just put "Total_Hourly_MW_8760_CurrentRegion" as an input into "solveGEN.R"
  
  # source('generate8760.R')
  # ldv_elec_quads <- sankey_json_out$links$Value[sankey_json_out$links$From == 'Electricity' & sankey_json_out$links$To == 'Transportation']
  # ev_charging_profile <- generate8760(ldv_quads = ldv_elec_quads)
  
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
  
  require(Rcgmin)  ## pakcage for gradient based optimization
  require(numDeriv)## Allows calculation of numerical derivative of a function within an evaluation function
  require(optimr)  ## pakcage for gradient based optimization
  
  source("solveGEN.R")
  ## Call the function
  #solveGEN_output <- solveGEN(RegionNumber = region_id, year = year, coal_percent = coal_percent, PV_percent = PV_percent, CSP_percent = CSP_percent, wind_percent = wind_percent, nuclear_percent = nuclear_percent, hydro_percent = hydro_percent, biomass_percent = biomass_percent, geothermal_percent = geothermal_percent, petroleum_percent = petroleum_percent)
  solveGEN_output <- solveGEN(RegionNumber = region_id, year = year, coal_percent = coal_percent, PV_percent = PV_percent, CSP_percent = CSP_percent, wind_percent = wind_percent, nuclear_percent = nuclear_percent, hydro_percent = hydro_percent, biomass_percent = biomass_percent, geothermal_percent = geothermal_percent, petroleum_percent = petroleum_percent,Total_Hourly_MW_8760_CurrentRegion)

  ## Specify certain data from solveGEN_outoput for use by "generate_FinalUVY_2050.R"
  PPdata_NoStorage <- solveGEN_output$PPdata_NoStorage
  PPdata_AnnualStorage <- solveGEN_output$PPdata_AnnualStorage
  ## Make a correction here to ensure no singular matrix calculations in Sankey diagrams.
  ## THe correction is to ensure very small values of generation do not occur for technologies for which the user does not want any generation
  
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
  #generate_FinalUVY_2050_output <- generate_FinalUVY_2050(RegionNumber = region_id, percent_ResidentialHeatPump = percent_ResidentialHeatPump, percent_ResidentialNG = percent_ResidentialNG)
  #generate_FinalUVY_2050_output <- generate_FinalUVY_2050(RegionNumber = region_id, percent_ResidentialHeatPump = percent_ResidentialHeatPump, percent_ResidentialNG = percent_ResidentialNG, PPdata_NoStorage,PPdata_AnnualStorage,Hourly_MW_NoStorage,Hourly_MW_AnnualStorage)
  #generate_FinalUVY_2050_output <- generate_FinalUVY_2050(RegionNumber = region_id, percent_ResidentialHeatPump = percent_ResidentialHeatPump, percent_ResidentialNG = percent_ResidentialNG, Hourly_MW_NoStorage,Hourly_MW_AnnualStorage,PPdata_NoStorage,PPdata_AnnualStorage,percent_ElectricLDV,LDVmiles_per_region_2050[RegionNumber],Total_AnnualMWh_LDV_EVs)
  generate_FinalUVY_2050_output <- generate_FinalUVY_2050(RegionNumber = region_id, percent_ResidentialHeatPump = percent_ResidentialHeatPump, percent_ResidentialNG = percent_ResidentialNG, Hourly_MW_NoStorage,Hourly_MW_AnnualStorage,PPdata_NoStorage,PPdata_AnnualStorage,percent_ElectricLDV,LDVmiles_current_region_2050,Total_AnnualMWh_LDV_EVs)
  save(generate_FinalUVY_2050_output,file="generate_FinalUVY_2050_output.Rdata")  ## I can just put "Total_Hourly_MW_8760_CurrentRegion" as an input into "solveGEN.R"
  
  ########################### END CAREY "generate_FinalUVY_2050.R"  ###########################



  ## call Jianwei's code (Sankey.R) to edit the Sankey based on user input
  ## inputs: end use changes, percent electricity generation from primary fuels, region
  ## outputs : JSON file with new values used to create Sankey
  ########################### BEGIN Jianwei Sankey Code ###########################
  
  source('Sankey_Function.R')
  #source('Sankey_Function_JDR.R')
  #source('Sankey_Function_JDR_cwk20200310.R')

  ## Inputs to sankey code are as XXXXXXX ... 
  ## All of the inputs to the Sankey code are treated as "percentages of the total for flows through some node" (with values 0 - 100).
  ## Thus certain items need to sum to 100 (100 percent) as follows:
  ## 100 = p_solar + p_nuclear + p_hydro + p_wind + p_geo + p_ng + p_bio + p_coal + p_ng + p_petrol
  ## 100 = 
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
  #ldv_elec = ldv_e  ## fraction of LDV miles driven on electricity
  cat(paste0("Need to add LDV miles driven per region (and per month or season in charging profile?)."),sep="\n")
  cat(paste0("Need to add electric ldv miles per kwh variation per region and/or temperature ... IF ADDING THIS, DO IT IN GENERATE8760."),sep="\n")
  #ldv_petrol = 30
  #ldv_ethanol = 20
  ldv_totalquads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Petrol"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Elec"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Ethanol"])
  ldv_petrol_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Petrol"])
  ldv_elec_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Elec"])
  ldv_biofuel_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Ethanol"])
  percent_ldv_elec_quads = 100*ldv_elec_quads/ldv_totalquads
  percent_ldv_petrol_quads = 100*ldv_petrol_quads/ldv_totalquads
  percent_ldv_biofuel_quads = 100 - percent_ldv_elec_quads - percent_ldv_petrol_quads ## Calcualting this way ensures these three percentages add exactly to 100  (and not off by some differnece like 1e-14)

  ## Other (non-LDV transportation)
  trans_other_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Petrol"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Other"])
  if (trans_other_totalenergy>0){
    trans_other_petrol = 100*sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Petrol"])/trans_other_totalenergy
    trans_other_ng = 100*sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_NG"])/trans_other_totalenergy
    #trans_other_other = max(0,100 - trans_other_petrol - trans_other_ng)  ## sometimes "100 - trans_other_petrol - trans_other_ng" expression can result in small negative values within double precision, so need to make 0 if so
    trans_other_other = 100 - trans_other_petrol - trans_other_ng  ## sometimes "100 - trans_other_petrol - trans_other_ng" expression can result in small negative values within double precision, so need to make 0 if so
    if (trans_other_other < 0) {
      trans_other_ng <- trans_other_ng + trans_other_other
      trans_other_other = 0
    }
  } else {
    trans_other_petrol = 100
    trans_other_ng = 0
    trans_other_other = 0
  }
  
  solar_percent = PV_percent + CSP_percent
  ## Load blank Y matrix template for U, V, and Y Sankey calculations
  # Y_template = read.csv("Y_template.csv",row.names = 1)
  # U_template = read.csv("U_template.csv",row.names = 1)
  # V_template = read.csv("V_template.csv",row.names = 1)
  # save(Y_template,U_template,V_template,file="generate_FinalUVY_2050_data/UVY_templates.Rdata")
  load("generate_FinalUVY_2050_data/UVY_templates.Rdata")
  
  ## Call to solve sankey with the "NonStorage" results of "solveGEN" and "generate_FinalUVY_2050"
  # sankey_json_out <- sankey_json(region_id = region_id, p_solar = solar_percent, p_nuclear = nuclear_percent, p_hydro = hydro_percent, p_wind = wind_percent, p_geo = geothermal_percent, p_ng = ng_percent, p_coal = coal_percent, p_bio = biomass_percent, p_petrol = petroleum_percent, r_sh_e = r_sh_e, r_wh_e = 100, r_ck_e = 100, c_sh_e = 100, c_wh_e = 100, c_ck_e = 100, ldv_elec = ldv_e, ldv_petrol = 30, ldv_ethanol = 20, trans_other_petrol = 40, trans_other_ng = 10, trans_other_other = 50)
  # sankey_json_out <- sankey_json(region_id = region_id, p_solar = solar_percent, p_nuclear = nuclear_percent, p_hydro = hydro_percent, p_wind = wind_percent, p_geo = geothermal_percent, p_ng = ng_percent, p_coal = coal_percent, p_bio = biomass_percent, p_petrol = petroleum_percent, r_sh_e = r_sh_e, r_wh_e = r_wh_e, r_ck_e = r_ck_e, c_sh_e = c_sh_e, c_wh_e = c_wh_e, c_ck_e = c_ck_e, ldv_elec = percent_ldv_elec_quads, ldv_petrol = percent_ldv_petrol_quads, ldv_ethanol = percent_ldv_biofuel_quads, trans_other_petrol = trans_other_petrol, trans_other_ng = trans_other_ng, trans_other_other = trans_other_other)
  ## TEST INPUTS TO CALL SANKEY CODE:
  ## sankey_json_out <- sankey_json(region_id = 1, p_solar = 5, p_nuclear = 5, p_hydro = 5, p_wind = 5, p_geo = 5, p_ng = 60, p_coal = 5, p_bio = 5, p_petrol = 5, r_sh_e = 50, r_sh_ng = 50, r_wh_e = 50, r_ck_e = 50, c_sh_e = 50, c_wh_e = 50, c_ck_e = 50, ldv_elec = 20, ldv_petrol = 70, ldv_ethanol = 10, trans_other_petrol = 90, trans_other_ng = 10, trans_other_other = 0)
  if (percent_ldv_elec_quads < 1) {
    percent_ldv_elec_quads = 0.01
    percent_ldv_petrol_quads = percent_ldv_petrol_quads
    percent_ldv_biofuel_quads = 100 - percent_ldv_elec_quads - percent_ldv_petrol_quads
    }  ## For purposes of plotting the Sankey Diagram consistently, if "ldv_e = 0" from user, we need some > 0 value for electricity to LDVs so that the "Transportation" node is displayed in alignment with the other "end use" sectors  
  #sankey_json_out <- sankey_json(region_id = region_id, p_solar = solar_percent, p_nuclear = nuclear_percent, p_hydro = hydro_percent, p_wind = wind_percent, p_geo = geothermal_percent, p_ng = ng_percent, p_coal = coal_percent, p_bio = biomass_percent, p_petrol = petroleum_percent, r_sh_e = r_sh_e, r_sh_ng = r_sh_ng, r_wh_e = r_wh_e, r_wh_ng = r_wh_ng, r_ck_e = r_ck_e, r_ck_ng = r_ck_ng, c_sh_e = c_sh_e, c_sh_ng = c_sh_ng, c_wh_e = c_wh_e, c_wh_ng = c_wh_ng, c_ck_e = c_ck_e, c_ck_ng = c_ck_ng, ldv_elec = percent_ldv_elec_quads, ldv_petrol = percent_ldv_petrol_quads, ldv_ethanol = percent_ldv_biofuel_quads, trans_other_petrol = trans_other_petrol, trans_other_ng = trans_other_ng, trans_other_other = trans_other_other,generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion,generate_FinalUVY_2050_output$V_NoStorage_2050_CurrentRegion,Y_template)

  ## Call Sankey function with "AnnualStorage" results of "solveGEN" and as input into "sankey_json"
  sankey_input_pct_AnnualStorage_Solar <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="PV")]+PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="CSP")])
  sankey_input_pct_AnnualStorage_Wind <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Wind")])
  sankey_input_pct_AnnualStorage_Biomass <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Biomass")])
  sankey_input_pct_AnnualStorage_Coal <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Coal")])
  sankey_input_pct_AnnualStorage_Nuclear <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Nuclear")])
  sankey_input_pct_AnnualStorage_Petroleum <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="PetroleumCC")])
  sankey_input_pct_AnnualStorage_Geothermal <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="Geothermal")])
  sankey_input_pct_AnnualStorage_Hydro <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="HydroDispatch")])
  #sankey_input_pct_AnnualStorage_NG <- 100*(PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="NGCC")]+PPdata_AnnualStorage$Fraction_MWhActual[which(PPdata_AnnualStorage$Technology=="NGCT")])
  sankey_input_pct_AnnualStorage_NG <- 100 - sum(sankey_input_pct_AnnualStorage_Solar,sankey_input_pct_AnnualStorage_Wind,sankey_input_pct_AnnualStorage_Biomass,sankey_input_pct_AnnualStorage_Coal,sankey_input_pct_AnnualStorage_Nuclear,sankey_input_pct_AnnualStorage_Petroleum,sankey_input_pct_AnnualStorage_Geothermal,sankey_input_pct_AnnualStorage_Hydro)
  pct_check <- sum(sankey_input_pct_AnnualStorage_Solar,sankey_input_pct_AnnualStorage_Wind,sankey_input_pct_AnnualStorage_Biomass,sankey_input_pct_AnnualStorage_NG,sankey_input_pct_AnnualStorage_Coal,sankey_input_pct_AnnualStorage_Nuclear,sankey_input_pct_AnnualStorage_Petroleum,sankey_input_pct_AnnualStorage_Geothermal,sankey_input_pct_AnnualStorage_Hydro)
  #sankey_inputs_AnnualStorage <- c(region_id, solar_percent, nuclear_percent, hydro_percent, wind_percent, geothermal_percent, ng_percent, coal_percent, biomass_percent, petroleum_percent, r_sh_e, r_sh_ng, r_wh_e, r_wh_ng, r_ck_e, r_ck_ng, c_sh_e, c_sh_ng, c_wh_e, c_wh_ng, c_ck_e, c_ck_ng, percent_ldv_elec_quads, percent_ldv_petrol_quads, percent_ldv_biofuel_quads, trans_other_petrol, trans_other_ng, trans_other_other)
  sankey_json_out <- sankey_json(region_id = region_id, p_solar = sankey_input_pct_AnnualStorage_Solar, p_nuclear = sankey_input_pct_AnnualStorage_Nuclear, p_hydro = sankey_input_pct_AnnualStorage_Hydro, p_wind = sankey_input_pct_AnnualStorage_Wind, p_geo = sankey_input_pct_AnnualStorage_Geothermal, p_ng = sankey_input_pct_AnnualStorage_NG, p_coal = sankey_input_pct_AnnualStorage_Coal, p_bio = sankey_input_pct_AnnualStorage_Biomass, p_petrol = sankey_input_pct_AnnualStorage_Petroleum, r_sh_e = r_sh_e, r_sh_ng = r_sh_ng, r_wh_e = r_wh_e, r_wh_ng = r_wh_ng, r_ck_e = r_ck_e, r_ck_ng = r_ck_ng, c_sh_e = c_sh_e, c_sh_ng = c_sh_ng, c_wh_e = c_wh_e, c_wh_ng = c_wh_ng, c_ck_e = c_ck_e, c_ck_ng = c_ck_ng, ldv_elec = percent_ldv_elec_quads, ldv_petrol = percent_ldv_petrol_quads, ldv_ethanol = percent_ldv_biofuel_quads, trans_other_petrol = trans_other_petrol, trans_other_ng = trans_other_ng, trans_other_other = trans_other_other,generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion,generate_FinalUVY_2050_output$V_AnnualStorage_2050_CurrentRegion,Y_template)
  
  ########################### END Jianwei Sankey Code ###########################
  

  ########################### BEGIN GOOGLESHEET ACCESS ###########################

  source('EIoF_gs4_function.R')
  
  # target <- c("Coal", "Nuclear", "NGCC", "NGCT", "HydroDispatch", "PV", "Wind", "Geothermal", "Biomass", "Other", "PetroleumCC", "AnnualStorage_Total")
  target <- c("Coal", "Nuclear", "NGCC", "NGCT", "HydroDispatch", "PV", "Wind", "Geothermal", "Biomass", "Other", "PetroleumCC", "AnnualStorage_Total","CSP")  ## This orders the input data into the Google Sheet
  load("solveGen_data/Tranfer_RegionFromTo.Rdata") ## Load data with the (1) miles of transmission to connect resources, per region and (2) % of electricity from resource coming from which region to the current RegionNumber
  
  ## +++++++++++
  ## CALL GOOGLE SHEET FOR "NO STORAGE" OPTION
  ## +++++++++++
  ## massage outputs from solveGEN to go into the googlesheets code for "No Storage" solvGEN outputs
  GS_inputs_NoStorage <- solveGEN_output$PPdata_NoStorage[c('Technology', 'MW_needed', 'TWhGeneration')]
  GS_inputs_NoStorage <- GS_inputs_NoStorage[-which(GS_inputs_NoStorage$Technology=="LandTotal"),]  ## remove the row "LandTotal" since that is not to be written into the Google Sheet
  GS_inputs_NoStorage <- GS_inputs_NoStorage[match(target, GS_inputs_NoStorage$Technology),]
  GS_inputs_NoStorage$Technology <- target
  GS_inputs_NoStorage[is.na(GS_inputs_NoStorage)] <- 0
  ## SECOND: Residential NG demand for space heating and all other (non space heating) since that has been affected by user's choice of fuels for Residential Heating 
  NG_ResHeating_NoStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_SpaceHeating_NG']
  NG_NonResHeating_NoStorage <- (1/10^15)*(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_WaterHeating_NG'] + generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_Cooking_NG'] + generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['NaturalGas_Flow','Resident_Other'])
  # NG_ResHeating_NoStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[6,21]
  # NG_NonResHeating_NoStorage <- (1/10^15)*(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[6,23] + generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[6,25] + generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[6,27])
  ## THRID: Petroleum consumption (quads) for LDv travel (since that has been affected by user's choice of fuels for LDVs)
  Petro_LDV_NoStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion['Petroleum_Flow','Transport_LDV_Petrol']
  # Petro_LDV_NoStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[9,36]
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
  # gg_out_NoStorage <- as.data.frame(gg_out_NoStorage[,-1])  ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet
  
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
  # NG_ResHeating_AnnualStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion[6,21]
  # NG_NonResHeating_AnnualStorage <- (1/10^15)*(generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion[6,23] + generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion[6,25] + generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion[6,27])
  ## THRID: Petroleum consumption (quads) for LDv travel (since that has been affected by user's choice of fuels for LDVs)
  Petro_LDV_AnnualStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion['Petroleum_Flow','Transport_LDV_Petrol']
  # Petro_LDV_AnnualStorage <- (1/10^15)*generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion[9,36]
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
  # gg_out_AnnualStorage <- as.data.frame(gg_out_AnnualStorage[,-1]) ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet

  ## Call Google Sheet (with inputs for BTOH AnnualStorage & NoStorage cases)
  gg_out_all <- EIoF_gs4_function(RegionNumber,GS_inputs_AnnualStorage = GS_inputs_AnnualStorage,GS_inputs_NoStorage = GS_inputs_NoStorage)
  gg_out_AnnualStorage <- gg_out_all[[1]]
  gg_out_NoStorage <- gg_out_all[[2]]
  gg_out_NoStorage <- as.data.frame(gg_out_NoStorage[,-1])  ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet
  gg_out_AnnualStorage <- as.data.frame(gg_out_AnnualStorage[,-1]) ## remove first column of output data frame which is the descriptors of the data from "Aggregation" Tab of the Google Sheet
  
  
  ## Rearrange Google Sheet output data
  #gs_auth(token = "googlesheets_token.rds") ## Authorize acess to the Google Sheet
  gg_out2 <- lapply(split(gg_out_AnnualStorage,gg_out_AnnualStorage$type,drop = TRUE), function(x) split(x, x[['value']], drop = TRUE))
  gg_out3 <- lapply(split(gg_out_NoStorage,    gg_out_NoStorage$type,    drop = TRUE), function(x) split(x, x[['value']], drop = TRUE))
  # print(paste('New 2050 CAPEX is: ', gg_out, sep = ''))
  
  
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
  # dollars_per_person_NoStorage_2050 = (1/(3*EIoF_Regions_Population_Projections$X2050[RegionNumber]))*1e9*(capex_3yr_TandD_NoStorage + capex_3yr_all_pp_NoStorage + opex_3yr_TandD_NoStorage + opex_3yr_all_pp_NoStorage)
  # dollars_per_person_NoStorage_2050 = (1/(3*EIoF_Regions_Population_Projections$X2050[RegionNumber]))*1e9*(deprec_3yr_TandD_NoStorage + deprec_3yr_all_pp_NoStorage + opex_3yr_TandD_NoStorage + opex_3yr_all_pp_NoStorage)
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
  # dollars_per_person_AnnualStorage_2050 = (1/(3*EIoF_Regions_Population_Projections$X2050[RegionNumber]))*1e9*(capex_3yr_TandD_AnnualStorage + capex_3yr_all_pp_AnnualStorage + opex_3yr_TandD_AnnualStorage + opex_3yr_all_pp_AnnualStorage)
  # dollars_per_person_AnnualStorage_2050 = (1/(3*EIoF_Regions_Population_Projections$X2050[RegionNumber]))*1e9*(deprec_3yr_TandD_AnnualStorage + deprec_3yr_all_pp_AnnualStorage + opex_3yr_TandD_AnnualStorage + opex_3yr_all_pp_AnnualStorage)
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
  ## add a column to "PPdata_AnnualStorage" as a flag for whether the user's desired % of electricity was achieved or not: 1=achieved; 0 = not achieved
  PPdata_AnnualStorage$Pct_MWh_flag <- rep(1,dim(PPdata_AnnualStorage)[1])
  tol_pct = 0.5  ## The "+/-" tolerance on what is acceptable in terms of the alogorithm solving to achieve the user's desired electricity mix
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
  ## add a column to "PPdata_NoStorage" as a flag for whether the user's desired % of electricity was achieved or not: 1=achieved; 0 = not achieved
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
  
  
  ########################### START MAKE FIGURES ###########################
  # ==============================================================================
  # COMMON COLORS -----------------------------------------------------
  # ==============================================================================
  red = rgb(0.894,0.090,0,1)
  orange = rgb(1,0.612,0,1)
  yellow = rgb(0.926,0.580,0.039,1)
  ltgreen = rgb(0.365,1,0,1)
  green = rgb(0.380,0.753,0.314,1)
  ltblue = rgb(0,0.730,1,1)
  mdblue = rgb(0.192,0.384,0.847,1)
  blue = rgb(0.157,0.478,0.878,1)
  dkblue = rgb(0.129,0,0.996,1)
  purple = rgb(0.623,0.369,1,1)
  brown = rgb(0.494,0.271,0.145,1)
  grey75 = rgb(0.75,0.75,0.75,1)
  grey50 = rgb(0.5,0.5,0.5,1)
  grey25 = rgb(0.25,0.25,0.25,1)
  black = rgb(0,0,0,1)
  white = rgb(1,1,1,1)
  
  
  # ==============================================================================
  # CREATE PLOT
  # ==============================================================================
  # (5) Define figure layout
  # Figure dimensions (in inches)
  fig.wd <- 6	  
  fig.ht <- 7
  
  # Font sizes
  lab.size <- .8  # 1.1
  axis.size <- .7 #1
  text.size <- .6
  
  # Device background color
  bg.col <- "white"
  # bg.col <- "black"
  
  # Foreground colors
  fg.col <- axis.col <- lab.col <- main.col <- "black"
  # fg.col <- axis.col <- lab.col <- main.col <- "white"
  
  
  # Line weight
  line.wt <- 1
  line.wts <- c(2,2,2)
  
  # line colors
  colors = c(black,"red",lab.col)
  
  # line types
  linetypes = c(1,1,1,2,2)
  
  
  # PUBLICATION FORMATS -----------------------------------------------
  # Start PDF/TIF/OTHER plotting device
  dpi = 300
  #png("EIoF_Figure_Test.png", units="in", height=fig.ht, width=fig.wd, res=dpi)
  #svg(filename="EIoF_Figure_Test.svg",height=fig.ht, width=fig.wd,pointsize=12) 
  svg(filename="EIoF_Figure_Test.svg",pointsize=12) 

  # set labels for legend
  #leg_labels = c("Food & Energy","Food","Energy")
  
  # (3) Define plot margins
  bmar <- .5+axis.size   # Bottom margin (1)
  lmar <- 3.2-1.2+axis.size # Left margin (2)
  tmar <- 1.50 		# Top margin (3)
  rmar <- 1-1+axis.size   # Right margin (4)
  
  # Define plot margins and axis tick label placement
  par(mar=c(bmar,lmar,tmar,rmar))
  
  # Define background, foreground and object colors
  par(bg=bg.col, fg=fg.col, col.axis=axis.col, col.lab=lab.col, col.main=main.col)
  
  # make the zero value of the x or y-axis to actually be at the
  # left (of x-axis) or bottom (of y-axis) of the plot area
  par(xaxs="i")
  par(yaxs="i")
  
  ## Select the data to plot
  # xdata = solveGEN_output$Hourly_MW_AnnualStorage$Hour_ending[1:7*24]
  # ydata = solveGEN_output$Hourly_MW_AnnualStorage$Load[1:7*24]
  xdata = solveGEN_output$Hourly_MW_AnnualStorage$Hour_ending
  ydata = solveGEN_output$Hourly_MW_AnnualStorage$Load
  
  ## Set figure axis lower and upper limits for plotting DISPLAY, and tick mark increments
  ylimit <- 1.1*max(ydata)
  yrange = c(0,ylimit)
  yinc <- 10000
  #xrange = c(min(xdata),max(xdata))
  xrange = c(0,7*24*4)
  xinc = 24
  
  
  # ==============================================================================
  # CREATE PLOTS
  # ==============================================================================
  # PLOT  1
  i=1
  # test.plot <- plot(xdata,ydata,type="l",xlab="",ylab="",ylim=yrange,xlim=xrange,
  #                   col=colors[i],lty=linetypes[i],lwd=line.wts[i],xaxt="n",yaxt="n")
  test.plot <- plot(seq(1,xrange[2]),ydata,type="l",xlab="",ylab="",ylim=yrange,xlim=xrange,
       col=colors[i],lty=linetypes[i],lwd=line.wts[i],xaxt="n",yaxt="n")
  
  # Add x-axis ticks and title text
  axis(1, seq(xrange[1],xrange[2],xinc), las=1, cex.axis=axis.size*.8,padj=-2.4,tck=-0.03)
  # title(xlab="Hour of Year", cex.lab=lab.size, line=bmar-1)
  
  # Add y-axis ticks and title text
  axis(2,seq(0,ylimit,yinc),lab=paste0(seq(0,ylimit,yinc)),las=1,cex.axis=axis.size,hadj=0.5,tck=-0.03)
  title(ylab="MW Generation", cex.lab=lab.size, line=lmar-1.*lab.size)
  
  # (1) INSERT PLOT TITLE
  title(main="Power Plant Generation per Hour (1 week per season)",cex.main=lab.size,line=tmar-1)#
  dev.off()
  
  ## Save .svg graph as base64 text
  #library(base64) ## see https://cran.r-project.org/web/packages/base64/base64.pdf
  tmp <- tempfile()  ## returns a vector of character strings which can be used as names for temporary files
  # testplot_svg <- base64::encode("EIoF_Figure_Test.svg",tmp,linebreaks = FALSE)
  # file_testplot<-file("testplot_svg.txt")
  # writeLines(read_file(testplot_svg), file_testplot)
  # close(file_testplot)
  testplot_svg2 <- base64enc::base64encode("EIoF_Figure_Test.svg",tmp)
  file_testplot2 <- file("testplot_svg2.txt")
  writeLines(testplot_svg2, file_testplot2)
  close(file_testplot2)
  # 
  # testplot_png <- base64::encode("EIoF_Figure_Test.png",tmp,linebreaks = FALSE)
  # file_testplot<-file("testplot_png.txt")
  # writeLines(read_file(testplot_png), file_testplot)
  # close(file_testplot)
  
  ########################### END MAKE FIGURES ###########################
  
  ########################### START ARRANGE DATA FOR OUTPUT TO WEBSITE ###########################
  
  #all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, solveGEN_output$PPdata_NoStorage, solveGEN_output$PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary)
  # all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, PPdata_NoStorage, PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary)
  # names(all) <- c('sankey_links', 'sankey_nodes', 'Hourly_MW_AnnualStorage', 'Hourly_MW_NoStorage', 'PPdata_NoStorage', 'PPdata_AnnualStorage', 'ggsheets_output_AnnualStorage', 'ggsheets_output_NoStorage', 'website_inputs', 'elec_cost_summary_2050')
  
  ## outputs with the figures
  # all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, PPdata_NoStorage, PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary,paste0(directory_now,"/EIoF_Figure_Test.eps"))
  # all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, PPdata_NoStorage, PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary,paste0(directory_now,"/EIoF_Figure_Test.png"))
  ## all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, PPdata_NoStorage, PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary,read_file(testplot_svg))
  all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, PPdata_NoStorage, PPdata_AnnualStorage, gg_out2, gg_out3,inputs,elec_cost_summary_2050,PrimaryEnergySummary,testplot_svg2)
  names(all) <- c('sankey_links', 'sankey_nodes', 'Hourly_MW_AnnualStorage', 'Hourly_MW_NoStorage', 'PPdata_NoStorage', 'PPdata_AnnualStorage', 'ggsheets_output_AnnualStorage', 'ggsheets_output_NoStorage', 'website_inputs', 'elec_cost_summary_2050', 'PrimaryEnergySummary','Figure1')
  
  
  ########################### END ARRANGE DATA FOR OUTPUT TO WEBSITE ###########################
  
  return(all)
  
  
# }
end_time <- Sys.time()
code_time=end_time - start_time
print(code_time)

## Write "all" to json file
#write_json(all, "eiof_json_TEST.json", pretty = TRUE, auto_unbox = FALSE)
write_json(all, "eiof_json_TXDefault.json", pretty = TRUE, auto_unbox = FALSE)
