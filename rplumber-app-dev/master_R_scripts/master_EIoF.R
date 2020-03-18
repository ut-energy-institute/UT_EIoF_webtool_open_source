## This function will be the One EIoF function to rule them all, One function to source them, 
## One function to bring them all and in the virtual machine bind them.

## This function will act as the main function to be used via Plumber in the TACC VM.
## It waits for a URL API from the UX website and calls all of the pieces of the EIoF
## to generate and return a JSON file to teh website for plotting.

## master_EIoF.R function 
## Carey W. King, PhD
## 2020-03-11

setwd('/scripts')

## the inputs from the website website API URL GET (I have not idea) call will be:
## 1) Region considered (between 1 and 13, inclusive)
## 2) End-uses that we allow the user to change
##    - home heating energy use (natural gas, electricity, biomass/other)
##    - light-duty vehicle energy use (petroleum vs. electricity)
## 3) Percent electricity generation from primary fuels

## Use this to call the function after it is sourced as a function:
# start_time <- Sys.time()  ## This is just to know how long it took to run the code
# eiof_out <- master_EIoF(region_id = 6, coal_percent = 10, PV_percent = 10, CSP_percent = 0, wind_percent = 10, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 20, r_sh_e = 0, r_sh_ng = 100)
# end_time <- Sys.time()
# code_time=end_time - start_time
# print(code_time)
  
master_EIoF <- function(region_id = 1, coal_percent = 10, PV_percent = 15, CSP_percent = 0, wind_percent = 15, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 50, r_sh_e = 50, r_sh_ng = 50){

  # to make testing easier
  # region_id = 6
  # coal_percent = 20
  # PV_percent = 0
  # CSP_percent = 0
  # wind_percent = 10
  # biomass_percent = 0
  # hydro_percent = 0
  # petroleum_percent = 0
  # nuclear_percent = 0
  # geothermal_percent = 0
  # ng_percent = 0
  # ldv_e = 50
  # r_sh_e = 0
  # r_sh_ng = 100

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
  cat("LDVmiles_per_region_2050 needs to be specified before calling 'generate8760.R' *(it is an input).",sep="\n")
  fraction_LDVmiles_per_region_2050 = (1/13)*rep(1,13)
  LDVmiles_per_region_2050 <- data.frame(0)
  assign("EIA_AEO2019_LDVmiles_2050",3472.650879*1e9)  ## miles driven all light duty vehicles (using liquid fuels) in 2050; value from AEO Ref. 2019 for U.S. is 3472.65087 billion miles
  for (i in 1:13) {
    LDVmiles_per_region_2050[i] = fraction_LDVmiles_per_region_2050[i]*EIA_AEO2019_LDVmiles_2050
  }
  region_names <- c("R1_NW","R2_CA", "R3_MN", "R4_SW", "R5_CE", "R6_TX", "R7_MW", "R8_AL", "R9_MA", "R10_SE", "R11_FL", "R12_NY", "R13_NE")  ## specify the region names as they appear as the column names of the "8760" input profiles
  names(LDVmiles_per_region_2050) <- c(region_names)
  
  source("generate8760.R")
  #generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV)
  generate8760_output <- generate8760(RegionNumber,year,percent_ResidentialHeatPump,percent_ResidentialNG,percent_ElectricLDV,LDVmiles_per_region_2050[RegionNumber])

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
  generate_FinalUVY_2050_output <- generate_FinalUVY_2050(RegionNumber = region_id, percent_ResidentialHeatPump = percent_ResidentialHeatPump, percent_ResidentialNG = percent_ResidentialNG, Hourly_MW_NoStorage,Hourly_MW_AnnualStorage,PPdata_NoStorage,PPdata_AnnualStorage,percent_ElectricLDV,LDVmiles_per_region_2050[RegionNumber],Total_AnnualMWh_LDV_EVs)
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
  cat(paste0("Need to add electric ldv miles per kwh variation per region and/or temperature."),sep="\n")
  #ldv_petrol = 30
  #ldv_ethanol = 20
  ldv_totalquads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Petrol"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Elec"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Ethanol"])
  ldv_petrol_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Petrol"])
  ldv_elec_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Elec"])
  ldv_biofuel_quads = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_LDV_Ethanol"])
  percent_ldv_elec_quads = 100*ldv_elec_quads/ldv_totalquads
  percent_ldv_petrol_quads = 100*ldv_petrol_quads/ldv_totalquads
  percent_ldv_biofuel_quads = 100*ldv_biofuel_quads/ldv_totalquads
  
  ## Other (non-LDV transportation)
  #trans_other_petrol = 80
  #trans_other_ng = 10
  trans_other_totalenergy = sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_NG"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Petrol"]) + sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Other"])
  if (trans_other_totalenergy>0){
    trans_other_petrol = 100*sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_Petrol"])/trans_other_totalenergy
    trans_other_ng = 100*sum(generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion[,"Transport_Other_NG"])/trans_other_totalenergy
    trans_other_other = 100 - trans_other_petrol - trans_other_ng
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
  sankey_json_out <- sankey_json(region_id = region_id, p_solar = solar_percent, p_nuclear = nuclear_percent, p_hydro = hydro_percent, p_wind = wind_percent, p_geo = geothermal_percent, p_ng = ng_percent, p_coal = coal_percent, p_bio = biomass_percent, p_petrol = petroleum_percent, r_sh_e = r_sh_e, r_sh_ng = r_sh_ng, r_wh_e = r_wh_e, r_wh_ng = r_wh_ng, r_ck_e = r_ck_e, r_ck_ng = r_ck_ng, c_sh_e = c_sh_e, c_sh_ng = c_sh_ng, c_wh_e = c_wh_e, c_wh_ng = c_wh_ng, c_ck_e = c_ck_e, c_ck_ng = c_ck_ng, ldv_elec = percent_ldv_elec_quads, ldv_petrol = percent_ldv_petrol_quads, ldv_ethanol = percent_ldv_biofuel_quads, trans_other_petrol = trans_other_petrol, trans_other_ng = trans_other_ng, trans_other_other = trans_other_other,generate_FinalUVY_2050_output$U_NoStorage_2050_CurrentRegion,generate_FinalUVY_2050_output$V_NoStorage_2050_CurrentRegion,Y_template)

  ## Call Sankey function with "AnnualStorage" results of "solveGEN" and "generate_FinalUVY_2050"
  # sankey_json_out_AnnualStorage <- sankey_json(region_id = region_id, p_solar = solar_percent, p_nuclear = nuclear_percent, p_hydro = hydro_percent, p_wind = wind_percent, p_geo = geothermal_percent, p_ng = ng_percent, p_coal = coal_percent, p_bio = biomass_percent, p_petrol = petroleum_percent, r_sh_e = r_sh_e, r_sh_ng = r_sh_ng, r_wh_e = r_wh_e, r_wh_ng = r_wh_ng, r_ck_e = r_ck_e, r_ck_ng = r_ck_ng, c_sh_e = c_sh_e, c_sh_ng = c_sh_ng, c_wh_e = c_wh_e, c_wh_ng = c_wh_ng, c_ck_e = c_ck_e, c_ck_ng = c_ck_ng, ldv_elec = percent_ldv_elec_quads, ldv_petrol = percent_ldv_petrol_quads, ldv_ethanol = percent_ldv_biofuel_quads, trans_other_petrol = trans_other_petrol, trans_other_ng = trans_other_ng, trans_other_other = trans_other_other,generate_FinalUVY_2050_output$U_AnnualStorage_2050_CurrentRegion,generate_FinalUVY_2050_output$V_AnnualStorage_2050_CurrentRegion,Y_template)
  
  ########################### END Jianwei Sankey Code ###########################
  

  ########################### BEGIN JOSH GOGGLESHEET ACCESS ###########################
  
  source('EIoF_gs_function.R')
  
  ## massage outputs from solveGEN to go into the googlesheets code for "Annual Storage" solvGEN outputs
  SG_out_AnnualStorage <- solveGEN_output$PPdata_AnnualStorage[c('Technology', 'MW_needed', 'TWhGeneration')]
  target <- c("Coal", "Nuclear", "NGCC", "NGCT", "HydroDispatch", "PV", "Wind", "Geothermal", "Biomass", "Other", "PetroleumCC", "AnnualStorage_Total")
  SG_out_AnnualStorage <- SG_out_AnnualStorage[match(target, SG_out_AnnualStorage$Technology),]
  SG_out_AnnualStorage$Technology <- target
  SG_out_AnnualStorage[is.na(SG_out_AnnualStorage)] <- 0
  gg_out_AnnualStorage <- EIoF_gs_function(SG_out = SG_out_AnnualStorage)
  gg_out_AnnualStorage <- as.data.frame(gg_out_AnnualStorage[,-1])
  
  ## massage outputs from solveGEN to go into the googlesheets code for "No Storage" solvGEN outputs
  SG_out_NoStorage <- solveGEN_output$PPdata_NoStorage[c('Technology', 'MW_needed', 'TWhGeneration')]
  target <- c("Coal", "Nuclear", "NGCC", "NGCT", "HydroDispatch", "PV", "Wind", "Geothermal", "Biomass", "Other", "PetroleumCC", "AnnualStorage_Total")
  SG_out_NoStorage <- SG_out_NoStorage[match(target, SG_out_NoStorage$Technology),]
  SG_out_NoStorage$Technology <- target
  SG_out_NoStorage[is.na(SG_out_NoStorage)] <- 0
  gg_out_NoStorage <- EIoF_gs_function(SG_out = SG_out_NoStorage)
  gg_out_NoStorage <- as.data.frame(gg_out_NoStorage[,-1])
  
  
  ## Rearrange Google Sheet output data
  gs_auth(token = "googlesheets_token.rds") ## Authorize acess to the Google Sheet
  gg_out2 <- lapply(split(gg_out_AnnualStorage, gg_out_AnnualStorage$type, drop = TRUE), function(x) split(x, x[['value']], drop = TRUE))
  gg_out3 <- lapply(split(gg_out_NoStorage, gg_out_NoStorage$type, drop = TRUE), function(x) split(x, x[['value']], drop = TRUE))
  # print(paste('New 2050 CAPEX is: ', gg_out, sep = ''))

  ## call (Josh's?) code to ping GG's Googlesheets to get information about capital and cash flow
  ## inputs: capacities needed of each type of power plant, MWh from Carey's code, region
  ## outputs: capacity and cashflow numbers to get there by 2050
  
  # Region number
  # Power plant data
    # Capacity (MW) for each type of power plant in 2015
    # Generation (MWh) from each power plant in 2050
  # Storage data
    # Capacity (MW) of storage in 2050
    # Caapcity (MWh) of storage in 2050
  
  
  ########################### END JOSH GOGGLESHEET ACCESS ###########################
  
  
  all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, solveGEN_output$PPdata_NoStorage, solveGEN_output$PPdata_AnnualStorage, gg_out2, gg_out3,inputs)
  #  EIOF_no_timeseries <- list(sankey_json_out$links, sankey_json_out$nodes, gg_out)
  #  EIOF_timeseries <- list(solveGEN_output$Hourly_MW_AnnualStorage)
  
  names(all) <- c('sankey_links', 'sankey_nodes', 'Hourly_MW_AnnualStorage', 'Hourly_MW_NoStorage', 'PPdata_NoStorage', 'PPdata_AnnualStorage', 'ggsheets_output_AnnualStorage', 'ggsheets_output_NoStorage', 'website_inputs')
  
#  write_lines(toJSON(all), 'all_EIOF_data.json')
#  write_lines(toJSON(EIOF_no_timeseries), 'EIOF_no_timeseries_data.json')
#  write_lines(toJSON(EIOF_timeseries), 'EIOF_timeseries_data.json')
  
  return(all)
  
  
}