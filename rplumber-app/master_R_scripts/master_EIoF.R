## This function will be the One EIoF function to rule them all, One function to source them, 
## One function to bring them all and in the virtual machine bind them.

## This function will act as the main function to be used via Plumber in the TACC VM.
## It waits for a URL API from the UX website and calls all of the pieces of the EIoF
## to generate and return a JSON file to teh website for plotting.

## master_EIoF_function.R 
## Joshua D. Rhodes, PhD
## 2019-04-30

setwd('/scripts')

## the inputs from the website website API URL GET (I have not idea) call will be:
## 1) Region considered (between 1 and 13, inclusive)
## 2) End-uses that we allow the user to change
##    - home heating energy use (natural gas, electricity, biomass/other)
##    - light-duty vehicle energy use (petroleum vs. electricity)
## 3) Percent electricity generation from primary fuels

master_EIoF <- function(region_id = 1, coal_percent = 0, PV_percent = 35, CSP_percent = 0, wind_percent = 25, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 50, r_sh_e = 50){
  
  
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
  'r_sh_e' = r_sh_e
  )))
  names(inputs) <- 'web_inputs'
  
  
  
  
  ##############################################################
  ##### This might could be run outside of this function? ######
  ##############################################################
  
  ## load up any needed libraries
  library(lubridate)
  library(jsonlite)
  library(readr)
  
  ## pre calcualtions from inputs
  ng_percent = 100 - as.integer((coal_percent + PV_percent + CSP_percent + wind_percent + biomass_percent + hydro_percent + petroleum_percent + nuclear_percent + geothermal_percent))
  
  
  
  ## source all the below functions
  
  ##############################################################
  
  ## Start calling functions that create the input data for the calculations 
  
  ## call Jianwei's code (Sankey.R) to edit the Sankey based on user input
  ## inputs: end use changes, percent electricity generation from primary fuels, region
  ## outputs : JSON file with new values used to create Sankey
  ########################### BEGIN Jianwei Sankey Code ###########################
  
  source('Sankey_Function_JDR.R')
  
  sankey_json_out <- sankey_json(region_id = region_id, p_solar = PV_percent, p_nuclear = nuclear_percent, p_hydro = hydro_percent, p_wind = wind_percent, p_geo = geothermal_percent, p_ng = ng_percent, p_coal = coal_percent, p_bio = biomass_percent, p_petrol = petroleum_percent, r_sh_e = 100, r_wh_e = 100, r_ck_e = 100, c_sh_e = 100, c_wh_e = 100, c_ck_e = 100, ldv_elec = 50, ldv_petrol = 30, ldv_ethanol = 20, trans_other_petrol = 40, trans_other_ng = 10, trans_other_other = 50)
  

  ########################### END Jianwei Sankey Code ###########################
  
  
  ########################### BEGIN generate8760 Code ###########################
  
  ldv_elec_quads <- sankey_json_out$links$Value[sankey_json_out$links$From == 'Electricity' & sankey_json_out$links$To == 'Transportation']
  
  #print(paste('LDV quads:', ldv_quads))
  
  source('generate8760.R')
  ev_charging_profile <- generate8760(ldv_quads = ldv_elec_quads)
  
  ########################### END generate8760 Code ###########################
  
  
  ## call Carey's solveGEN code to get needed power plant capacities
  ## inputs: 8760 hour profiles of electricity (all uses) demand, percent electricity generation from primary fuels, region
  ## outputs: capacities needed of each type of power plant to realize percent electricity generation from primary fuels, 8760 generation curves from each type of power plant 
  ########################### BEGIN CAREY solveGEN ########################### 
  
  #Notes:
    # code has been edited to add EV charging profile to Carey's code

  #start_time <- Sys.time()  ## This is just to know how long it took to run the code
  
  require(Rcgmin)  ## pakcage for gradient based optimization
  require(numDeriv)## Allows calculation of numerical derivative of a function within an evaluation function
  require(optimr)  ## pakcage for gradient based optimization
  
#  wd <- getwd()
#  setwd("solveGEN_20190509")
  source("solveGEN.R")
  

  year = 2016  ## This is the year of input data to use for 8760 hour profiles of load, wind, and PV output.
  
  # get values 0-1 for solveGen

  
  ## Note: The fraction of electric power from natural gas combined cycle and natural gas combustion turbines
  ## is assumed to be any generation not otherwise specified in the inputs. Thus, there is no input of 
  ## "natural_gas_percent" to solveGEN.r.
  RegionNumber = region_id  ## Specify the region number to calculate (there are 13 defined U.S. regions)
  if ( (RegionNumber >= 1) & (RegionNumber <= 13)){
    solveGEN_output <- solveGEN(RegionNumber,year,coal_percent, PV_percent, CSP_percent, wind_percent, nuclear_percent, hydro_percent, biomass_percent, geothermal_percent, petroleum_percent, ev_charging_profile)
  } else {
    stop("You have not selected a valid RegionNumber to call solveGEN.r.")
  } 
  
  #end_time <- Sys.time()
  #code_time=end_time - start_time
  #print(code_time)
  
#  setwd(wd)
  
  #return(solveGEN_output)
  
  ########################### END CAREY solveGEN ###########################
  
  ########################### BEGIN JOSH GOGGLESHEET ACCESS ###########################
  
  source('EIoF_gs_function.R')
  
  ## inputs are the same % inputs as to Carey's solveGEN above
  
  gg_out <- EIoF_gs_function(Coal = coal_percent, Nuclear =  nuclear_percent,	Natural_Gas =  ng_percent,	Hydro =  hydro_percent, Solar =  PV_percent, Wind =  wind_percent, Geothermal =  geothermal_percent, MSW =  biomass_percent/2, Other_biomass =  biomass_percent/2,	Other =  0, Petroleum =  petroleum_percent)

 # print(paste('New 2050 CAPEX is: ', gg_out, sep = ''))
  
  # add 
  
  
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
  
  
  
  all <- list(sankey_json_out$links, sankey_json_out$nodes, solveGEN_output$Hourly_MW_AnnualStorage, solveGEN_output$Hourly_MW_NoStorage, solveGEN_output$PPdata_NoStorage, solveGEN_output$PPdata_AnnualStorage, gg_out, inputs)
#  EIOF_no_timeseries <- list(sankey_json_out$links, sankey_json_out$nodes, gg_out)
#  EIOF_timeseries <- list(solveGEN_output$Hourly_MW_AnnualStorage)
  
  names(all) <- c('sankey_links', 'sankey_nodes', 'Hourly_MW_AnnualStorage', 'Hourly_MW_NoStorage', 'PPdata_NoStorage', 'PPdata_AnnualStorage', 'ggsheets_output', 'website_inputs')
   
#  write_lines(toJSON(all), 'all_EIOF_data.json')
#  write_lines(toJSON(EIOF_no_timeseries), 'EIOF_no_timeseries_data.json')
#  write_lines(toJSON(EIOF_timeseries), 'EIOF_timeseries_data.json')
  
  return(all)
  
  
}