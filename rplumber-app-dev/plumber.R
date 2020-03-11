# plumber.R
#library(rjson)
library(jsonlite)
library(readr)

#* @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
  
}


#* Run the EIoF master code to get JSON output
#* @param coal_percent The percent of electricity to get from coal.
#* @param PV_percent The percent of electricity to get from PV.
#* @param CSP_percent The percent of electricity to get from CSP.
#* @param wind_percent The percent of electricity to get from wind.
#* @param biomass_percent The percent of electricity to get from biomass.
#* @param hydro_percent The percent of electricity to get from hydro.
#* @param petroleum_percent The percent of electricity to get from petrol.
#* @param nuclear_percent The percent of electricity to get from nuclear.
#* @param geothermal_percent The percent of electricity to get from geothmal.
#* @param ng_percent The percent of electricity to get from natural gas.
#* @param ldv_e The percent of light-duty vehicle miles to come from electricity.
#* @param r_sh_e The percent of residential heating to get from electric heat pumps.
#* @param r_sh_ng The percent of residential heating to get from natural gas furnaces.
#* @param region_id The region id to consider.
#* @get /eiof
function(region_id = 1, coal_percent = 0, PV_percent = 35, CSP_percent = 0, wind_percent = 25, biomass_percent = 0, hydro_percent = 0, petroleum_percent = 0, nuclear_percent = 10, geothermal_percent = 0, ng_percent = 0, ldv_e = 50, r_sh_e = 50, r_sh_ng = 50){
  
  region_id <- as.integer(as.numeric(region_id))
  coal_percent <- as.integer(as.numeric(coal_percent))
  PV_percent <- as.integer(as.numeric(PV_percent))
  CSP_percent <- as.integer(as.numeric(CSP_percent))
  wind_percent <- as.integer(as.numeric(wind_percent))
  biomass_percent <- as.integer(as.numeric(biomass_percent))
  hydro_percent <- as.integer(as.numeric(hydro_percent))
  petroleum_percent <- as.integer(as.numeric(petroleum_percent))
  nuclear_percent <- as.integer(as.numeric(nuclear_percent))
  geothermal_percent <- as.integer(as.numeric(geothermal_percent))
  ng_percent <- as.integer(as.numeric(ng_percent))
  ldv_e <- as.integer(as.numeric(ldv_e))
  r_sh_e <- as.integer(as.numeric(r_sh_e))
  r_sh_ng <- as.integer(as.numeric(r_sh_e))
  
  source("/scripts/master_EIoF.R")  

  master_EIoF(region_id = region_id, coal_percent = coal_percent, PV_percent = PV_percent, CSP_percent = CSP_percent, wind_percent = wind_percent, biomass_percent = biomass_percent, hydro_percent = hydro_percent, petroleum_percent = petroleum_percent, nuclear_percent = nuclear_percent, geothermal_percent = geothermal_percent, ng_percent = ng_percent, ldv_e = ldv_e, r_sh_e = r_sh_e, r_sh_ng = r_sh_ng)
  
}


