# EIoF_plumber.R

#' Run the EIoF master code to get JSON output
#' @param coal_percent The percent of electricity to get from coal.
#' @param PV_percent The percent of electricity to get from PV.
#' @param CSP_percent The percent of electricity to get from CSP.
#' @param wind_percent The percent of electricity to get from wind.
#' @param biomass_percent The percent of electricity to get from biomass.
#' @param hydro_percent The percent of electricity to get from hydro.
#' @param petroleum_percent The percent of electricity to get from petrol.
#' @param nuclear_percent The percent of electricity to get from nuclear.
#' @param geothermal_percent The percent of electricity to get from geothmal.
#' @param ng_percent The percent of electricity to get from natural gas.
#' @param ldv_e The percent of light-duty vehicle miles to come from electricity.
#' @param r_sh_e The percent of residnetial heating ot get from electricity.
#' @param region_id The region id to consider.
#' @get /echo
function(region_id = 1, coal_percent = 0.0, PV_percent = 0.35, CSP_percent = 0, wind_percent = 0.25, biomass_percent = 0.0, hydro_percent = 0.0, petroleum_percent = 0.0, nuclear_percent = 0.1, geothermal_percent = 0.0, ng_percent = 0.0, ldv_e = 1.0, r_sh_e = 1.0){
  
  region_id <- as.numeric(region_id)
  coal_percent <- as.numeric(coal_percent)
  PV_percent <- as.numeric(PV_percent)
  CSP_percent <- as.numeric(CSP_percent)
  wind_percent <- as.numeric(wind_percent)
  biomass_percent <- as.numeric(biomass_percent)
  hydro_percent <- as.numeric(hydro_percent)
  petroleum_percent <- as.numeric(petroleum_percent)
  nuclear_percent <- as.numeric(nuclear_percent)
  geothermal_percent <- as.numeric(geothermal_percent)
  ng_percent <- as.numeric(ng_percent)
  ldv_e <- as.numeric(ldv_e)
  r_sh_e <- as.numeric(r_sh_e)
  
  source('master_EIoF.R')
  
  master_EIoF(region_id = region_id, coal_percent = coal_percent, PV_percent = PV_percent, CSP_percent = CSP_percent, wind_percent = wind_percent, biomass_percent = biomass_percent, hydro_percent = hydro_percent, petroleum_percent = petroleum_percent, nuclear_percent = nuclear_percent, geothermal_percent = geothermal_percent, ng_percent = ng_percent, ldv_e = ldv_e, r_sh_e = r_sh_e)
  
}