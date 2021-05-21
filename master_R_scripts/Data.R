#Last modified on Oct 2, 2019 by Jianwei Du


#------------------------------------------------#
# sub-function: data_generate                    #
#------------------------------------------------#

data_generate <- function(U, V, Y){
  
  # set efficiency for end use of each sector
  eta_NG_R <- 0.5
  eta_Elec_R <- 1
  eta_R <- 0.65
  eta_NG_C <- 0.5
  eta_Elec_C <- 1
  eta_C <- 0.65
  eta_I <- 0.49
  eta_Elec_T <- 0.8
  eta_NG_T <- 0.5
  eta_Petrol_T <- 0.21
  eta_T <- 0.21
  
  ## new ones
  eta_Ethanol_T <- 0.21
  
  # pre-calculation - V
  ## total energy of each primary energy source
  V["Solar", "Solar_Flow"] <- sum(U["Solar_Flow",])
  V["Nuclear", "Nuclear_Flow"] <- sum(U["Nuclear_Flow",])
  V["Hydro", "Hydro_Flow"] <- sum(U["Hydro_Flow",])
  V["Wind", "Wind_Flow"] <- sum(U["Wind_Flow",])
  V["Geothermal", "Geothermal_Flow"] <- sum(U["Geothermal_Flow",])
  V["Natural_Gas", "NaturalGas_Flow"] <- sum(U["NaturalGas_Flow",])
  V["Coal", "Coal_Flow"] <- sum(U["Coal_Flow",])
  V["Biomass", "Biomass_Flow"] <- sum(U["Biomass_Flow",])
  V["Petroleum", "Petroleum_Flow"] <- sum(U["Petroleum_Flow",])
  
  ## electicity generation of each primary energy "power plants"
  V["Solar_Plant", "Solar_Electricity"] <- U["Solar_Electricity", "Electricity_Grid"]
  V["Nuclear_Plant", "Nuclear_Electricity"] <- U["Nuclear_Electricity", "Electricity_Grid"]
  V["Hydro_Plant", "Hydro_Electricity"] <- U["Hydro_Electricity", "Electricity_Grid"]
  V["Wind_Plant", "Wind_Electricity"] <- U["Wind_Electricity", "Electricity_Grid"]
  V["Geothermal_Plant", "Geothermal_Electricity"] <- U["Geothermal_Electricity", "Electricity_Grid"]
  V["Natural_Gas_Plant", "NaturalGas_Electricity"] <- U["NaturalGas_Electricity", "Electricity_Grid"]
  V["Coal_Plant", "Coal_Electricity"] <- U["Coal_Electricity", "Electricity_Grid"]
  V["Biomass_Plant", "Biomass_Electricity"] <- U["Biomass_Electricity", "Electricity_Grid"]
  V["Petroleum_Plant", "Petroleum_Electricity"] <- U["Petroleum_Electricity", "Electricity_Grid"]
  V["Import_Net", "Import_Net_Electricity"] <- U["Import_Net_Electricity", "Electricity_Grid"]
  V["Electricity_Grid", "Electricity_Flow"] <- sum(U[, "Electricity_Grid"])
  
  ## end use services
  V["Resident_SpaceHeating_NG", "Services_Resident_SpaceHeating_NG"] <- eta_NG_R*U["NaturalGas_Flow", "Resident_SpaceHeating_NG"]
  V["Resident_SpaceHeating_Elec", "Services_Resident_SpaceHeating_Elec"] <- eta_Elec_R*U["Electricity_Flow", "Resident_SpaceHeating_Elec"]
  V["Resident_WaterHeating_NG", "Services_Resident_WaterHeating_NG"] <- eta_NG_R*U["NaturalGas_Flow", "Resident_WaterHeating_NG"]
  V["Resident_WaterHeating_Elec", "Services_Resident_WaterHeating_Elec"] <- eta_Elec_R*U["Electricity_Flow", "Resident_WaterHeating_Elec"]
  V["Resident_Cooking_NG", "Services_Resident_Cooking_NG"] <- eta_NG_R*U["NaturalGas_Flow", "Resident_Cooking_NG"]
  V["Resident_Cooking_Elec", "Services_Resident_Cooking_Elec"] <- eta_Elec_R*U["Electricity_Flow", "Resident_Cooking_Elec"]
  V["Resident_Other", "Services_Resident_Other"] <- eta_R*sum(U[,"Resident_Other"])
  
  V["Commerce_SpaceHeating_NG", "Services_Commerce_SpaceHeating_NG"] <- eta_NG_C*U["NaturalGas_Flow", "Commerce_SpaceHeating_NG"]
  V["Commerce_SpaceHeating_Elec", "Services_Commerce_SpaceHeating_Elec"] <- eta_Elec_C*U["Electricity_Flow", "Commerce_SpaceHeating_Elec"]
  V["Commerce_WaterHeating_NG", "Services_Commerce_WaterHeating_NG"] <- eta_NG_C*U["NaturalGas_Flow", "Commerce_WaterHeating_NG"]
  V["Commerce_WaterHeating_Elec", "Services_Commerce_WaterHeating_Elec"] <- eta_Elec_C*U["Electricity_Flow", "Commerce_WaterHeating_Elec"]
  V["Commerce_Cooking_NG", "Services_Commerce_Cooking_NG"] <- eta_NG_C*U["NaturalGas_Flow", "Commerce_Cooking_NG"]
  V["Commerce_Cooking_Elec", "Services_Commerce_Cooking_Elec"] <- eta_Elec_C*U["Electricity_Flow", "Commerce_Cooking_Elec"]
  V["Commerce_Other", "Services_Commerce_Other"] <- eta_C*sum(U[,"Commerce_Other"])
  
  V["Industrial", "Services_Industrial"] <- eta_I*sum(U[,"Industrial"])
  
  V["Transport_LDV_Petrol", "Services_Transport_LDV_Petrol"] <- eta_Petrol_T*U["Petroleum_Flow", "Transport_LDV_Petrol"]
  V["Transport_LDV_Elec", "Services_Transport_LDV_Elec"] <- eta_Elec_T*U["Electricity_Flow", "Transport_LDV_Elec"]
  V["Transport_LDV_Ethanol", "Services_Transport_LDV_Ethanol"] <- eta_Ethanol_T*U["Biomass_Flow", "Transport_LDV_Ethanol"]
  V["Transport_Other_NG", "Services_Transport_Other_NG"] <- eta_NG_T*U["NaturalGas_Flow", "Transport_Other_NG"]
  V["Transport_Other_Petrol", "Services_Transport_Other_Petrol"] <- eta_Petrol_T*U["Petroleum_Flow", "Transport_Other_Petrol"]
  
  ## what to include in other
  V["Transport_Other_Other", "Services_Transport_Other_Other"] <- eta_Elec_T*sum(U["Electricity_Flow","Transport_Other_Other"])
  
  # pre-calculation - Y
  ##Y[21:41,1] <- diag(V[21:41,21:41])
  Y["Services_Resident_SpaceHeating_NG", "Energy_Services"] <- V["Resident_SpaceHeating_NG", "Services_Resident_SpaceHeating_NG"]
  Y["Services_Resident_SpaceHeating_Elec", "Energy_Services"] <- V["Resident_SpaceHeating_Elec", "Services_Resident_SpaceHeating_Elec"]
  Y["Services_Resident_WaterHeating_NG", "Energy_Services"] <- V["Resident_WaterHeating_NG", "Services_Resident_WaterHeating_NG"]
  Y["Services_Resident_WaterHeating_Elec", "Energy_Services"] <- V["Resident_WaterHeating_Elec", "Services_Resident_WaterHeating_Elec"]
  Y["Services_Resident_Cooking_NG", "Energy_Services"] <- V["Resident_Cooking_NG", "Services_Resident_Cooking_NG"]
  Y["Services_Resident_Cooking_Elec", "Energy_Services"] <- V["Resident_Cooking_Elec", "Services_Resident_Cooking_Elec"]
  Y["Services_Resident_Other", "Energy_Services"] <- V["Resident_Other", "Services_Resident_Other"]
  
  Y["Services_Commerce_SpaceHeating_NG", "Energy_Services"] <- V["Commerce_SpaceHeating_NG", "Services_Commerce_SpaceHeating_NG"]
  Y["Services_Commerce_SpaceHeating_Elec", "Energy_Services"] <- V["Commerce_SpaceHeating_Elec", "Services_Commerce_SpaceHeating_Elec"]
  Y["Services_Commerce_WaterHeating_NG", "Energy_Services"] <- V["Commerce_WaterHeating_NG", "Services_Commerce_WaterHeating_NG"]
  Y["Services_Commerce_WaterHeating_Elec", "Energy_Services"] <- V["Commerce_WaterHeating_Elec", "Services_Commerce_WaterHeating_Elec"]
  Y["Services_Commerce_Cooking_NG", "Energy_Services"] <- V["Commerce_Cooking_NG", "Services_Commerce_Cooking_NG"]
  Y["Services_Commerce_Cooking_Elec", "Energy_Services"] <- V["Commerce_Cooking_Elec", "Services_Commerce_Cooking_Elec"]
  Y["Services_Commerce_Other", "Energy_Services"] <- V["Commerce_Other", "Services_Commerce_Other"]
  
  Y["Services_Industrial", "Energy_Services"] <- V["Industrial", "Services_Industrial"]
  
  Y["Services_Transport_LDV_Petrol", "Energy_Services"] <- V["Transport_LDV_Petrol", "Services_Transport_LDV_Petrol"]
  Y["Services_Transport_LDV_Elec", "Energy_Services"] <- V["Transport_LDV_Elec", "Services_Transport_LDV_Elec"]
  Y["Services_Transport_LDV_Ethanol", "Energy_Services"] <- V["Transport_LDV_Ethanol", "Services_Transport_LDV_Ethanol"]
  Y["Services_Transport_Other_Petrol", "Energy_Services"] <- V["Transport_Other_Petrol", "Services_Transport_Other_Petrol"]
  Y["Services_Transport_Other_NG", "Energy_Services"] <- V["Transport_Other_NG", "Services_Transport_Other_NG"]
  ## JDR added
  Y["Services_Transport_Other_Other", "Energy_Services"] <- V["Transport_Other_Other", "Services_Transport_Other_Other"]
  
  # generate list of U/V/Y
  result <- list('U' = U, 'V' = V, 'Y' = Y)
  
  return(result)

}