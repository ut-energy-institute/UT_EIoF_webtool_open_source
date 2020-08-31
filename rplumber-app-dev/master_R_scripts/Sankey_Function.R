#Last modified on Aug 30, 2019 by Jianwei Du
#rm(list = ls(all = TRUE))


#------------------------------------------------#
# Initialization                                 #
#------------------------------------------------#

# load libraries
library(jsonlite)   ## for JSON
library(matsbyname) ## Matt Huen's package for matrix operation
library(matsindf)   ## Matt Huen's package for matrix operation
library(Recca)      ## Matt Huen's package for matrix operation
library(magrittr)   ## for pipe operation
library(tibble)
library(dplyr)      ## for data reshape

# load sub-functions
source('Data.R')    ## for precalculation for U/V/Y matrices
source('ELNL.R')    ## for edge_list & node_list generation

# define region set
region <- 1:13

#------------------------------------------------#
# Main Function: sankey_json                     #
#------------------------------------------------#
sankey_json <- function(region_id, p_solar, p_nuclear, p_hydro, p_wind, p_geo, p_ng, p_coal, p_bio, p_petrol, r_sh_e, r_sh_ng, r_wh_e, r_wh_ng, r_ck_e, r_ck_ng, c_sh_e, c_sh_ng, c_wh_e, c_wh_ng, c_ck_e, c_ck_ng, ldv_elec, ldv_petrol, ldv_ethanol, trans_other_petrol, trans_other_ng, trans_other_other,U,V,Y){
  
  # error message - region_id; sum of percentages equal to 100
  if (!(as.numeric(region_id) %in% region)){
    stop("Wrong region_id in Sankey_Function.R")
  } else if (sum(as.numeric(p_solar), as.numeric(p_nuclear), as.numeric(p_hydro), as.numeric(p_wind), as.numeric(p_geo), as.numeric(p_ng), as.numeric(p_coal), as.numeric(p_bio), as.numeric(p_petrol)) != 100){
    stop("Total proportion of different Electricity parts unequal to 1 in Sankey_Function.R")
  } else if (sum(as.numeric(ldv_elec), as.numeric(ldv_petrol), as.numeric(ldv_ethanol)) != 100){
    stop("LDV fuel does not add to 100% in Sankey_Function.R")
  } else {
    
    # read data - for now, only {1,2} for U/Y; V is just the same for all regions
    #U <- read.csv(paste0("./EIoFRegion_2050Sankey_Input_Files_20191024_NumberLabels/U", region_id, ".csv"), row.names = 1) %>% as.matrix() %>% setrowtype("Products") %>% setcoltype("Industries")
    #V <- read.csv(paste0("./EIoFRegion_2050Sankey_Input_Files_20191024_NumberLabels/V", region_id, ".csv"), row.names = 1) %>% as.matrix() %>% setrowtype("Industries") %>% setcoltype("Products")
    #Y <- read.csv(paste0("./EIoFRegion_2050Sankey_Input_Files_20191024_NumberLabels/Y", region_id, ".csv"), row.names = 1) %>% as.matrix() %>% setrowtype("Products") %>% setcoltype("Industries")
    # U <- read.csv(paste0("U", 1, ".csv"), row.names = 1) %>% as.matrix() %>% setrowtype("Products") %>% setcoltype("Industries")
    # V <- read.csv(paste0("V", 1, ".csv"), row.names = 1) %>% as.matrix() %>% setrowtype("Industries") %>% setcoltype("Products")
    # Y <- read.csv(paste0("Y", 1, ".csv"), row.names = 1) %>% as.matrix() %>% setrowtype("Products") %>% setcoltype("Industries")
    U <- U %>% as.matrix() %>% setrowtype("Products") %>% setcoltype("Industries")
    V <- V %>% as.matrix() %>% setrowtype("Industries") %>% setcoltype("Products")
    Y <- Y %>%  as.matrix() %>% setrowtype("Products") %>% setcoltype("Industries")
    U <- U/1e15 # convert to QUADS
    V <- V/1e15 # convert to QUADS
    Y <- Y/1e15 # convert to QUADS
    
    # pre-calculation - U, V, Y
    U <- data_generate(U, V, Y)[["U"]]
    V <- data_generate(U, V, Y)[["V"]]
    Y <- data_generate(U, V, Y)[["Y"]]
    
    # pre-calculation - constants
    ## electricity fraction constants
    Electricity <- V["Electricity_Grid", "Electricity_Flow"]
    v_solar <- round(V["Solar_Plant","Solar_Electricity"]/Electricity*100)
    v_nuclear <- round(V["Nuclear_Plant","Nuclear_Electricity"]/Electricity*100)
    v_hydro <- round(V["Hydro_Plant","Hydro_Electricity"]/Electricity*100)
    v_wind <- round(V["Wind_Plant","Wind_Electricity"]/Electricity*100)
    v_geo <- round(V["Geothermal_Plant","Geothermal_Electricity"]/Electricity*100)
    v_ng <- round(V["Natural_Gas_Plant","NaturalGas_Electricity"]/Electricity*100)
    v_coal <- round(V["Coal_Plant","Coal_Electricity"]/Electricity*100)
    v_bio <- round(V["Biomass_Plant","Biomass_Electricity"]/Electricity*100)
    v_petrol <- 100 - sum(v_solar, v_nuclear, v_hydro, v_wind, v_geo, v_ng, v_coal, v_bio)
    ## In case rounding is affecting the answer, I think we are only using rounding here because the web interface is assumign user inputs only come in increments of whole percentages (e.g., 10%, 11%, 12%, etc.) without decimals (e.g., the web user cannot enter 12.3%)
    if (v_petrol < 0) {
      v_ng = v_ng + v_petrol  ## This adds any negative value of "v_petrol" to adjust "v_ng" as default adjustment.
      v_petrol = 0
    }
    
    ## end use service constants
    Service_Resident_SpaceHeating <- Y["Services_Resident_SpaceHeating_NG", "Energy_Services"] + Y["Services_Resident_SpaceHeating_Elec", "Energy_Services"]
    cat(paste0("I need to determine how to fully/properly add `other' heating fuels into resident SpaceHeating."),sep="\n")
    Service_Resident_WaterHeating <- Y["Services_Resident_WaterHeating_NG", "Energy_Services"] + Y["Services_Resident_WaterHeating_Elec", "Energy_Services"]
    Service_Resident_Cooking <- Y["Services_Resident_Cooking_NG", "Energy_Services"] + Y["Services_Resident_Cooking_Elec", "Energy_Services"]
    Service_Commerce_SpaceHeating <- Y["Services_Commerce_SpaceHeating_NG", "Energy_Services"] + Y["Services_Commerce_SpaceHeating_Elec", "Energy_Services"]
    Service_Commerce_WaterHeating <- Y["Services_Commerce_WaterHeating_NG", "Energy_Services"] + Y["Services_Commerce_WaterHeating_Elec", "Energy_Services"]
    Service_Commerce_Cooking <- Y["Services_Commerce_Cooking_NG", "Energy_Services"] + Y["Services_Commerce_Cooking_Elec", "Energy_Services"]
    ## JDR edit added ethanol
    Service_LDV <- Y["Services_Transport_LDV_Petrol", "Energy_Services"] + Y["Services_Transport_LDV_Elec", "Energy_Services"] + Y["Services_Transport_LDV_Ethanol", "Energy_Services"]
    ## JDR edit swapped FT for Other
    Service_Other_Transport <- Y["Services_Transport_Other_Other", "Energy_Services"] + Y["Services_Transport_Other_Petrol", "Energy_Services"] + Y["Services_Transport_Other_NG", "Energy_Services"]
    ## CWK EDIT 8/12/20
    Service_Resident_Other <- Y["Services_Resident_Other", "Energy_Services"]
    Service_Commerce_Other <- Y["Services_Commerce_Other", "Energy_Services"]
    Service_Industrial <- Y["Services_Industrial", "Energy_Services"]

    # pre-calculation - io_mats
    io_mats <- calc_io_mats(U = U, V = V, Y = Y, S_units = NULL)
    
    # calculation - Y_Prime
    Y_prime <- Y
    #Y_prime["Services_Resident_SpaceHeating_NG", "Energy_Services"] <- Service_Resident_SpaceHeating*(1 - as.numeric(r_sh_e)/100)
    Y_prime["Services_Resident_SpaceHeating_NG", "Energy_Services"] <- Service_Resident_SpaceHeating*as.numeric(r_sh_ng)/100
    Y_prime["Services_Resident_SpaceHeating_Elec", "Energy_Services"] <- Service_Resident_SpaceHeating*as.numeric(r_sh_e)/100
    #Y_prime["Services_Resident_WaterHeating_NG", "Energy_Services"] <- Service_Resident_WaterHeating*(1 - as.numeric(r_wh_e)/100)
    Y_prime["Services_Resident_WaterHeating_NG", "Energy_Services"] <- Service_Resident_WaterHeating*as.numeric(r_wh_ng)/100
    Y_prime["Services_Resident_WaterHeating_Elec", "Energy_Services"] <- Service_Resident_WaterHeating*as.numeric(r_wh_e)/100
    #Y_prime["Services_Resident_Cooking_NG", "Energy_Services"] <- Service_Resident_Cooking*(1 - as.numeric(r_ck_e)/100)
    Y_prime["Services_Resident_Cooking_NG", "Energy_Services"] <- Service_Resident_Cooking*as.numeric(r_ck_ng)/100
    Y_prime["Services_Resident_Cooking_Elec", "Energy_Services"] <- Service_Resident_Cooking*as.numeric(r_ck_e)/100
    #Y_prime["Services_Commerce_SpaceHeating_NG", "Energy_Services"] <- Service_Commerce_SpaceHeating*(1 - as.numeric(c_sh_e)/100)
    Y_prime["Services_Commerce_SpaceHeating_NG", "Energy_Services"] <- Service_Commerce_SpaceHeating*as.numeric(c_sh_ng)/100
    Y_prime["Services_Commerce_SpaceHeating_Elec", "Energy_Services"] <- Service_Commerce_SpaceHeating*as.numeric(c_sh_e)/100
    #Y_prime["Services_Commerce_WaterHeating_NG", "Energy_Services"] <- Service_Commerce_WaterHeating*(1 - as.numeric(c_wh_e)/100)
    Y_prime["Services_Commerce_WaterHeating_NG", "Energy_Services"] <- Service_Commerce_WaterHeating*as.numeric(c_wh_ng)/100
    Y_prime["Services_Commerce_WaterHeating_Elec", "Energy_Services"] <- Service_Commerce_WaterHeating*as.numeric(c_wh_e)/100
    #Y_prime["Services_Commerce_Cooking_NG", "Energy_Services"] <- Service_Commerce_Cooking*(1 - as.numeric(c_ck_e)/100)
    Y_prime["Services_Commerce_Cooking_NG", "Energy_Services"] <- Service_Commerce_Cooking*as.numeric(c_ck_ng)/100
    Y_prime["Services_Commerce_Cooking_Elec", "Energy_Services"] <- Service_Commerce_Cooking*as.numeric(c_ck_e)/100
    
    ## CWK EDIT 8/12/20
    # Y_prime["Services_Resident_Other", "Energy_Services"] <- Service_Resident_Other/100
    # Y_prime["Services_Commerce_Other", "Energy_Services"] <- Service_Commerce_Other/100
    # Y_prime["Services_Industrial", "Energy_Services"] <- Service_Industrial/100
    cat(paste0("[Sankey_Function.R]: Likely need to add flows into ['Services_Resident_Other', 'Energy_Services'] into U, V, and Y matrices."),sep="\n")
    
    ## need to add ethanol and potentially a new input "ldv_ethanol" maybe some efficiency somewhere too?
    Y_prime["Services_Transport_LDV_Petrol", "Energy_Services"] <- Service_LDV*as.numeric(ldv_petrol)/100
    Y_prime["Services_Transport_LDV_Elec", "Energy_Services"] <- Service_LDV*as.numeric(ldv_elec)/100
    Y_prime["Services_Transport_LDV_Ethanol", "Energy_Services"] <- Service_LDV*as.numeric(ldv_ethanol)/100
    cat(paste0("[Sankey_Function.R]: Likely need to make 'Services_Transport_LDV_Ethanol' in Y_prime as a direct proportionality to 'Services_Transport_LDV_Petrol' with biofuel blending assumption."),sep="\n")
    
    ## need to edit below swap FT for Other also elec for Other
    Y_prime["Services_Transport_Other_Other", "Energy_Services"] <- Service_Other_Transport*as.numeric(trans_other_other)/100
    Y_prime["Services_Transport_Other_Petrol", "Energy_Services"] <- Service_Other_Transport*as.numeric(trans_other_petrol)/100
    Y_prime["Services_Transport_Other_NG", "Energy_Services"] <- Service_Other_Transport*as.numeric(trans_other_ng)/100
    
    # create k_prime matrix
    frac_solar <- as.numeric(p_solar)/100
    frac_nuclear <- as.numeric(p_nuclear)/100
    frac_hydro <- as.numeric(p_hydro)/100
    frac_wind <- as.numeric(p_wind)/100
    frac_geo <- as.numeric(p_geo)/100
    frac_coal <- as.numeric(p_coal)/100
    frac_bio <- as.numeric(p_bio)/100
    frac_petrol <- as.numeric(p_petrol)/100
    # frac_ng <- as.numeric(p_ng)/100
    frac_ng <- 1 - sum(frac_solar,frac_nuclear,frac_hydro,frac_wind,frac_geo,frac_coal,frac_bio,frac_petrol)
    check_frac_total <- 1 - sum(frac_ng,frac_solar,frac_nuclear,frac_hydro,frac_wind,frac_geo,frac_coal,frac_bio,frac_petrol)
    
    k_prime <- matrix(data = c(frac_solar,frac_nuclear,frac_hydro, frac_wind, frac_geo, frac_ng, frac_coal, frac_bio, frac_petrol), ncol = 1, dimnames = list(c("Solar_Electricity", "Nuclear_Electricity", "Hydro_Electricity", "Wind_Electricity", "Geothermal_Electricity", "NaturalGas_Electricity", "Coal_Electricity", "Biomass_Electricity", "Petroleum_Electricity"), "Electricity_Grid")) %>%
       setrowtype("Products") %>% setcoltype("Industries")
    # k_prime <- matrix(data = c(as.numeric(p_solar)/100, as.numeric(p_nuclear)/100, as.numeric(p_hydro)/100, as.numeric(p_wind)/100, as.numeric(p_geo)/100, as.numeric(p_ng)/100, as.numeric(p_coal)/100, as.numeric(p_bio)/100, as.numeric(p_petrol)/100), ncol = 1, dimnames = list(c("Solar_Electricity", "Nuclear_Electricity", "Hydro_Electricity", "Wind_Electricity", "Geothermal_Electricity", "NaturalGas_Electricity", "Coal_Electricity", "Biomass_Electricity", "Petroleum_Electricity"), "Electricity_Grid")) %>%
    #   setrowtype("Products") %>% setcoltype("Industries")
    # k_prime <- matrix(data = c(as.integer(p_solar)/100, as.integer(p_nuclear)/100, as.integer(p_hydro)/100, as.integer(p_wind)/100, as.integer(p_geo)/100, as.integer(p_ng)/100, as.integer(p_coal)/100, as.integer(p_bio)/100, as.integer(p_petrol)/100), ncol = 1, dimnames = list(c("Solar_Electricity", "Nuclear_Electricity", "Hydro_Electricity", "Wind_Electricity", "Geothermal_Electricity", "NaturalGas_Electricity", "Coal_Electricity", "Biomass_Electricity", "Petroleum_Electricity"), "Electricity_Grid")) %>%
    #   setrowtype("Products") %>% setcoltype("Industries")
    # cat(paste0("[Sankey_Function.R]: Sum of k_prime is: ", sum(k_prime)),sep="\n")

    # 1st recalculation based on k_prime
    cat(paste0("[Sankey_Function.R]: Check if zero: sum(k_prime) - 1 = ", (sum(k_prime)-1)),sep="\n")
    if ((sum(k_prime)-1) != 0) {
      frac_ng <- frac_ng - (sum(k_prime)-1)
      ## Recalcualte k_prime
      k_prime <- matrix(data = c(frac_solar,frac_nuclear,frac_hydro, frac_wind, frac_geo, frac_ng, frac_coal, frac_bio, frac_petrol), ncol = 1, dimnames = list(c("Solar_Electricity", "Nuclear_Electricity", "Hydro_Electricity", "Wind_Electricity", "Geothermal_Electricity", "NaturalGas_Electricity", "Coal_Electricity", "Biomass_Electricity", "Petroleum_Electricity"), "Electricity_Grid")) %>%
        setrowtype("Products") %>% setcoltype("Industries")
      cat(paste0("[Sankey_Function.R]: Check if zero (after correction): sum(k_prime) - 1 = ", (sum(k_prime)-1)),sep="\n")
    }
    browser()  ## use to start debugging here
    UV_k <- new_k_ps(c(io_mats, list(U = U, V = V, Y = Y, k_prime = k_prime)))

    # update io_mats_prime
    # sometimes small negative values appear due (likely) to computational approximations to zero, but negative values should not be in UV_k$V_prime and UV_k$U_prime as athey cause matrix inversion singularity
    tol <- 1e-14
    UV_k$U_prime[which(UV_k$U_prime<tol)]=0
    UV_k$V_prime[which(UV_k$V_prime<tol)]=0
    # UV_k$U_prime[which(UV_k$U_prime<0)]=0
    # UV_k$V_prime[which(UV_k$V_prime<0)]=0
    io_mats_prime <- calc_io_mats(U = UV_k$U_prime, V = UV_k$V_prime, Y = Y, S_units = NULL)

    # 2nd recalculation based on Y_prime
    UV_Y <- new_Y(c(io_mats_prime, list(Y_prime = Y_prime)))
    
    # generate edge_list and node_list
    el <- elnl_generate(UV_Y$U_prime, UV_Y$V_prime, UV_Y$Y_prime)[['el']]
    nl <- elnl_generate(UV_Y$U_prime, UV_Y$V_prime, UV_Y$Y_prime)[['nl']]
    
    # generate JSON filen from edge_list and node_list
    json <- list(links = el, nodes = nl)
    
    return(json)
    
  }}