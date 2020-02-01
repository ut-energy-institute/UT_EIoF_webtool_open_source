# this function 
# 1. takes the total amount of electricy used in the transportation sector and returns an hourly charging profile
# 2. takes the total amount of electricity used for residentail heat and creats an hourly demand profile (might change)


generate8760 <- function(ldv_quads, res_heat_quads){
  
  set.seed(3000)
  
  # convert LDV quads to MWh
  quad_to_MWh_conversion <- 293071083.3
  ldv_MWh <- quad_to_MWh_conversion*ldv_quads
  
  #################### begin make up profile (need to update later!) #################### 
  day <- c(10,10,10,9,8,8,7,5,4,3,2,2,3,3,2,3,3,4,5,7,8,9,10,10)
  year <- rep(day, times = 365)
  
  for(i in 1:length(year)){
    
    year[i] <- year[i] + runif(1, min = -1)
    
  }
  
  ldv_charge_profile <- year/sum(year)
  #################### end make up profile (need to update later!) ####################
  
  ldv_charging_load <- ldv_charge_profile*ldv_MWh
  
  return(ldv_charging_load)
  
}