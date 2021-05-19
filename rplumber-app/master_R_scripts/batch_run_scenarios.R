#This file runs consecutive simulations of the EFD code through the master_EIoF.R main function
#It takes in a specified simulations_[batch name].csv file contain the inputs for master_EIoF
#It saves out an .rds file containing the outputs from master_EIoF

# Created 5/13/2021 by Danny Greer

library("optparse")

## Command line arguements ##
option_list = list(
  make_option(c("-b", "--batch"), action="store", default=NA, type='character',
              help="batch name")

)
opt = parse_args(OptionParser(option_list=option_list))

# setwd('/Users/danny/Documents/UT_EIoF_webtool/rplumber-app/master_R_scripts')
source('master_EIoF.R')


#comment out this line and unccoment the next for testing in RStudio
# batch_name = opt$b
batch_name = 'test'

#read in specified simulations file
simulations = read.csv(paste0('simulations_',batch_name,'.csv'))

#create folder for saving outputs
save_folder = paste0(getwd(),'/simulations/',batch_name,'/')
if(dir.exists(save_folder) == FALSE){
  dir.create(save_folder)
}

# save time for each run
times = c()

#This for loop iterates through each row in the specified simulations csv file.
#It first collects inputs needed to run the EFD. These are what would normally be entered on the website
#     - percentage of generation by technology
#     - percentage of electric light duty vehicles
#     - percent of residential heating by technology

for(n in 1:nrow(simulations)){
  
#run 1 scenario for testing  
# for(n in 1:1){    start = Sys.time() #start timer
  
  start = Sys.time() #start timer
  
  print('')
  print('**************************************')
  print(paste0('Running simulation ',n,' of ',nrow(simulations)))
  print('**************************************')
  print('')
  
  #collect input data
  s = simulations[n,]
  scenario_prefix = batch_name
  region_id = s$region_id
  coal_percent = s$coal
  pv_percent = s$PV
  csp_percent = s$CSP
  wind_percent = s$wind
  biomass_percent = s$biomass
  hydro_percent = s$hydro
  petroleum_percent = s$petroleum
  nuclear_percent = s$nuclear
  geothermal_percent = s$geothermal
  ng_percent  = s$ng
  ldv_e = s$electric_light_duty_vehicles
  r_sh_e = s$electric_residential_heating
  r_sh_ng = s$natural_gas_residential_heating
  #create specific scenario name
  scenario_name = paste0(scenario_prefix,'_region_',region_id,'_coal_',coal_percent,'_pv_',pv_percent,'_csp_',csp_percent,'_wind_',wind_percent,'_bio_',biomass_percent,'_hydro_',hydro_percent,'_pet_',petroleum_percent,'_nuc_',nuclear_percent,'_geo_',geothermal_percent,'_ng_',ng_percent,'_ldve_',ldv_e,'_rshe_',r_sh_e,'_rshng_',r_sh_ng)
  
  #check if this run already exists. If yes, skip it
  if(file.exists(paste0(save_folder,scenario_name,'.rds'))){
    print('Simulaiton found, skipping to next one ')
   } else{
    start = Sys.time() #start timer
     
    #Run a simulation with the given inputs. Throw an error message if the run couldn't complete for some reason
    data = tryCatch(master_EIoF(region_id, coal_percent, pv_percent, csp_percent, wind_percent, biomass_percent, hydro_percent, petroleum_percent, nuclear_percent, geothermal_percent, ng_percent, ldv_e, r_sh_e, r_sh_ng),
            warning = function(w){print('***THE SIMULATION WAS SUCCESSFUL WITH AT LEAST 1 WARING***')},
            error = function(e){print('***THE SIMULATION FAILED TO COMPLETE***')})
    #Save the results of the simulation. Throw an error message if the data can't be saved for some reason.
    tryCatch(save(data,file=paste0(save_folder,scenario_name,'.rds')),
             warning=function(w){print('***THE DATA WAS SAVED WITH AT LEAST 1 WARNING***')},
             error=function(e){print('***THE DATA COULD NOT BE SAVE***')})

  }


  stop = Sys.time() - start #stop timer
  times = append(times,as.numeric(stop))
  average_time = mean(times)
  time_left = ((nrow(simulations) - n)*average_time)/60/60
  
  print('########################')
  print('')
  print(stop) #read timer
  print(paste0('Average time is: ',average_time))
  print(paste0('Estimated time remaining is: ',time_left))
  print('')
  print('########################')
  
  #remove the data
  rm(data)
}
