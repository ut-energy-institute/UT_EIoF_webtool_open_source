library("optparse")

### Take in arguments for running on with command line ###
option_list = list(
  make_option(c("-b", "--batch"), action="store", default=NA, type='character',
              help="batch name")

)
opt = parse_args(OptionParser(option_list=option_list))

# setwd('/Users/danny/Documents/UT_EIoF_webtool/rplumber-app/master_R_scripts')
source('master_EIoF.R')

###### Scenarios ##########

#comment out this line and unccoment the next for testing in RStudio
# batch_name = opt$b
batch_name = 'test'

scenarios = read.csv(paste0('scenarios_',batch_name,'.csv'))

scenarios$scenario_name = as.character(scenarios$scenario_name)

save_folder = paste0(getwd(),'/simulations/',batch_name)

if(dir.exists(save_folder) == FALSE){
  
  
}

# save time for each run
times = c()

#Start of loop
for(n in 1:nrow(scenarios)){
  
# for(n in 1:1){
  print(paste0('scenario ',n,' of ',nrow(scenarios)))
  s = scenarios[n,]
  scenario_prefix = s$scenario_name
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
  ldv_e = s$ldv_e
  r_sh_e = s$r_sh_e
  r_sh_ng = s$r_sh_ng
  scenario_name = paste0(scenario_prefix,'_region_',region_id,'_coal_',coal_percent,'_pv_',pv_percent,'_csp_',csp_percent,'_wind_',wind_percent,'_bio_',biomass_percent,'_hydro_',hydro_percent,'_pet_',petroleum_percent,'_nuc_',nuclear_percent,'_geo_',geothermal_percent,'_ng_',ng_percent,'_ldve_',ldv_e,'_rshe_',r_sh_e,'_rshng_',r_sh_ng)
  if(file.exists(paste0(save_folder,batch_name,'/',scenario_name,'.rds'))){
    next
   } else{
    old = Sys.time() #start timer
    data = tryCatch(master_EIoF(region_id, coal_percent, pv_percent, csp_percent, wind_percent, biomass_percent, hydro_percent, petroleum_percent, nuclear_percent, geothermal_percent, ng_percent, ldv_e, r_sh_e, r_sh_ng),
            warning = function(w){print('Warning')},
            error = function(e){print('Failed')})
    tryCatch(save(data,file=paste0(save_folder,batch_name,'/',scenario_name,'.rds')),
             warning=function(w){print('Nothing to save')},
             error=function(e){print('Nothing to save')})

    new = Sys.time() - old #stop timer
    times = append(times,as.numeric(new))
    average_time = mean(times)
    time_left = ((nrow(scenarios) - n)*average_time)/60/60
  }


  print('########################')
  print('')
  print(new) #read timer
  print(paste0('Average time is: ',average_time))
  print(paste0('Estimated time remaining is: ',time_left))
  print('')
  print('########################')
  rm(data)
}
