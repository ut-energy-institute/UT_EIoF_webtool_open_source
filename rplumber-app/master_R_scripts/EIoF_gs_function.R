## This function will send data to GG's googlesheet model and extract data from it after it has updated

## EIoF_gs_function.R 
## Joshua D. Rhodes, PhD
## 2019-05-06

EIoF_gs_function <- function(SG_out){
  
  print(SG_out)
  
  library(googlesheets)
  suppressMessages(library(dplyr))
  gs_auth(token = "googlesheets_token.rds")
  
  # name the sheet to access
#  eiof <- gs_title("EIoF_gsheets_v1_beta", verbose = F) 
  eiof <- gs_title("EIOF_google_sheets_model_20191215", verbose = F)

#  elec_gen_fuels <- c(Coal, Nuclear, Natural_Gas, Hydro, Solar, Wind, Geothermal, MSW, Other_biomass, Other, Petroleum)
  
  # change a value (or range of values in the above sheet)
  gs_edit_cells(ss = eiof, ws = 3, input = SG_out$MW_needed, anchor = "H3", byrow = F, verbose = F)
  gs_edit_cells(ss = eiof, ws = 3, input = SG_out$TWhGeneration, anchor = "I3", byrow = F, verbose = F)
  
  # get values from the sheet after input has been changed
  output <- gs_read(ss = eiof, ws = 5, range = "A1:BB42", col_names = T, verbose = F)
  
  #output <- as.numeric(gsub(',', '', gs_read(ss = eiof, ws = 6, range = "U15:BA15", col_names = F, verbose = F)))
  
  return(output)
  
}