## This function will send data to GG's googlesheet model and extract data from it after it has updated

## EIoF_gs_function.R 
## ORIGINAL:Joshua D. Rhodes, PhD, 2019-05-06
## CURRENT: Danny Greer, 2021-05-19

EIoF_gs4_function <- function(RegionNumber,GS_inputs_AnnualStorage,GS_inputs_NoStorage){

  suppressMessages(library(dplyr))
  load("EIoF_gs4_function_data/EIoF_GoogleSheet_NamesAndIDs.Rdata")
  
  config = read.csv('config.csv')
  g_email = as.character(config$g_email)
  
  ## +++++++++++++
  ## Array and selection of the correct two Google Sheets for the given input EIoF Region
  ## +++++++++++++
  googlesheet_fileID_NoStorage = as.character(EIoF_GoogleSheet_Names$NoStorage_ID[RegionNumber])   ## This is ID for spreashseet to perform "NoStorage" calculations 
  googlesheet_fileID_AnnualStorage = as.character(EIoF_GoogleSheet_Names$AnnualStorage_ID[RegionNumber])   ## This is ID for spreashseet to perform "NoStorage" calculations  

  # ## +++++++++++++
  # ## Authorization for EIoF Google Sheet (06-11-2020)
  # ## +++++++++++++
  gs4_auth(email = g_email,
          scopes = "https://www.googleapis.com/auth/spreadsheets")#,

  eiof_AnnualStorage = gs4_get(googlesheet_fileID_AnnualStorage)
  eiof_NoStorage = gs4_get(googlesheet_fileID_NoStorage)
  # ## +++++++++++++

  ## Define/arrange inputs to Google Sheets
  gs4_input_AnnualStorage <- data.frame(GS_inputs_AnnualStorage$MW_needed,GS_inputs_AnnualStorage$TWhGeneration)  ## Inputs:  MW capacity (and some other data) and TWh needed per generator
  gs4_input_NoStorage <- data.frame(GS_inputs_NoStorage$MW_needed,GS_inputs_NoStorage$TWhGeneration)  ## Inputs:  MW capacity (and some other data) and TWh needed per generator

  ## Write data to Google Sheets
  range_write(eiof_NoStorage,gs4_input_NoStorage,sheet="Inputs",range = "H3",col_names = FALSE)
  range_write(eiof_AnnualStorage,gs4_input_AnnualStorage,sheet="Inputs",range = "H3",col_names = FALSE)

  output_NoStorage <- range_read(eiof_NoStorage,sheet="Aggregation",range = "A1:BB93",col_names = T)
  output_AnnualStorage <- range_read(eiof_AnnualStorage,sheet="Aggregation",range = "A1:BB93",col_names = T)
  output <- list(output_AnnualStorage,output_NoStorage)
  return(output)
  
}

