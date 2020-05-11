## This function will send data to GG's googlesheet model and extract data from it after it has updated

## EIoF_gs_function.R 
## Joshua D. Rhodes, PhD
## 2019-05-06

EIoF_gs_function <- function(SG_out){
#EIoF_gs_function <- function(Coal = 0.2157, Nuclear =  0.1176,	Natural_Gas =  0.3544,	Hydro =  0.0550, Solar =  0.1442, Wind =  0.0824, Geothermal =  0.0120, MSW =  0.0050, Other_biomass =  0.0070,	Other =  0.0040, Petroleum =  0.0020){
  
  #library(googledrive)
  library(googlesheets)
  suppressMessages(library(dplyr))
  
  ## ++++++++++++++++
  # ## Code to create a new Google Sheet Refresh Token
  # token_TEMP <- gs_auth(cache = FALSE)
  # gd_token()
  # saveRDS(token_TEMP, file = "googlesheets_token_TEMP.rds")
  # ## Code to load Google Sheet Refresh Token to read/write to Google Sheet
  # gs_auth(token_TEMP = "googlesheets_token_TEMP.rds")  ## creates new token
  # suppressMessages(gs_auth(token_TEMP = "googlesheets_token_TEMP.rds", verbose = FALSE))   ## if you want silence re: token loading, use this instead
  ##
  ##
  ## Code to make GS token for transfer of Initial Google Sheet to EI Account
  # gstoken_EIOF_google_sheets_model_20191215_EICopy <- gs_auth(cache = FALSE)
  # gd_token()
  # saveRDS(gstoken_EIOF_google_sheets_model_20191215_EICopy, file = "gstoken_EIOF_google_sheets_model_20191215_EICopy.rds")
  # gs_auth(gstoken_EIOF_google_sheets_model_20191215_EICopy = "gstoken_EIOF_google_sheets_model_20191215_EICopy.rds")  ## creates new token
  # suppressMessages(gs_auth(gstoken_EIOF_google_sheets_model_20191215_EICopy = "gstoken_EIOF_google_sheets_model_20191215_EICopy.rds", verbose = FALSE))   ## if you want silence re: token loading, use this instead
  ## ++++++++++++++++
  
  ## +++++++++++++
  ## Call or obtain access to Google Sheet (original owned by JDR)
  ## +++++++++++++
  ##Load current Google Sheet Access token to access the Google Sheet
  # gs_auth(token = "googlesheets_token.rds")
  ##name the sheet to access
  ##Call Google Sheet on JDR account
  # eiof <- gs_title("EIOF_google_sheets_model_20191215", verbose = F)  ## Call Google Sheet on JDR account

  ## +++++++++++++
  ## Call or obtain access to Google Sheet (owned by EI)
  ## +++++++++++++
  ## Call Google Sheet on EI account
  client.id.gs <- "613586152170-2bj9g0i5kmdcsuo2bu43nkua1011t4ho.apps.googleusercontent.com"
  client.secret.gs <- "f7BtdCT_maBHCrlngN_9bJdf"
  token_eiof <- gs_auth(token = NULL,
                        new_user = FALSE,
                        key = client.id.gs,
                        secret = client.secret.gs,
                        cache = TRUE,
                        verbose = TRUE)  ## I think this command only needs to be called when the the file is first run in a new R session, but not after ...
  eiof <- gs_title("EIOF_google_sheets_model_20191215_EICopy", verbose = F)  ## Call Google Sheet on EI account
  
  ## +++++++++++++
  ## OTHER TESTING METHODS TO Call or obtain access to Google Sheet (owned by EI)
  ## +++++++++++++
  # test_eiof = drive_auth(email="utenergyinstitute@gmail.com",
  #                        scopes = "https://www.googleapis.com/auth/spreadsheets",
  #                        path = "client_secret_613586152170-2bj9g0i5kmdcsuo2bu43nkua1011t4ho.apps.googleusercontent.com.json")
  # drive_auth(email="utenergyinstitute@gmail.com",
  #                        scopes = "https://www.googleapis.com/auth/spreadsheets",
  #                        path = "client_secret_613586152170-2bj9g0i5kmdcsuo2bu43nkua1011t4ho.apps.googleusercontent.com.json")
  # drive_auth(email="utenergyinstitute@gmail.com",
  #                        scopes = "https://www.googleapis.com/auth/spreadsheets")
  # googlesheet_projectID = "ambient-segment-274318"
  # googlesheet_fileID = "16jQuodzL_jBE6lOgbVvahovt7mqDq-9a47Ndt97KAA0"
  # req <- request_generate(
  #   "drive.files.get",
  #   list(fileId = googlesheet_fileID),
  #   token = drive_token()
  # )
  # req
  # gs_auth(token = "EIToken_v2.rds")
  
  
  # change a value (or range of values in the above sheet)
#  gs_edit_cells(ss = eiof, ws = 3, input = elec_gen_fuels, anchor = "B49", byrow = F, verbose = F)
  # change a value (or range of values in the above sheet)
  gs_input<-matrix(c(SG_out$MW_needed,SG_out$TWhGeneration),ncol=2)
  gs_edit_cells(ss = eiof, ws = "Inputs", input = gs_input, anchor = "H3", byrow = F, verbose = F)
  #gs_edit_cells(ss = eiof, ws = 3, input = SG_out$MW_needed, anchor = "H3", byrow = F, verbose = F)
  #gs_edit_cells(ss = eiof, ws = 3, input = SG_out$TWhGeneration, anchor = "I3", byrow = F, verbose = F)
  
  # get values from the sheet after input has been changed
  output <- gs_read(ss = eiof, ws = "Aggregation", range = "A1:BB51", col_names = T, verbose = F)
  
  #output <- as.numeric(gsub(',', '', gs_read(ss = eiof, ws = 6, range = "U15:BA15", col_names = F, verbose = F)))
  
  return(output)
  
}