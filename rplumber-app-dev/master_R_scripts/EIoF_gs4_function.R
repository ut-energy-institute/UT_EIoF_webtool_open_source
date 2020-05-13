## This function will send data to GG's googlesheet model and extract data from it after it has updated

## EIoF_gs_function.R 
## ORIGINAL:Joshua D. Rhodes, PhD, 2019-05-06
## CURRENT: Carey W. King, PhD, 2020-05-11

EIoF_gs4_function <- function(SG_out){

  library(googlesheets4) # need to specifically call library "rlang"?
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
  
  # ## +++++++++++++
  # ## Call or obtain access to Google Sheet (owned by EI)
  # ## Using "googlesheets" R package
  # ## +++++++++++++
  # ## Spreadsheet ID
  # googlesheet_fileID = "16jQuodzL_jBE6lOgbVvahovt7mqDq-9a47Ndt97KAA0"
  # ## Call Google Sheet on EI account
  # client.id.gs <- "613586152170-2bj9g0i5kmdcsuo2bu43nkua1011t4ho.apps.googleusercontent.com"
  # client.secret.gs <- "f7BtdCT_maBHCrlngN_9bJdf"
  # token_eiof <- gs_auth(token = NULL,
  #                       new_user = FALSE,
  #                       key = client.id.gs,
  #                       secret = client.secret.gs,
  #                       cache = TRUE,
  #                       verbose = TRUE)  ## I think this command only needs to be called when the the file is first run in a new R session, but not after ...
  # eiof <- gs_title("EIOF_google_sheets_model_20191215_EICopy", verbose = F)  ## Call Google Sheet on EI account
  # 
  # # change a value (or range of values in the above sheet)
  # gs_input<-matrix(c(SG_out$MW_needed,SG_out$TWhGeneration),ncol=2)
  # gs_edit_cells(ss = eiof, ws = "Inputs", input = gs_input, anchor = "H3", byrow = F, verbose = F)
  # # get values from the sheet after input has been changed
  # output <- gs_read(ss = eiof, ws = "Aggregation", range = "A1:BB51", col_names = T, verbose = F)
  
  
  
  ## ++++++++++++++++
  # ## Code to create a new Google Sheet Refresh Token using "googlesheets4" v0.2.0 R package
  ## ++++++++++++++++
  # if (gs4_has_token()) {
  #   req <- request_generate(
  #     "sheets.spreadsheets.get",
  #     list(spreadsheetId = "abc"),
  #     token = gs4_token()
  #   )
  #   req
  # }
  ## ++++++++++++++++
  
  # if (gs4_has_token()) {
  #   req <- request_generate(
  #     "sheets.spreadsheets.get",
  #     list(spreadsheetId = googlesheet_fileID),
  #     token = gs4_token()
  #   )
  #   req
  # }
  # token_eiof = req$token
  
  ## +++++++++++++
  ## Call or obtain access to Google Sheet (owned by EI)
  ## Using "googlesheets4" R package
  ## +++++++++++++
  ## Spreadsheet ID
  googlesheet_fileID = "16jQuodzL_jBE6lOgbVvahovt7mqDq-9a47Ndt97KAA0"
  gs4_auth(email = "utenergyinstitute@gmail.com",
                        scopes = "https://www.googleapis.com/auth/spreadsheets",
                        path = "client_secret_613586152170-2bj9g0i5kmdcsuo2bu43nkua1011t4ho.apps.googleusercontent.com.json")
  # eiof = gs4_get("16jQuodzL_jBE6lOgbVvahovt7mqDq-9a47Ndt97KAA0")
  eiof = gs4_get(googlesheet_fileID)
  
  # ## Call Google Sheet on EI account
  # client.id.gs <- "613586152170-2bj9g0i5kmdcsuo2bu43nkua1011t4ho.apps.googleusercontent.com"
  # client.secret.gs <- "f7BtdCT_maBHCrlngN_9bJdf"
  # token_eiof <- gs4_auth(token = NULL,
  #                       new_user = FALSE,
  #                       key = client.id.gs,
  #                       secret = client.secret.gs,
  #                       cache = TRUE,
  #                       verbose = TRUE)  ## I think this command only needs to be called when the the file is first run in a new R session, but not after ...
  # eiof <- gs_title("EIOF_google_sheets_model_20191215_EICopy", verbose = F)  ## Call Google Sheet on EI account
  
  # change a value (or range of values in the above sheet)
  # gs4_input_test <- data.frame(rep(2e4,12),rep(200,12))
  # names(gs4_input_test)<-c("MW","TWh")
  # range_write(eiof,gs4_input_test,sheet="Inputs",range = "H3",col_names = FALSE)
  
  #gs_input <- matrix(c(SG_out$MW_needed,SG_out$TWhGeneration),ncol=2)
  gs4_input <- data.frame(SG_out$MW_needed,SG_out$TWhGeneration)
  range_write(eiof,gs4_input,sheet="Inputs",range = "H3",col_names = FALSE)
  # get values from the sheet after input has been changed
  output <- range_read(eiof,sheet="Aggregation",range = "A1:BB51",col_names = T)

  return(output)
  
}