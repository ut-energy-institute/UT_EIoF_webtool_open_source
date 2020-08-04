## This function will send data to GG's googlesheet model and extract data from it after it has updated

## EIoF_gs_function.R 
## ORIGINAL:Joshua D. Rhodes, PhD, 2019-05-06
## CURRENT: Carey W. King, PhD, 2020-05-11

#EIoF_gs4_function <- function(GS_inputs){
EIoF_gs4_function <- function(RegionNumber,GS_inputs_AnnualStorage,GS_inputs_NoStorage){

  library(googlesheets4) 
  suppressMessages(library(dplyr))
  
  ## +++++++++++++
  ## Array and selection of the correct two Google Sheets for the given input EIoF Region
  ## +++++++++++++
  if (RegionNumber == 1) {  ## Northwest Region
    googlesheet_fileID_AnnualStorage = "1n26pHHyz_8V1K3gLenpRNpdRV-erf3Vi8Fm5H6Qznuk"  ## This is ID for spreashseet "EIoF_CashFlow_AnnualStorage_NW" 
    googlesheet_fileID_NoStorage = "1IoXwXD1dyK5Wexw17klDDM5WJAFP1Hufp5FnFEl-nlU"  ## This is ID for spreashseet "EIoF_CashFlow_NoStorage_NW" 
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 2) { ## California
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 3) { ## Mountain North
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 4) { ## Southwest
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 5) { ## Central
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 6) { ## Texas
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 7) { ## Midwest
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 8) { ## Arkansas-Louisiana
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 9) { ## Mid-Atlantic
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 10) { ## Southeast
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 11) { ## Florida
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 12) { ## New York
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  } else if (RegionNumber == 13) { ## New England
    googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
    googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  }

  # ## +++++++++++++
  # ## Carey King's authorization for EIoF Google Sheet (06-11-2020)
  # ## NOTE: To allow the googlesheets4 package to access a google sheet, then you must invite this e-mail account to be editor of the Google Sheet:
  # ## "eiof-project@ambient-segment-274318.iam.gserviceaccount.com"
  # ## +++++++++++++
  # googlesheet_fileID = "1c8bmrvi_yMGn5UbFsB679ytxI7Z_Hnfu-JlQG2BQmsc"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI" that I changed to name "EIoF_CashFlow_working_version_EI"
  # gs4_auth(email = "eiof-project@ambient-segment-274318.iam.gserviceaccount.com",
  #         scopes = "https://www.googleapis.com/auth/spreadsheets",
  #         path = "sa.json")  ## "sa.json" refers to "service account (sa) key in json format"
  #         # path = "ambient-segment-274318-ServiceAcctKey_20200611v1.json")
  # eiof = gs4_get(googlesheet_fileID)
  # ## +++++++++++++
  # ## +++++++++++++
  # 
  # ## +++++++++++++
  # googlesheet_fileID_AnnualStorage = "1Mnrm6Xh8PTSuPsm3vN3b6RKnNZu5KIQ9KUVDbmzZxgQ"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_AnnualStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
  gs4_auth(email = "eiof-project@ambient-segment-274318.iam.gserviceaccount.com",
          scopes = "https://www.googleapis.com/auth/spreadsheets",
          path = "sa.json")  ## "sa.json" refers to "service account (sa) key in json format"
  eiof_AnnualStorage = gs4_get(googlesheet_fileID_AnnualStorage)
  # ## +++++++++++++
  # 
  # ## +++++++++++++
  # googlesheet_fileID_NoStorage = "161Y2juXTPGGmRO8HA_spdjiOM5Fq9M2ojHr8AIl6308"  ## This is ID for spreashseet "EIoF_CashFlow_working_version_EI_NoStorage" that I changed to name "EIoF_CashFlow_working_version_EI"
  gs4_auth(email = "eiof-project@ambient-segment-274318.iam.gserviceaccount.com",
          scopes = "https://www.googleapis.com/auth/spreadsheets",
          path = "sa.json")  ## "sa.json" refers to "service account (sa) key in json format"
  eiof_NoStorage = gs4_get(googlesheet_fileID_NoStorage)
  # ## +++++++++++++
  
  ## Define/arrange inputs to Google Sheets
  # gs4_input <- data.frame(GS_inputs$MW_needed,GS_inputs$TWhGeneration)  ## Inputs:  MW capacity (and some other data) and TWh needed per generator
  gs4_input_AnnualStorage <- data.frame(GS_inputs_AnnualStorage$MW_needed,GS_inputs_AnnualStorage$TWhGeneration)  ## Inputs:  MW capacity (and some other data) and TWh needed per generator
  gs4_input_NoStorage <- data.frame(GS_inputs_NoStorage$MW_needed,GS_inputs_NoStorage$TWhGeneration)  ## Inputs:  MW capacity (and some other data) and TWh needed per generator
  
  ## Write data to Google Sheets
  # range_write(eiof,gs4_input,sheet="Inputs",range = "H3",col_names = FALSE)
  range_write(eiof_NoStorage,gs4_input_NoStorage,sheet="Inputs",range = "H3",col_names = FALSE)
  range_write(eiof_AnnualStorage,gs4_input_AnnualStorage,sheet="Inputs",range = "H3",col_names = FALSE)
  
  ## Put in a pause to see if this lack of pause is causing issues in getting correct values from Google Sheet
#   delay_time = 0 ## in seconds
#   for (i in 1:1)  {
# #    print(i)
#     date_time<-Sys.time()
#     while((as.numeric(Sys.time()) - as.numeric(date_time))<delay_time){} #dummy while loop
# }
  # for (i in 1:2)
  # {
  #   print(i)
  #   date_time<-Sys.time()
  #   while((as.numeric(Sys.time()) - as.numeric(date_time))<2.5){} #dummy while loop
  # }
  
  # Read calculated values from Google Sheet
  # output <- range_read(eiof,sheet="Aggregation",range = "A1:BB63",col_names = T)
  # return(output)
  
  output_NoStorage <- range_read(eiof_NoStorage,sheet="Aggregation",range = "A1:BB63",col_names = T)
  output_AnnualStorage <- range_read(eiof_AnnualStorage,sheet="Aggregation",range = "A1:BB63",col_names = T)
  output <- list(output_AnnualStorage,output_NoStorage)
  return(output)
  
}