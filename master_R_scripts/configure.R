#This file configures setting for running the EFD locally
#The main function of this script manage the google sheet cahsflow models for the EFD 
# First, the script establishes a connection with the googlesheets api for an individual user

#Created by Danny Greer 05-19-2021

library(googledrive)

print("****************************************************************************************************************************")
print("*                                                                                                                          *")
print("*  In order to use the EFD, you will need a valid google account for interfacing with the googlesheets.                    *")
print("*  Please enter the email address asociated with your google account. This should open a browser window,                   *")
print("*  prompting you to authorize tidyverse to access to google drive account. You will need to authorize this to use the EFD  *")
print("*                                                                                                                          *")
print("****************************************************************************************************************************")

g_email <- readline(prompt="Google account email: ")

#authorize to connect to google drive acount
drive_auth(
  email = g_email,
  scopes = "https://www.googleapis.com/auth/drive",
)


#need to save the sheet names and IDs for later
AnnualStorage_Name = c()
NoStorage_Name = c()
AnnualStorage_ID = c()
NoStorage_ID = c()
EIoF_Region_storage = c()
EIoF_Region_NOstorage = c()

#get list of files to upload as google sheets
g_sheets = list.files("GoogleSheets")
#iterate through list of files
for(file_name in g_sheets){
  
  name = gsub(".xlsx","",file_name)
  region = substr(name,nchar(name)-1,nchar(name))
  
  #upload excel file to google drive as a google sheet
  sheet_meta = drive_upload(
    media = paste0(getwd(),"/GoogleSheets/",file_name),
    name,
    type = "spreadsheet",
    overwrite = TRUE,
    verbose=FALSE)
  
  drive_share(
    file = name,
    role = "writer",
    type = "user",
    emailAddress = g_email
  )
  
  #save the name and ID
  if(grepl('AnnualStorage', name, fixed = TRUE)){
    AnnualStorage_Name= append(AnnualStorage_Name,name)
    AnnualStorage_ID = append(AnnualStorage_ID,sheet_meta$id)
    EIoF_Region_storage = append(EIoF_Region_storage,region)
    
  }else{
    NoStorage_Name = append(NoStorage_Name,name)
    NoStorage_ID = append(NoStorage_ID,sheet_meta$id)
    EIoF_Region_NOstorage = append(EIoF_Region_NOstorage,region)
    
  }
  print(paste0("Successfully uploaded ",name))
}

###############################################################
#             Installing R packages                           #
###############################################################

install.packages(c('../R_packages_static/rlang_0.4.6.tar.gz','R_packages_static/glue_1.4.1.tar.gz','R_packages_static/lifecycle_0.2.0.tar.gz',
'R_packages_static/mime_0.9.tar.gz','R_packages_static/magrittr_1.5.tar.gz','R_packages_static/swagger_3.33.0.tar.gz','R_packages_static/sodium_1.1.tar.gz',
'R_packages_static/BH_1.72.0-3.tar.gz','R_packages_static/Rcpp_1.0.4.6.tar.gz','R_packages_static/later_1.1.0.1.tar.gz','R_packages_static/R6_2.4.1.tar.gz',
'R_packages_static/promises_1.1.1.tar.gz','R_packages_static/crayon_1.3.4.tar.gz','R_packages_static/httpuv_1.5.3.1.tar.gz','R_packages_static/jsonlite_1.6.1.tar.gz',
'R_packages_static/curl_4.3.tar.gz','R_packages_static/webutils_1.1.tar.gz','R_packages_static/stringi_1.4.6.tar.gz','R_packages_static/plumber_0.4.6.tar.gz'),
repos = NULL, type='source')

###############################################################
#   Outputs                                                   #
#   1. Rdata file containing the google sheet names and IDs   #
#   2. config.csv file containing configuration settings      #
###############################################################


#Save a data file with all names and sheet IDs for each region
EIoF_GoogleSheet_Names_storage = data.frame(EIoF_Region_storage,AnnualStorage_ID, AnnualStorage_Name)
EIoF_GoogleSheet_Names_NOstorage = data.frame(EIoF_Region_NOstorage, NoStorage_ID, NoStorage_Name)
EIoF_GoogleSheet_Names = merge(EIoF_GoogleSheet_Names_NOstorage,EIoF_GoogleSheet_Names_storage,by.x = 'EIoF_Region_NOstorage',by.y = 'EIoF_Region_storage')
colnames(EIoF_GoogleSheet_Names) = c('EIoF_Region','NoStorage_ID','NoStorage_Name','AnnualStorage_ID','AnnualStorage_Name')

region_order = c('NW','CA','MN','SW','CE','TX','MW','AL','MA','SE','FL','NY','NE')
EIoF_GoogleSheet_Names = EIoF_GoogleSheet_Names[match(region_order, EIoF_GoogleSheet_Names$EIoF_Region),]

save(EIoF_GoogleSheet_Names,file=paste0("EIoF_gs4_function_data/EIoF_GoogleSheet_NamesAndIDs.rdata"))     ## save Google Sheet names and IDs as Rdata file to load in other R codes

#save a configuration with any specific info that is needed for the EFD to run
#right now this is just the email for associated google drive account
config = data.frame(g_email)
write.csv(config,'config.csv',row.names = FALSE)
