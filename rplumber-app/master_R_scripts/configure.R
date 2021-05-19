#This file configures setting for running the EFD locally
#The main function of this script manage the google sheet cahsflow models for the EFD 
# First, the script establishes a connection with the googlesheets api for an individual user

print("****************************************************************************************************************************")
print("*                                                                                                                          *")
print("*  In order to use the EFD, you will need a valid google account for interfacing with the googlesheets.                    *")
print("*  Please enter the email address asociated with your google account. This should open a browser window,                   *")
print("*  prompting you to authorize tidyverse to access to google drive account. You will need to authorize this to use the EFD  *")
print("*                                                                                                                          *")
print("****************************************************************************************************************************")

g_email <- readline(prompt="Google account email: ")

drive_auth(
  email = g_email,
  scopes = "https://www.googleapis.com/auth/drive",
)

g_sheets = list.files("GoogleSheets")

AnnualStorage_Name = c()
NoStorage_Name = c()
AnnualStorage_ID = c()
NoStorage_ID = c()

for(file_name in g_sheets){
  
  name = gsub(".xlsx","",file_name)
  EIoF_Region = substr(name,nchar(name)-1,nchar(name))
  
  sheet_meta = drive_upload(
    media = paste0(getwd(),"/GoogleSheets/",file_name),
    name,
    type = "spreadsheet")
  
  
  if(grepl('AnnualStorage', name, fixed = TRUE)){
    append(AnnualStorage_Name,name)
    append(AnnualStorage_ID,sheet_meta$id)
  }else{
    append(NoStorage_Name,name)
    append(NoStorage_ID,sheet_meta$id)
  }
}

EIoF_GoogleSheet_Names = data.frame(EIoF_Region, NoStorage_ID, NoStorage_Name, 
                                    AnnualStorage_ID, AnnualStorage_Name)

save(EIoF_GoogleSheet_Names,file=paste0("EIoF_gs4_function_data/EIoF_GoogleSheet_NamesAndIDs.rdata"))     ## save Google Sheet names and IDs as Rdata file to load in other R codes

config = data.frame(g_email)
write.csv(config,'config.csv',row.names = FALSE)
