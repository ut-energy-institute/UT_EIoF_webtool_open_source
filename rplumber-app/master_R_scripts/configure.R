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

#get list of files to upload as google sheets
g_sheets = list.files("GoogleSheets")
#iterate through list of files
for(file_name in g_sheets){
  
  name = gsub(".xlsx","",file_name)
  EIoF_Region = substr(name,nchar(name)-1,nchar(name))
  
  #upload excel file to google drive as a google sheet
  sheet_meta = drive_upload(
    media = paste0(getwd(),"/GoogleSheets/",file_name),
    name,
    type = "spreadsheet")
  
  #save the name and ID
  if(grepl('AnnualStorage', name, fixed = TRUE)){
    AnnualStorage_Name= append(AnnualStorage_Name,name)
    AnnualStorage_ID = append(AnnualStorage_ID,sheet_meta$id)
  }else{
    NoStorage_Name = append(NoStorage_Name,name)
    NoStorage_ID = append(NoStorage_ID,sheet_meta$id)
  }
}

#Save a data file with all names and sheet IDs for each region
EIoF_GoogleSheet_Names = data.frame(EIoF_Region, NoStorage_ID, NoStorage_Name, 
                                    AnnualStorage_ID, AnnualStorage_Name)

save(EIoF_GoogleSheet_Names,file=paste0("EIoF_gs4_function_data/EIoF_GoogleSheet_NamesAndIDs.rdata"))     ## save Google Sheet names and IDs as Rdata file to load in other R codes

#save a configuration with any specific info that is needed for the EFD to run
#right now this is just the email for associated google drive account
config = data.frame(g_email)
write.csv(config,'config.csv',row.names = FALSE)
