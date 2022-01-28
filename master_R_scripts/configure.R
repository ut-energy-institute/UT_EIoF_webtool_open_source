#This file configures setting for running the EFD locally
#The main function of this script manage the google sheet cahsflow models for the EFD 
# First, the script establishes a connection with the googlesheets api for an individual user

#Created by Danny Greer 05-19-2021



###############################################################
#             Installing R packages                           #
###############################################################


cat('Select how you would like to install packages: \n 1. Install previous versions from local tar.gz files (this will overwrite any installed packages with a previous version) \n 2. Install most recent version from web (as of 5/26/2021 the current versions of these packages will work with the EFD, \n      newer versions of packages may not be compatible)')

pkg_inst <- readline(prompt="Select: ")

install.packages(c('../R_packages_static/Recca_0.1.18.tar.gz','../R_packages_static/matsbyname_0.4.14.tar.gz','../R_packages_static/matsindf_0.3.4.tar.gz'),repos=NULL,type='source')

if(pkg_inst == "1"){

install.packages(c('../R_packages_static/lubridate_1.7.8.tar.gz','../R_packages_static/jsonlite_1.6.1.tar.gz','../R_packages_static/readr_1.3.1.tar.gz','../R_packages_static/magrittr_1.5.tar.gz',
                   '../R_packages_static/tibble_3.0.1.tar.gz','../R_packages_static/googledrive_1.0.1.tar.gz','../R_packages_static/googlesheets4_0.2.0.tar.gz','../R_packages_static/dplyr_0.8.5.tar.gz',
                   '../R_packages_static/Rcgmin_2013-2.21.tar.gz','../R_packages_static/numDeriv_2016.8-1.1.tar.gz'),repos = NULL, type='source')
}else{
#install.packages(c('lubridate','jsonlite','tibble','googledrive','Rcgmin','numDeriv','readr','googlesheets4','dplyr','magrittr'))
  
  options(install.packages.compile.from.source = "never")  ## instructs "install.packages" command to not install packages from sources that require compilation
  ## 1
  ## You can likely answer "no" to the question "Do you want to install from sources the packages which need compilation?"
  print("Installing (+ dependencies): 'lubridate','jsonlite','readr','Rcgmin','fuzzyjoin','lazyeval','networkD3'.")
  install.packages(c('lubridate','jsonlite','readr','Rcgmin',
                     'fuzzyjoin','lazyeval','networkD3'))
  ## 2
  ## You can likely answer "no" to the question "Do you want to install from sources the packages which need compilation?"
  print("Installing (+ dependencies): 'matsbyname','matsindf'.")
  install.packages(c('matsbyname','matsindf'))
  ## 3
  ## You can likely answer "no" to the question "Do you want to install from sources the packages which need compilation?"
  ## The installation of "igraph" tends to fail generally when installing from source on January 19, 2022, so first try not installing from source that requires compilation.
  print("Installing (+ dependencies): 'igraph','qgraph'.")
  install.packages(c('igraph','qgraph'))
  ## 4
  print("Installing package 'Recca' from source .tar.gz file.")
  install.packages(c('../R_packages_static/Recca_0.1.18.tar.gz'),repos = NULL, type='source')
  
  options(install.packages.compile.from.source = "always")  ## instructs "install.packages" command to install packages from sources that require compilation
  ## 5
  ## You likely need to answer "YES" to the question "Do you want to install from sources the packages which need compilation?"
  print("Installing (+ dependencies) and compiling from source code if needed: 'htmlTable'.")
  install.packages(c('htmlTable'))
  ## 6
  print("Installing (+ dependencies) and compiling from source code if needed: 'optimr'.")
  install.packages(c('optimr'))
  ## 7
  ## You likely need to answer "YES" to the question "Do you want to install from sources the packages which need compilation?"
  print("Installing (+ dependencies) and compiling from source code if needed: 'googlesheets4'.")
  install.packages(c('googlesheets4'))
  
  options(install.packages.compile.from.source = "interactive")  ## instructs "install.packages" command (back to default condition) to ASK THE USER whether to install packages from sources that require compilation
  print("All packages installed.")

}


###############################################################
#             Creating Google Sheets                          #
###############################################################

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

print("****************************************************************************************************************************")
print("*                                                                                                                          *")
print("configure.R is complete.")
print("If the code does not work after this configure.R file is complete, this is likely due to                                    ")
print("a problem installing one of the packages.  Try to install each package, one-by-one as prompted,                             ")
print("with dependencies and without 'compiling from source code' if prompted until the code runs.                                 ")
print("Use the code 'master_EIoF_FOR_CODE_TESTING.R' to run 1 full simulation, and you can change the inputs                       ")
print("in the early lines of the code: region_id = 6; coal_percent = 10; ... r_sh_ng = 58, to test different user inputs.          ")
print("*                                                                                                                          *")
print("****************************************************************************************************************************")
