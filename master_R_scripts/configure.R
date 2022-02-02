#This file configures setting for running the EFD locally
#The main function of this script manage the google sheet cahsflow models for the EFD 
# First, the script establishes a connection with the googlesheets api for an individual user

#Created by Danny Greer 05-19-2021
#Edited by Carey King 02-02-2022



###############################################################
#             Installing R packages                           #
###############################################################


cat('Select how you would like to install packages: \n 1. Install previous versions from local tar.gz files (this will overwrite any installed packages with a previous version) \n 2. Install most recent version from web (as of 5/26/2021 the current versions of these packages will work with the EFD, \n      newer versions of packages may not be compatible)')
pkg_inst <- readline(prompt="Select: ")

cat('Are you running a Windows operating system or Mac/Unix (used if installing from source files)? \n 1 = Windows; \n 2 = Mac (or Linux, Unix)')
OS_type <- readline(prompt="Select: ")

#install.packages(c('../R_packages_static/Recca_0.1.18.tar.gz','../R_packages_static/matsbyname_0.4.14.tar.gz','../R_packages_static/matsindf_0.3.4.tar.gz'),repos=NULL,type='source')

if(pkg_inst == "1"){
   cat('This installation from source packages will likely take 10s of minutes (nearly 1 hour).')

#  install.packages(c('../R_packages_static/lubridate_1.7.8.tar.gz','../R_packages_static/jsonlite_1.6.1.tar.gz','../R_packages_static/readr_1.3.1.tar.gz','../R_packages_static/magrittr_1.5.tar.gz',
#                   '../R_packages_static/tibble_3.0.1.tar.gz','../R_packages_static/googledrive_1.0.1.tar.gz','../R_packages_static/googlesheets4_0.2.0.tar.gz','../R_packages_static/dplyr_0.8.5.tar.gz',
#                   '../R_packages_static/Rcgmin_2013-2.21.tar.gz','../R_packages_static/numDeriv_2016.8-1.1.tar.gz'),repos = NULL, type='source')

  if(OS_type==2){
    install.packages(c('../R_packages_static/yaml_2.2.1.tar.gz','../R_packages_static/xml2_1.3.2.tar.gz','../R_packages_static/XML_3.99-0.3.tar.gz','../R_packages_static/xfun_0.13.tar.gz','../R_packages_static/withr_2.2.0.tar.gz','../R_packages_static/whisker_0.4.tar.gz','../R_packages_static/viridisLite_0.3.0.tar.gz','../R_packages_static/gtable_0.3.0.tar.gz','../R_packages_static/gridExtra_2.3.tar.gz','../R_packages_static/rlang_0.4.6.tar.gz','../R_packages_static/glue_1.4.1.tar.gz','../R_packages_static/digest_0.6.25.tar.gz','../R_packages_static/ellipsis_0.3.0.tar.gz','../R_packages_static/vctrs_0.3.0.tar.gz','../R_packages_static/pkgconfig_2.0.3.tar.gz','../R_packages_static/utf8_1.1.4.tar.gz','../R_packages_static/fansi_0.4.1.tar.gz','../R_packages_static/crayon_1.3.4.tar.gz','../R_packages_static/assertthat_0.2.1.tar.gz','../R_packages_static/cli_2.0.2.tar.gz','../R_packages_static/pillar_1.4.4.tar.gz','../R_packages_static/magrittr_1.5.tar.gz','../R_packages_static/lifecycle_0.2.0.tar.gz','../R_packages_static/tibble_3.0.1.tar.gz','../R_packages_static/RColorBrewer_1.1-2.tar.gz','../R_packages_static/R6_2.4.1.tar.gz','../R_packages_static/colorspace_1.4-1.tar.gz','../R_packages_static/munsell_0.5.0.tar.gz','../R_packages_static/labeling_0.3.tar.gz','../R_packages_static/farver_2.0.3.tar.gz','../R_packages_static/scales_1.1.1.tar.gz','../R_packages_static/praise_1.0.0.tar.gz','../R_packages_static/rstudioapi_0.11.tar.gz','../R_packages_static/backports_1.1.7.tar.gz','../R_packages_static/rprojroot_1.3-2.tar.gz','../R_packages_static/prettyunits_1.1.1.tar.gz','../R_packages_static/desc_1.2.0.tar.gz','../R_packages_static/ps_1.3.3.tar.gz','../R_packages_static/processx_3.4.2.tar.gz','../R_packages_static/callr_3.4.3.tar.gz','../R_packages_static/pkgbuild_1.0.8.tar.gz','../R_packages_static/pkgload_1.0.2.tar.gz','../R_packages_static/evaluate_0.14.tar.gz','../R_packages_static/testthat_2.3.2.tar.gz','../R_packages_static/Rcpp_1.0.4.6.tar.gz','../R_packages_static/isoband_0.2.1.tar.gz','../R_packages_static/ggplot2_3.3.0.tar.gz','../R_packages_static/viridis_0.5.1.tar.gz','../R_packages_static/uuid_0.1-4.tar.gz','../R_packages_static/tweenr_1.0.1.tar.gz','../R_packages_static/purrr_0.3.4.tar.gz','../R_packages_static/tidyselect_1.1.0.tar.gz','../R_packages_static/stringi_1.4.6.tar.gz','../R_packages_static/plogr_0.2.0.tar.gz','../R_packages_static/BH_1.72.0-3.tar.gz','../R_packages_static/dplyr_0.8.5.tar.gz','../R_packages_static/tidyr_1.0.3.tar.gz',
        '../R_packages_static/igraph_1.2.5.tar.gz','../R_packages_static/tidygraph_1.2.0.tar.gz','../R_packages_static/sys_3.3.tar.gz','../R_packages_static/survival_3.1-12.tar.gz','../R_packages_static/stringdist_0.9.5.5.tar.gz','../R_packages_static/sp_1.4-1.tar.gz','../R_packages_static/setRNG_2013.9-1.tar.gz','../R_packages_static/numDeriv_2016.8-1.1.tar.gz','../R_packages_static/optextras_2019-12.4.tar.gz','../R_packages_static/Rvmmin_2018-4.17.tar.gz','../R_packages_static/rpart_4.1-15.tar.gz','../R_packages_static/data.table_1.12.8.tar.gz','../R_packages_static/jsonlite_1.6.1.tar.gz','../R_packages_static/rlist_0.4.6.1.tar.gz','../R_packages_static/rjson_0.2.20.tar.gz','../R_packages_static/stringr_1.4.0.tar.gz','../R_packages_static/plyr_1.8.6.tar.gz','../R_packages_static/reshape2_1.4.4.tar.gz','../R_packages_static/remotes_2.1.1.tar.gz','../R_packages_static/rematch2_2.1.2.tar.gz','../R_packages_static/rematch_1.0.1.tar.gz','../R_packages_static/clipr_0.7.0.tar.gz','../R_packages_static/hms_0.5.3.tar.gz','../R_packages_static/readr_1.3.1.tar.gz','../R_packages_static/RcppEigen_0.3.3.7.0.tar.gz','../R_packages_static/RcppArmadillo_0.9.870.2.0.tar.gz','../R_packages_static/Rcgmin_2013-2.21.tar.gz','../R_packages_static/R.methodsS3_1.8.0.tar.gz','../R_packages_static/R.oo_1.23.0.tar.gz','../R_packages_static/R.utils_2.9.2.tar.gz','../R_packages_static/graphlayouts_0.7.0.tar.gz','../R_packages_static/ggrepel_0.8.2.tar.gz','../R_packages_static/polyclip_1.10-0.tar.gz','../R_packages_static/ggforce_0.3.1.tar.gz','../R_packages_static/ggraph_2.0.2.tar.gz','../R_packages_static/abind_1.4-5.tar.gz','../R_packages_static/pbapply_1.4-2.tar.gz','../R_packages_static/BDgraph_2.62.tar.gz','../R_packages_static/gtools_3.8.2.tar.gz','../R_packages_static/d3Network_0.5.2.1.tar.gz','../R_packages_static/fdrtool_1.2.15.tar.gz','../R_packages_static/huge_1.3.4.1.tar.gz','../R_packages_static/glasso_1.11.tar.gz','../R_packages_static/corpcor_1.6.9.tar.gz','../R_packages_static/png_0.1-7.tar.gz','../R_packages_static/jpeg_0.1-8.1.tar.gz','../R_packages_static/base64enc_0.1-3.tar.gz','../R_packages_static/htmltools_0.4.0.tar.gz','../R_packages_static/htmlwidgets_1.5.1.tar.gz','../R_packages_static/checkmate_2.0.0.tar.gz','../R_packages_static/mime_0.9.tar.gz','../R_packages_static/markdown_1.1.tar.gz','../R_packages_static/highr_0.8.tar.gz','../R_packages_static/knitr_1.28.tar.gz','../R_packages_static/htmlTable_1.13.3.tar.gz','../R_packages_static/acepack_1.4.1.tar.gz','../R_packages_static/latticeExtra_0.6-29.tar.gz','../R_packages_static/Formula_1.2-3.tar.gz','../R_packages_static/Hmisc_4.4-0.tar.gz','../R_packages_static/pbivnorm_0.6.0.tar.gz','../R_packages_static/mnormt_1.5-7.tar.gz','../R_packages_static/lavaan_0.6-6.tar.gz','../R_packages_static/psych_1.9.12.31.tar.gz',
        '../R_packages_static/qgraph_1.6.5.tar.gz','../R_packages_static/optimr_2019-12.16.tar.gz','../R_packages_static/askpass_1.1.tar.gz','../R_packages_static/openssl_1.4.1.tar.gz','../R_packages_static/nnet_7.3-14.tar.gz','../R_packages_static/nlme_3.1-147.tar.gz','../R_packages_static/networkD3_0.4.tar.gz','../R_packages_static/mgcv_1.8-31.tar.gz','../R_packages_static/matsbyname_0.4.14.tar.gz','../R_packages_static/matsindf_0.3.4.tar.gz','../R_packages_static/Matrix_1.2-18.tar.gz','../R_packages_static/MASS_7.3-51.6.tar.gz','../R_packages_static/generics_0.0.2.tar.gz','../R_packages_static/lubridate_1.7.8.tar.gz','../R_packages_static/lazyeval_0.2.2.tar.gz','../R_packages_static/lattice_0.20-41.tar.gz','../R_packages_static/ids_1.0.1.tar.gz','../R_packages_static/curl_4.3.tar.gz','../R_packages_static/httr_1.4.1.tar.gz','../R_packages_static/fs_1.4.1.tar.gz','../R_packages_static/gargle_0.5.0.tar.gz','../R_packages_static/googledrive_1.0.1.tar.gz','../R_packages_static/cellranger_1.1.0.tar.gz','../R_packages_static/googlesheets4_0.2.0.tar.gz','../R_packages_static/BiocGenerics_0.34.0.tar.gz','../R_packages_static/graph_1.66.0.tar.gz','../R_packages_static/ggm_2.5.tar.gz','../R_packages_static/geosphere_1.5-10.tar.gz','../R_packages_static/fuzzyjoin_0.1.5.tar.gz','../R_packages_static/cluster_2.1.0.tar.gz','../R_packages_static/zeallot_0.1.0.tar.gz','../R_packages_static/Recca_0.1.18.tar.gz'), repos = NULL, type='source')
  } else if (OS_type==1){
  ## 1 
  install.packages(c('../R_packages_static/yaml_2.2.1.tar.gz','../R_packages_static/xml2_1.3.2.tar.gz','../R_packages_static/xfun_0.13.tar.gz','../R_packages_static/withr_2.2.0.tar.gz','../R_packages_static/whisker_0.4.tar.gz','../R_packages_static/viridisLite_0.3.0.tar.gz','../R_packages_static/gtable_0.3.0.tar.gz','../R_packages_static/gridExtra_2.3.tar.gz','../R_packages_static/rlang_0.4.6.tar.gz','../R_packages_static/glue_1.4.1.tar.gz','../R_packages_static/digest_0.6.25.tar.gz','../R_packages_static/ellipsis_0.3.0.tar.gz','../R_packages_static/vctrs_0.3.0.tar.gz','../R_packages_static/pkgconfig_2.0.3.tar.gz','../R_packages_static/utf8_1.1.4.tar.gz','../R_packages_static/fansi_0.4.1.tar.gz','../R_packages_static/crayon_1.3.4.tar.gz','../R_packages_static/assertthat_0.2.1.tar.gz','../R_packages_static/cli_2.0.2.tar.gz','../R_packages_static/pillar_1.4.4.tar.gz','../R_packages_static/magrittr_1.5.tar.gz','../R_packages_static/lifecycle_0.2.0.tar.gz','../R_packages_static/tibble_3.0.1.tar.gz','../R_packages_static/RColorBrewer_1.1-2.tar.gz','../R_packages_static/R6_2.4.1.tar.gz','../R_packages_static/colorspace_1.4-1.tar.gz','../R_packages_static/munsell_0.5.0.tar.gz','../R_packages_static/labeling_0.3.tar.gz','../R_packages_static/farver_2.0.3.tar.gz','../R_packages_static/scales_1.1.1.tar.gz','../R_packages_static/praise_1.0.0.tar.gz','../R_packages_static/rstudioapi_0.11.tar.gz','../R_packages_static/backports_1.1.7.tar.gz','../R_packages_static/rprojroot_1.3-2.tar.gz','../R_packages_static/prettyunits_1.1.1.tar.gz','../R_packages_static/desc_1.2.0.tar.gz','../R_packages_static/ps_1.3.3.tar.gz','../R_packages_static/processx_3.4.2.tar.gz','../R_packages_static/callr_3.4.3.tar.gz','../R_packages_static/pkgbuild_1.0.8.tar.gz','../R_packages_static/pkgload_1.0.2.tar.gz','../R_packages_static/evaluate_0.14.tar.gz','../R_packages_static/testthat_2.3.2.tar.gz','../R_packages_static/Rcpp_1.0.4.6.tar.gz','../R_packages_static/isoband_0.2.1.tar.gz','../R_packages_static/ggplot2_3.3.0.tar.gz','../R_packages_static/viridis_0.5.1.tar.gz','../R_packages_static/uuid_0.1-4.tar.gz','../R_packages_static/tweenr_1.0.1.tar.gz','../R_packages_static/purrr_0.3.4.tar.gz','../R_packages_static/tidyselect_1.1.0.tar.gz','../R_packages_static/stringi_1.4.6.tar.gz','../R_packages_static/plogr_0.2.0.tar.gz','../R_packages_static/BH_1.72.0-3.tar.gz','../R_packages_static/dplyr_0.8.5.tar.gz','../R_packages_static/tidyr_1.0.3.tar.gz'), repos = NULL, type='source')
  
  ## 2 Normally "igraph" package would be next in list. Skip igraph install for now.
  install.packages(c('../R_packages_static/sys_3.3.tar.gz','../R_packages_static/survival_3.1-12.tar.gz','../R_packages_static/stringdist_0.9.5.5.tar.gz','../R_packages_static/sp_1.4-1.tar.gz','../R_packages_static/setRNG_2013.9-1.tar.gz','../R_packages_static/numDeriv_2016.8-1.1.tar.gz','../R_packages_static/optextras_2019-12.4.tar.gz','../R_packages_static/Rvmmin_2018-4.17.tar.gz','../R_packages_static/rpart_4.1-15.tar.gz','../R_packages_static/data.table_1.12.8.tar.gz','../R_packages_static/jsonlite_1.6.1.tar.gz','../R_packages_static/rjson_0.2.20.tar.gz','../R_packages_static/stringr_1.4.0.tar.gz','../R_packages_static/plyr_1.8.6.tar.gz','../R_packages_static/reshape2_1.4.4.tar.gz','../R_packages_static/remotes_2.1.1.tar.gz','../R_packages_static/rematch2_2.1.2.tar.gz','../R_packages_static/rematch_1.0.1.tar.gz','../R_packages_static/clipr_0.7.0.tar.gz','../R_packages_static/hms_0.5.3.tar.gz','../R_packages_static/readr_1.3.1.tar.gz','../R_packages_static/RcppEigen_0.3.3.7.0.tar.gz','../R_packages_static/RcppArmadillo_0.9.870.2.0.tar.gz','../R_packages_static/Rcgmin_2013-2.21.tar.gz','../R_packages_static/R.methodsS3_1.8.0.tar.gz','../R_packages_static/R.oo_1.23.0.tar.gz','../R_packages_static/R.utils_2.9.2.tar.gz','../R_packages_static/ggrepel_0.8.2.tar.gz','../R_packages_static/polyclip_1.10-0.tar.gz','../R_packages_static/ggforce_0.3.1.tar.gz','../R_packages_static/abind_1.4-5.tar.gz','../R_packages_static/pbapply_1.4-2.tar.gz','../R_packages_static/gtools_3.8.2.tar.gz','../R_packages_static/d3Network_0.5.2.1.tar.gz','../R_packages_static/fdrtool_1.2.15.tar.gz','../R_packages_static/glasso_1.11.tar.gz','../R_packages_static/corpcor_1.6.9.tar.gz','../R_packages_static/png_0.1-7.tar.gz','../R_packages_static/jpeg_0.1-8.1.tar.gz','../R_packages_static/base64enc_0.1-3.tar.gz','../R_packages_static/htmltools_0.4.0.tar.gz','../R_packages_static/htmlwidgets_1.5.1.tar.gz','../R_packages_static/checkmate_2.0.0.tar.gz','../R_packages_static/mime_0.9.tar.gz','../R_packages_static/markdown_1.1.tar.gz','../R_packages_static/highr_0.8.tar.gz','../R_packages_static/knitr_1.28.tar.gz','../R_packages_static/htmlTable_1.13.3.tar.gz','../R_packages_static/acepack_1.4.1.tar.gz','../R_packages_static/latticeExtra_0.6-29.tar.gz','../R_packages_static/Formula_1.2-3.tar.gz','../R_packages_static/Hmisc_4.4-0.tar.gz','../R_packages_static/pbivnorm_0.6.0.tar.gz','../R_packages_static/mnormt_1.5-7.tar.gz','../R_packages_static/lavaan_0.6-6.tar.gz','../R_packages_static/psych_1.9.12.31.tar.gz',
     '../R_packages_static/optimr_2019-12.16.tar.gz','../R_packages_static/askpass_1.1.tar.gz','../R_packages_static/openssl_1.4.1.tar.gz','../R_packages_static/nnet_7.3-14.tar.gz','../R_packages_static/nlme_3.1-147.tar.gz','../R_packages_static/mgcv_1.8-31.tar.gz','../R_packages_static/matsbyname_0.4.14.tar.gz','../R_packages_static/Matrix_1.2-18.tar.gz','../R_packages_static/MASS_7.3-51.6.tar.gz','../R_packages_static/generics_0.0.2.tar.gz','../R_packages_static/lubridate_1.7.8.tar.gz','../R_packages_static/lazyeval_0.2.2.tar.gz','../R_packages_static/lattice_0.20-41.tar.gz','../R_packages_static/ids_1.0.1.tar.gz','../R_packages_static/curl_4.3.tar.gz','../R_packages_static/httr_1.4.1.tar.gz','../R_packages_static/fs_1.4.1.tar.gz','../R_packages_static/gargle_0.5.0.tar.gz','../R_packages_static/googledrive_1.0.1.tar.gz','../R_packages_static/cellranger_1.1.0.tar.gz','../R_packages_static/googlesheets4_0.2.0.tar.gz','../R_packages_static/BiocGenerics_0.34.0.tar.gz','../R_packages_static/graph_1.66.0.tar.gz','../R_packages_static/ggm_2.5.tar.gz','../R_packages_static/geosphere_1.5-10.tar.gz','../R_packages_static/fuzzyjoin_0.1.5.tar.gz','../R_packages_static/cluster_2.1.0.tar.gz','../R_packages_static/zeallot_0.1.0.tar.gz'), repos = NULL, type='source')

  ## 3 install "igraph"package using binary (.zip) install choice.
  ## For unknown reason, install DOES NOT WORK FROM .tar.gz: install.packages(c('../R_packages_static/igraph_1.2.5.tar.gz'), repos = NULL, type='source')
  ## install.packages(c('igraph'))
  install.packages(c('../R_packages_static/igraph_1.2.11.zip'), repos = NULL, type='source')
  
  ## 4 Packages to get installed after igraph
  install.packages(c('../R_packages_static/tidygraph_1.2.0.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/graphlayouts_0.7.0.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/BDgraph_2.62.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/huge_1.3.4.1.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/ggraph_2.0.2.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/qgraph_1.6.5.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/networkD3_0.4.tar.gz'), repos = NULL, type='source')
  
  ## 4
  ## Need to install XML from binary .zip in Windows
  ## For unknown reason, install DOES NOT WORK FROM .tar.gz: install.packages(c('../R_packages_static/XML_3.99-0.3.tar.gz'), repos = NULL, type='source')
  ## install.packages(c('XML'))
  install.packages(c('../R_packages_static/XML_3.99-0.8.zip'), repos = NULL, type='source')

  ## 5: Installing other packages to use "Recca" package by Matt Heun used to Make Sankey diagrams and perform RECCA = "R Energy Conversion Chain Analysis"
  ## https://matthewheun.github.io/Recca/
  install.packages(c('../R_packages_static/rlist_0.4.6.1.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/matsindf_0.3.4.tar.gz'), repos = NULL, type='source')
  install.packages(c('../R_packages_static/Recca_0.1.18.tar.gz'), repos = NULL, type='source')
  } # END: else if (OS_type==1){
  
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
