
create_figures <- function(data){
  
library(ggplot2) 
library(base64)
start.hour <- 4350 ## minimum is 1, maximum is 8760 
end.hour <- start.hour+24*5   ## minimum is 1, maximum is 8760 
num_PPtech <- dim(PP_MWneeded)[1]-1-dim(PP_MWneeded_temp)[1]  ##  Total number of Power Plant


Hour <- rep(data$Hour.ending[start.hour:end.hour],each=(num_PPtech))  ## 
Generation_MW <- rep(0,(end.hour-start.hour+1)*(num_PPtech))
for (i in 1:(end.hour-start.hour+1)) {
  # Generation_MW[((num_PPtech)*(i-1)+1):((num_PPtech)*i)] <- c(data$Nuclear_MW[i],(data$Wind_MW_total[i]-curtailed_Wind[i]),(data$PV_MW_total[i]-curtailed_PV[i]),(data$CSP_MW_total[i]-curtailed_CSP[i]),data$Coal_MW[i],data$NGCC_MW[i],data$NGCT_MW[i],data$UserTech1_MW[i],data$UserTech2_MW[i],data$Biomass_MW[i],data$Geothermal_MW[i],data$PetroleumCC_MW[i],data$HydroDispatch_MW[i])
  Generation_MW[((num_PPtech)*(i-1)+1):((num_PPtech)*i)] <- c(data$Nuclear_MW[start.hour+i],(data$Wind_MW_total[start.hour+i]-curtailed_Wind[start.hour+i]),(data$PV_MW_total[start.hour+i]-curtailed_PV[start.hour+i]),(data$CSP_MW_total[start.hour+i]-curtailed_CSP[start.hour+i]),data$Coal_MW[start.hour+i],data$NGCC_MW[start.hour+i],data$NGCT_MW[start.hour+i],data$UserTech1_MW[start.hour+i],data$UserTech2_MW[start.hour+i],data$Biomass_MW[start.hour+i],data$Geothermal_MW[start.hour+i],data$PetroleumCC_MW[start.hour+i],(data$HydroDispatch_MW[start.hour+i]+data$HydroNonDispatch_MW[start.hour+i]))
}
##Generator1 <- factor(rep(c("Nuclear","Wind","PV","Coal","NGCC","NGCT","UserTech1","UserTech2"),times=(end.hour-start.hour+1)))
##Generator1 = factor(Generator1,levels=levels(Generator1)[c(7,6,3,2,1,8,5,4)]) #reorder
Generator1 <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro"),times=(end.hour-start.hour+1)))
Generator1 = factor(Generator1,levels=levels(Generator1)[c(5,12,11,7,6,9,2,1,4,3,10,13,8)]) #reorder
data_stacked_area1 <- data.frame(Generation_MW,Hour,Generator1)
p1<-ggplot(data_stacked_area1, aes(x=Hour, y=Generation_MW, fill=Generator1)) +     geom_area() +
  labs(x = "Hour") + labs(y = "Generation (MW)") + guides(fill=guide_legend(title="Generator"))
ggsave('stacked_area_gen_no_storage.svg')
tmp1 <- tempfile()
figure1 <- base64enc::base64encode("stacked_area_gen_no_storage.svg",tmp1)

Hour <- rep(data$Hour.ending[start.hour:end.hour],each=(num_PPtech1a))  ## 
Generation_MW <- rep(0,(end.hour-start.hour+1)*(num_PPtech1a))
for (i in 1:(end.hour-start.hour+1)) {
  # Generation_MW[((num_PPtech)*(i-1)+1):((num_PPtech)*i)] <- c(data$Nuclear_MW[i],(data$Wind_MW_total[i]-curtailed_Wind[i]),(data$PV_MW_total[i]-curtailed_PV[i]),(data$CSP_MW_total[i]-curtailed_CSP[i]),data$Coal_MW[i],data$NGCC_MW[i],data$NGCT_MW[i],data$UserTech1_MW[i],data$UserTech2_MW[i],data$Biomass_MW[i],data$Geothermal_MW[i],data$PetroleumCC_MW[i],data$HydroDispatch_MW[i])
  Generation_MW[((num_PPtech1a)*(i-1)+1):((num_PPtech1a)*i)] <- c(data$Nuclear_MW[start.hour+i],(data$Wind_MW_DirectToGrid_AnnualStorage[start.hour+i]),(data$PV_MW_DirectToGrid_AnnualStorage[start.hour+i]),(data$CSP_MW_DirectToGrid_AnnualStorage[start.hour+i]),data$Coal_AnnualStorage_MW[start.hour+i],data$NGCC_AnnualStorage_MW[start.hour+i],data$NGCT_AnnualStorage_MW[start.hour+i],data$UserTech1_AnnualStorage_MW[start.hour+i],data$UserTech2_AnnualStorage_MW[start.hour+i],data$Biomass_AnnualStorage_MW[start.hour+i],data$Geothermal_AnnualStorage_MW[start.hour+i],data$PetroleumCC_AnnualStorage_MW[start.hour+i],(data$HydroDispatch_AnnualStorage_MW[start.hour+i]+data$HydroNonDispatch_MW[start.hour+i]),data$Dispatched_StoredWindSolar_MW[start.hour+i])
}
Generator1a <- factor(rep(c("Nuclear","Wind","PV","CSP","Coal","NGCC","NGCT","UserTech1","UserTech2","Biomass","Geothermal","Petroleum","Hydro","Annual Storage"),times=(end.hour-start.hour+1)))
#Generator1a = factor(Generator1,levels=levels(Generator1)[c(5,12,11,7,6,9,2,1,4,3,10,13,8)]) #reorder
Generator1a = factor(Generator1a,levels=levels(Generator1a)[c(1,6,13,12,8,7,10,3,2,5,4,11,14,9)]) #reorder
data_stacked_area1a <- data.frame(Generation_MW,Hour,Generator1a)
p1a<-ggplot(data_stacked_area1a, aes(x=Hour, y=Generation_MW, fill=Generator1a)) +     geom_area() +
  labs(x = "Hour") + labs(y = "Generation (MW)") + guides(fill=guide_legend(title="Generator"))
tmp2 <- tempfile()
figure2 <- base64enc::base64encode("stacked_area_gen_w_storage.svg",tmp2)

outputs <- list('Figure1'=figure1,'Figure2'=figure2)
return(outputs)
}