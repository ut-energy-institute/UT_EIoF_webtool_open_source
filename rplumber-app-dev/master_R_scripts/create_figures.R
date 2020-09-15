
#create_figures <- function(data){
#----
library(ggplot2) 
library(base64)
library(reshape2)
library(dplyr)
library(plyr)
library(base64enc)
library(svglite)

fig.wd <- 6	  
fig.ht <- 7

#----
colors <- c("#3D3BFF","#925E95","#FAE059","#BCA845","#A6F2A9","#A87D4C","#62BEF7","#AED7EF","#898989","#7AA37B","#D44148","black")
techs_no_storage <-c('Hour_ending','Hydro','Wind_DirectToGrid','PV_DirectToGrid','CSP_DirectToGrid','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear')
techs_w_storage <-c('Hour_ending','Hydro','Wind_DirectToGrid','PV_DirectToGrid','CSP_DirectToGrid','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear','WindSolar_Stored_then_Dispatched')
names(techs_no_storage) <- c('Hour_ending','Hydro','Wind','Solar PV','Solar CSP','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear')
names(techs_w_storage) <- c('Hour_ending','Hydro','Wind','Solar PV','Solar CSP','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear','Storage Dispatched')

#----
##############################################
#    Plotting Hourly Dispatch W/ Storage
#     Figure 1
##############################################
Hourly_MW_AnnualStorage <- data$Hourly_MW_AnnualStorage
Hourly_MW_AnnualStorage$Hour_ending <- (1:(7*4*24))

Hourly_MW_AnnualStorage$Load <- NULL
Hourly_MW_AnnualStorage$PV_curtailed <- NULL
Hourly_MW_AnnualStorage$CSP_curtailed <- NULL
Hourly_MW_AnnualStorage$Wind_curtailed <- NULL
Hourly_MW_AnnualStorage <- Hourly_MW_AnnualStorage[,techs_w_storage]
names(Hourly_MW_AnnualStorage) <- names(techs_w_storage)

Hourly_MW_AnnualStorage <- melt(Hourly_MW_AnnualStorage,c('Hour_ending'))
Hourly_MW_AnnualStorage <- Hourly_MW_AnnualStorage[with(Hourly_MW_AnnualStorage,order(Hour_ending,variable)), ]
figure1 <-  ggplot(Hourly_MW_AnnualStorage,aes(x=Hour_ending,y=value,fill=variable)) + 
  geom_area() + 
  scale_fill_manual(values=colors) + 
  labs(x='',y='Megawatts') + 
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) + 
  annotate('rect',xmin=1,
           xmax = 7*24,
           ymin = -Inf,
           ymax = Inf, fill = 'blue', alpha = 0.1) +
  annotate('rect',xmin=7*24*2,
           xmax = 7*24*3,
           ymin = -Inf,
           ymax = Inf, fill = 'blue', alpha = 0.1) + 
  annotate('text',x=75,y=45000,label='Winter') + 
  annotate('text',x=240,y=45000,label='Spring') +
  annotate('text',x=405,y=45000,label='Summer') +
  annotate('text',x=570,y=45000,label='Fall')

ggsave(filename="figure1.svg")#,height=fig.ht, width=fig.wd,pointsize=12)
tmp <- tempfile()  ## returns a vector of character strings which can be used as names for temporary files
figure1_enc <- base64enc::base64encode("figure1.svg",tmp)
# print(figure1)
#----

##############################################
#    Plotting Hourly Dispatch No Storage
#     Figure 2
##############################################
Hourly_MW_NoStorage <- data$Hourly_MW_NoStorage
Hourly_MW_NoStorage$Hour_ending <- (1:(7*4*24))


Hourly_MW_NoStorage$Load <- NULL
Hourly_MW_NoStorage$PV_curtailed <- NULL
Hourly_MW_NoStorage$CSP_curtailed <- NULL
Hourly_MW_NoStorage$Wind_curtailed <- NULL
Hourly_MW_NoStorage <- Hourly_MW_NoStorage[,techs_no_storage]
names(Hourly_MW_NoStorage) <- names(techs_no_storage)

Hourly_MW_NoStorage <- melt(Hourly_MW_NoStorage,c('Hour_ending'))
# Hourly_MW_NoStorage <- Hourly_MW_NoStorage[with(Hourly_MW_NoStorage,order(Hour_ending,variable)), ]
figure2 <-  ggplot(Hourly_MW_NoStorage,aes(x=Hour_ending,y=value,fill=variable)) + 
  geom_area() + 
  scale_fill_manual(values=colors) + 
  labs(x='',y='Megawatts') + 

  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) + 
  annotate('rect',xmin=1,
                xmax = 7*24,
                ymin = -Inf,
                ymax = Inf, fill = 'blue', alpha = 0.1) +
  annotate('rect',xmin=7*24*2,
                xmax = 7*24*3,
                ymin = -Inf,
                ymax = Inf, fill = 'blue', alpha = 0.1) + 
  annotate('text',x=75,y=45000,label='Winter') + 
  annotate('text',x=240,y=45000,label='Spring') +
  annotate('text',x=405,y=45000,label='Summer') +
  annotate('text',x=570,y=45000,label='Fall')

# print(figure2)

#----
##############################################
#    Plotting Annual Spending W/ Storage
#     Figure 3
##############################################

pp_capex <- data$ggsheets_output_AnnualStorage$capex$energy_sys_BillionsUSD
pp_opex <- data$ggsheets_output_AnnualStorage$opex$energy_sys_BillionsUSD
tran_capex <- data$ggsheets_output_AnnualStorage$capex$TandD
tran_opex <- data$ggsheets_output_AnnualStorage$opex$TandD
pet_ind <- data$ggsheets_output_AnnualStorage$oil_indus_spending$energy_sys_BillionsUSD
pet_transport <- data$ggsheets_output_AnnualStorage$oil_transport_spending$energy_sys_BillionsUSD
pet_comm <- data$ggsheets_output_AnnualStorage$oil_commerce_spending$energy_sys_BillionsUSD
pet_res <- data$ggsheets_output_AnnualStorage$oil_resid_spending$energy_sys_BillionsUSD
ng_ind <- data$ggsheets_output_AnnualStorage$ng_indus_spending$energy_sys_BillionsUSD
ng_transport <- data$ggsheets_output_AnnualStorage$ng_transport_spending$energy_sys_BillionsUSD
ng_comm <- data$ggsheets_output_AnnualStorage$ng_commerce_spending$energy_sys_BillionsUSD
ng_res <- data$ggsheets_output_AnnualStorage$ng_resid_spending$energy_sys_BillionsUSD
coal <- data$ggsheets_output_AnnualStorage$coal_not_elec$energy_sys_BillionsUSD

annual_spending <- rbind(pp_opex,pp_capex,tran_opex,tran_capex,pet_ind,pet_comm,pet_transport,pet_res,ng_ind,ng_comm,ng_transport,ng_res,coal)
spend_types <- c('Pwr Plants & Storage (O&M)','Pwr Plants & Storage (Capital)','Elec. Trans & Dist (O&M)','Elec. Trans & Dist (capital)','Petroleum (industrial)','Petroleum (commercial)','Petroleum (transportation)','Petroleum (residential)','NG (industrial)','NG (commercial)','NG (transportation)','NG (residential)','Coal (non-electricity)')
annual_spending$type <-spend_types
annual_spending$value <- NULL

annual_spending <- melt(annual_spending,c('type'))
annual_spending$variable <- as.numeric(as.character(annual_spending$variable))
#----
annual_spending$type <- factor(annual_spending$type,spend_types)
colors <- c('orange','#D5A10B','#E59879','#E13105','#2A9324','#2A9324','#2A9324','#2A9324','#92D6FF','#92D6FF','#92D6FF','#92D6FF','#898989')

figure3 <-  ggplot(annual_spending,aes(x=variable,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='$ Billions') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(size = 6)) + 
  
guides(shape = guide_legend(override.aes = list(size = .1)),
       color = guide_legend(override.aes = list(size = .1)))


# print(figure3)

#----

##############################################
#    Plotting Annual Spending No Storage
#     Figure 4
##############################################

pp_capex <- data$ggsheets_output_NoStorage$capex$energy_sys_BillionsUSD
pp_opex <- data$ggsheets_output_NoStorage$opex$energy_sys_BillionsUSD
tran_capex <- data$ggsheets_output_NoStorage$capex$TandD
tran_opex <- data$ggsheets_output_NoStorage$opex$TandD
pet_ind <- data$ggsheets_output_NoStorage$oil_indus_spending$energy_sys_BillionsUSD
pet_transport <- data$ggsheets_output_NoStorage$oil_transport_spending$energy_sys_BillionsUSD
pet_comm <- data$ggsheets_output_NoStorage$oil_commerce_spending$energy_sys_BillionsUSD
pet_res <- data$ggsheets_output_NoStorage$oil_resid_spending$energy_sys_BillionsUSD
ng_ind <- data$ggsheets_output_NoStorage$ng_indus_spending$energy_sys_BillionsUSD
ng_transport <- data$ggsheets_output_NoStorage$ng_transport_spending$energy_sys_BillionsUSD
ng_comm <- data$ggsheets_output_NoStorage$ng_commerce_spending$energy_sys_BillionsUSD
ng_res <- data$ggsheets_output_NoStorage$ng_resid_spending$energy_sys_BillionsUSD
coal <- data$ggsheets_output_NoStorage$coal_not_elec$energy_sys_BillionsUSD

annual_spending <- rbind(pp_opex,pp_capex,tran_opex,tran_capex,pet_ind,pet_comm,pet_transport,pet_res,ng_ind,ng_comm,ng_transport,ng_res,coal)
spend_types <- c('Pwr Plants & Storage (O&M)','Pwr Plants & Storage (Capital)','Elec. Trans & Dist (O&M)','Elec. Trans & Dist (capital)','Petroleum (industrial)','Petroleum (commercial)','Petroleum (transportation)','Petroleum (residential)','NG (industrial)','NG (commercial)','NG (transportation)','NG (residential)','Coal (non-electricity)')
annual_spending$type <-spend_types
annual_spending$value <- NULL

annual_spending <- melt(annual_spending,c('type'))
annual_spending$variable <- as.numeric(as.character(annual_spending$variable))
annual_spending$type <- factor(annual_spending$type,spend_types)
colors <- c('orange','#D5A10B','#E59879','#E13105','#2A9324','#2A9324','#2A9324','#2A9324','#92D6FF','#92D6FF','#92D6FF','#92D6FF','#898989')

figure4 <-  ggplot(annual_spending,aes(x=variable,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='$ Billions') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(size = 6)) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))


# print(figure4)

#----
######################################################
#    Plotting Annual Spending Per Person W/ Storage
#     Figure 5
######################################################

pp_capex <- data$ggsheets_output_AnnualStorage$all_pp_capex$USD_per_person
pp_opex <- data$ggsheets_output_AnnualStorage$all_pp_opex$USD_per_person
tran_capex <- data$ggsheets_output_AnnualStorage$TandD_capex$USD_per_person
tran_opex <- data$ggsheets_output_AnnualStorage$TandD_opex$USD_per_person
pet_ind <- data$ggsheets_output_AnnualStorage$oil_indus_spending$USD_per_person
pet_transport <- data$ggsheets_output_AnnualStorage$oil_transport_spending$USD_per_person
pet_comm <- data$ggsheets_output_AnnualStorage$oil_commerce_spending$USD_per_person
pet_res <- data$ggsheets_output_AnnualStorage$oil_resid_spending$USD_per_person
ng_ind <- data$ggsheets_output_AnnualStorage$ng_indus_spending$USD_per_person
ng_transport <- data$ggsheets_output_AnnualStorage$ng_transport_spending$USD_per_person
ng_comm <- data$ggsheets_output_AnnualStorage$ng_commerce_spending$USD_per_person
ng_res <- data$ggsheets_output_AnnualStorage$ng_resid_spending$USD_per_person
coal <- data$ggsheets_output_AnnualStorage$coal_not_elec$USD_per_person

annual_spending <- rbind(pp_opex,pp_capex,tran_opex,tran_capex,pet_ind,pet_comm,pet_transport,pet_res,ng_ind,ng_comm,ng_transport,ng_res,coal)
spend_types <- c('Pwr Plants & Storage (O&M)','Pwr Plants & Storage (Capital)','Elec. Trans & Dist (O&M)','Elec. Trans & Dist (capital)','Petroleum (industrial)','Petroleum (commercial)','Petroleum (transportation)','Petroleum (residential)','NG (industrial)','NG (commercial)','NG (transportation)','NG (residential)','Coal (non-electricity)')
annual_spending$type <-spend_types
annual_spending$value <- NULL

annual_spending <- melt(annual_spending,c('type'))
annual_spending$variable <- as.numeric(as.character(annual_spending$variable))

annual_spending$type <- factor(annual_spending$type,spend_types)
colors <- c('orange','#D5A10B','#E59879','#E13105','#2A9324','#2A9324','#2A9324','#2A9324','#92D6FF','#92D6FF','#92D6FF','#92D6FF','#898989')

figure5 <-  ggplot(annual_spending,aes(x=variable,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='$/person') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(size = 6)) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))


# print(figure5)

#----
######################################################
#    Plotting Annual Spending Per Person No Storage
#     Figure 6
######################################################

pp_capex <- data$ggsheets_output_NoStorage$all_pp_capex$USD_per_person
pp_opex <- data$ggsheets_output_NoStorage$all_pp_opex$USD_per_person
tran_capex <- data$ggsheets_output_NoStorage$TandD_capex$USD_per_person
tran_opex <- data$ggsheets_output_NoStorage$TandD_opex$USD_per_person
pet_ind <- data$ggsheets_output_NoStorage$oil_indus_spending$USD_per_person
pet_transport <- data$ggsheets_output_NoStorage$oil_transport_spending$USD_per_person
pet_comm <- data$ggsheets_output_NoStorage$oil_commerce_spending$USD_per_person
pet_res <- data$ggsheets_output_NoStorage$oil_resid_spending$USD_per_person
ng_ind <- data$ggsheets_output_NoStorage$ng_indus_spending$USD_per_person
ng_transport <- data$ggsheets_output_NoStorage$ng_transport_spending$USD_per_person
ng_comm <- data$ggsheets_output_NoStorage$ng_commerce_spending$USD_per_person
ng_res <- data$ggsheets_output_NoStorage$ng_resid_spending$USD_per_person
coal <- data$ggsheets_output_NoStorage$coal_not_elec$USD_per_person

annual_spending <- rbind(pp_opex,pp_capex,tran_opex,tran_capex,pet_ind,pet_comm,pet_transport,pet_res,ng_ind,ng_comm,ng_transport,ng_res,coal)
spend_types <- c('Pwr Plants & Storage (O&M)','Pwr Plants & Storage (Capital)','Elec. Trans & Dist (O&M)','Elec. Trans & Dist (capital)','Petroleum (industrial)','Petroleum (commercial)','Petroleum (transportation)','Petroleum (residential)','NG (industrial)','NG (commercial)','NG (transportation)','NG (residential)','Coal (non-electricity)')
annual_spending$type <-spend_types
annual_spending$value <- NULL

annual_spending <- melt(annual_spending,c('type'))
annual_spending$variable <- as.numeric(as.character(annual_spending$variable))

annual_spending$type <- factor(annual_spending$type,spend_types)
colors <- c('orange','#D5A10B','#E59879','#E13105','#2A9324','#2A9324','#2A9324','#2A9324','#92D6FF','#92D6FF','#92D6FF','#92D6FF','#898989')

figure6 <-  ggplot(annual_spending,aes(x=variable,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='$/person') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(size = 6)) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))


# print(figure6)

#----
######################################################
#    Plotting Annual Spending Per Person W/ Storage
#     Figure 7
######################################################

pp_capex <- data$ggsheets_output_AnnualStorage$all_pp_capex$USDperUSDofGDP_pct
pp_opex <- data$ggsheets_output_AnnualStorage$all_pp_opex$USDperUSDofGDP_pct
tran_capex <- data$ggsheets_output_AnnualStorage$TandD_capex$USDperUSDofGDP_pct
tran_opex <- data$ggsheets_output_AnnualStorage$TandD_opex$USDperUSDofGDP_pct
pet_ind <- data$ggsheets_output_AnnualStorage$oil_indus_spending$USDperUSDofGDP_pct
pet_transport <- data$ggsheets_output_AnnualStorage$oil_transport_spending$USDperUSDofGDP_pct
pet_comm <- data$ggsheets_output_AnnualStorage$oil_commerce_spending$USDperUSDofGDP_pct
pet_res <- data$ggsheets_output_AnnualStorage$oil_resid_spending$USDperUSDofGDP_pct
ng_ind <- data$ggsheets_output_AnnualStorage$ng_indus_spending$USDperUSDofGDP_pct
ng_transport <- data$ggsheets_output_AnnualStorage$ng_transport_spending$USDperUSDofGDP_pct
ng_comm <- data$ggsheets_output_AnnualStorage$ng_commerce_spending$USDperUSDofGDP_pct
ng_res <- data$ggsheets_output_AnnualStorage$ng_resid_spending$USDperUSDofGDP_pct
coal <- data$ggsheets_output_AnnualStorage$coal_not_elec$USDperUSDofGDP_pct

annual_spending <- rbind(pp_opex,pp_capex,tran_opex,tran_capex,pet_ind,pet_comm,pet_transport,pet_res,ng_ind,ng_comm,ng_transport,ng_res,coal)
spend_types <- c('Pwr Plants & Storage (O&M)','Pwr Plants & Storage (Capital)','Elec. Trans & Dist (O&M)','Elec. Trans & Dist (capital)','Petroleum (industrial)','Petroleum (commercial)','Petroleum (transportation)','Petroleum (residential)','NG (industrial)','NG (commercial)','NG (transportation)','NG (residential)','Coal (non-electricity)')
annual_spending$type <-spend_types
annual_spending$value <- NULL

annual_spending <- melt(annual_spending,c('type'))
annual_spending$variable <- as.numeric(as.character(annual_spending$variable))

annual_spending$type <- factor(annual_spending$type,spend_types)
colors <- c('orange','#D5A10B','#E59879','#E13105','#2A9324','#2A9324','#2A9324','#2A9324','#92D6FF','#92D6FF','#92D6FF','#92D6FF','#898989')

figure7 <-  ggplot(annual_spending,aes(x=variable,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='%/GDP') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(size = 6)) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))


# print(figure7)

#----
######################################################
#    Plotting Annual Spending Per Person No Storage
#     Figure 8
######################################################

pp_capex <- data$ggsheets_output_NoStorage$all_pp_capex$USDperUSDofGDP_pct
pp_opex <- data$ggsheets_output_NoStorage$all_pp_opex$USDperUSDofGDP_pct
tran_capex <- data$ggsheets_output_NoStorage$TandD_capex$USDperUSDofGDP_pct
tran_opex <- data$ggsheets_output_NoStorage$TandD_opex$USDperUSDofGDP_pct
pet_ind <- data$ggsheets_output_NoStorage$oil_indus_spending$USDperUSDofGDP_pct
pet_transport <- data$ggsheets_output_NoStorage$oil_transport_spending$USDperUSDofGDP_pct
pet_comm <- data$ggsheets_output_NoStorage$oil_commerce_spending$USDperUSDofGDP_pct
pet_res <- data$ggsheets_output_NoStorage$oil_resid_spending$USDperUSDofGDP_pct
ng_ind <- data$ggsheets_output_NoStorage$ng_indus_spending$USDperUSDofGDP_pct
ng_transport <- data$ggsheets_output_NoStorage$ng_transport_spending$USDperUSDofGDP_pct
ng_comm <- data$ggsheets_output_NoStorage$ng_commerce_spending$USDperUSDofGDP_pct
ng_res <- data$ggsheets_output_NoStorage$ng_resid_spending$USDperUSDofGDP_pct
coal <- data$ggsheets_output_NoStorage$coal_not_elec$USDperUSDofGDP_pct

annual_spending <- rbind(pp_opex,pp_capex,tran_opex,tran_capex,pet_ind,pet_comm,pet_transport,pet_res,ng_ind,ng_comm,ng_transport,ng_res,coal)
spend_types <- c('Pwr Plants & Storage (O&M)','Pwr Plants & Storage (Capital)','Elec. Trans & Dist (O&M)','Elec. Trans & Dist (capital)','Petroleum (industrial)','Petroleum (commercial)','Petroleum (transportation)','Petroleum (residential)','NG (industrial)','NG (commercial)','NG (transportation)','NG (residential)','Coal (non-electricity)')
annual_spending$type <-spend_types
annual_spending$value <- NULL

annual_spending <- melt(annual_spending,c('type'))
annual_spending$variable <- as.numeric(as.character(annual_spending$variable))

annual_spending$type <- factor(annual_spending$type,spend_types)
colors <- c('orange','#D5A10B','#E59879','#E13105','#2A9324','#2A9324','#2A9324','#2A9324','#92D6FF','#92D6FF','#92D6FF','#92D6FF','#898989')

figure8 <-  ggplot(annual_spending,aes(x=variable,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='%/GDP') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text  = element_text(size = 6)) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))


# print(figure8)
#----

##############################################
#    Plotting CO2 Emissions W/ Storage
##############################################
co2_coal <- data$ggsheets_output_AnnualStorage$MtCO2$coal
co2_petrolum <- data$ggsheets_output_AnnualStorage$MtCO2$petroleum
co2_ng <- data$ggsheets_output_AnnualStorage$MtCO2$NG
co2_pp <- data$ggsheets_output_AnnualStorage$MtCO2$embodied_in_PP

co2 <- rbind(co2_coal,co2_petrolum,co2_ng,co2_pp)
co2$type <- NULL
co2 <- melt(co2,'value')
names(co2) <- c('type','year','value')

# co2 <- co2[with(co2,order(c(year,value))), ]
# co2<-co2 %>% arrange(desc(value))
co2$year <- as.integer(as.character(co2$year))
co2$type[co2$type == 'coal'] <- 'Coal'
co2$type[co2$type == 'NG'] <- 'Natural Gas'
co2$type[co2$type == 'embodied_in_PP'] <- 'Power Plant Mfg./Constr'
co2$type[co2$type == 'petroleum'] <- 'Petroleum'

colors <- c('#898989','#92D6FF','#2A9324','orange')
figure9 <-  ggplot(co2,aes(x=year,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='Million Metric Tonnes of CO2') + 
  scale_fill_manual(values=colors) + 
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())
 
# print(figure9)

#----

##############################################
#    Plotting CO2 Emissions No Storage
##############################################
co2_coal_no_storage <- data$ggsheets_output_NoStorage$MtCO2$coal
co2_petrolum_no_storage <- data$ggsheets_output_NoStorage$MtCO2$petroleum
co2_ng_no_storage <- data$ggsheets_output_NoStorage$MtCO2$NG
co2_pp_no_storage <- data$ggsheets_output_NoStorage$MtCO2$embodied_in_PP

co2_no_storage <- rbind(co2_coal_no_storage,co2_petrolum_no_storage,co2_ng_no_storage,co2_pp_no_storage)
co2_no_storage$type <- NULL
co2_no_storage <- melt(co2_no_storage,'value')
names(co2_no_storage) <- c('type','year','value')


co2_no_storage$year <- as.integer(as.character(co2_no_storage$year))
co2_no_storage$type[co2_no_storage$type == 'coal'] <- 'Coal'
co2_no_storage$type[co2_no_storage$type == 'NG'] <- 'Natural Gas'
co2_no_storage$type[co2_no_storage$type == 'embodied_in_PP'] <- 'Power Plant Mfg./Constr'
co2_no_storage$type[co2_no_storage$type == 'petroleum'] <- 'Petroleum'

colors <- c('#898989','#92D6FF','#2A9324','orange')
figure10 <-  ggplot(co2_no_storage,aes(x=year,y=value,fill=type)) + 
  geom_area() +
  labs(x='',y='Million Metric Tonnes of CO2') + 
  scale_fill_manual(values=colors) + 
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())

# print(figure10)
#----
##############################################
#    Plotting Annual Capacity W/ Storage
#           Figure 11
##############################################
cap <- data$ggsheets_output_AnnualStorage$powerplantcapacity
cap <- ldply(cap, rbind)
cap$value <- c('Storage Capacity','Biomass','Coal','Geothermal','Hydro','NGCC','NGCT','Nuclear','Petroleum','Solar CSP','Solar PV','Total','Wind')
cap <- cap[-c(12),]
cap['.id'] <- NULL
cap['type'] <- NULL

cap <- melt(cap,'value')
names(cap) <- c('tech','year','value')
cap$year <- as.integer(as.character(cap$year))
cap$tech <- factor(cap$tech,c('Storage Capacity','Hydro','Wind','Solar PV','Solar CSP','Biomass','Coal','Geothermal','NGCC','NGCT','Petroleum','Nuclear'))
colors <- c('black',"#3D3BFF","#925E95","#FAE059","#BCA845","#A6F2A9","#898989","#A87D4C","#62BEF7","#AED7EF","#7AA37B","#D44148")

figure11 <-  ggplot(cap,aes(x=year,y=value,fill=tech)) + 
  geom_area() +
  labs(x='',y='Megawatt') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))

# print(figure11)
#----

##############################################
#    Plotting Annual Capacity No Storage
#           Figure 12
##############################################

cap <- data$ggsheets_output_NoStorage$powerplantcapacity
cap <- ldply(cap, rbind)
cap$value <- c('Storage Capacity','Biomass','Coal','Geothermal','Hydro','NGCC','NGCT','Nuclear','Petroleum','Solar CSP','Solar PV','Total','Wind')
cap <- cap[-c(1,12),]
cap['.id'] <- NULL
cap['type'] <- NULL

cap <- melt(cap,'value')
names(cap) <- c('tech','year','value')
cap$year <- as.integer(as.character(cap$year))
cap$tech <- factor(cap$tech,c('Hydro','Wind','Solar PV','Solar CSP','Biomass','Coal','Geothermal','NGCC','NGCT','Petroleum','Nuclear'))
colors <- c("#3D3BFF","#925E95","#FAE059","#BCA845","#A6F2A9","#898989","#A87D4C","#62BEF7","#AED7EF","#7AA37B","#D44148")

figure12 <-  ggplot(cap,aes(x=year,y=value,fill=tech)) + 
  geom_area() +
  labs(x='',y='Megawatt') + 
  scale_fill_manual(values=colors) +
  
  theme(axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) + 
  
  guides(shape = guide_legend(override.aes = list(size = .1)),
         color = guide_legend(override.aes = list(size = .1)))

# # print(figure12)
#----
#}


