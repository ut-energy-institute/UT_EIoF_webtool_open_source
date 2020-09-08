
#create_figures <- function(data){
  
library(ggplot2) 
library(base64)
library(reshape2)
library(dplyr)

#----
colors <- c("#3D3BFF","#925E95","#FAE059","#BCA845","#A6F2A9","#A87D4C","#62BEF7","#AED7EF","#898989","#7AA37B","#D44148","black")
techs_no_storage <-c('Hour_ending','Hydro','Wind_DirectToGrid','PV_DirectToGrid','CSP_DirectToGrid','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear')
techs_w_storage <-c('Hour_ending','Hydro','Wind_DirectToGrid','PV_DirectToGrid','CSP_DirectToGrid','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear','WindSolar_Stored_then_Dispatched')
names(techs_no_storage) <- c('Hour_ending','Hydro','Wind','Solar PV','Solar CSP','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear')
names(techs_w_storage) <- c('Hour_ending','Hydro','Wind','Solar PV','Solar CSP','Biomass','Geothermal','NGCC','NGCT','Coal','Petroleum','Nuclear','Storage Dispatched')

#----
Hourly_MW_AnnualStorage <- data$Hourly_MW_AnnualStorage
Hourly_MW_AnnualStorage$Hour_ending <- (1:(7*4*24))
Hourly_MW_AnnualStorage[1:(7*24),'season'] <- 'Winter'
Hourly_MW_AnnualStorage[(7*24):(7*2*24),'season'] <- 'Spring'
Hourly_MW_AnnualStorage[(7*2*24):(7*3*24),'season'] <- 'Summer'
Hourly_MW_AnnualStorage[(7*3*24):(7*4*24),'season'] <- 'Fall'

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

print(figure1)
#----
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

print(figure2)

#----
# capex <- data$ggsheets_output_AnnualStorage$capex
# opex <- data$ggsheets_output_AnnualStorage$opex

# spend_year_w_storage <- rbind(
#}


