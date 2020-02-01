#Last modified on Oct 2, 2019 by Jianwei Du


#------------------------------------------------#
# sub-function: elnl_generate                    #
#------------------------------------------------#

elnl_generate <- function(U, V, Y){
  
  # add "Rejected_Energy": el
  el <- edge_list(U=U, V=V, Y=Y, waste = "Rejected_Energy")[["Edge list"]]
  
  # modify data for visualization
  ## remove numerical precision for small values: el1
  el <- el %>% filter(abs(Value) >= 1e-10)
  el1 <- el[,1:4]
  
  ## change names for each end use sector: el1
  el1$To[grep("^Resident", el1$To)] <- "Residential"
  el1$To[grep("^Commerce", el1$To)] <- "Commercial"
  el1$To[grep("^Transport", el1$To)] <- "Transportation"
  el1$From[grep("^Resident", el1$From)] <- "Residential"
  el1$From[grep("^Commerce", el1$From)] <- "Commercial"
  el1$From[grep("^Transport", el1$From)] <- "Transportation"
  el1$Product[grep("?Resident", el1$Product)] <- "Services_Residential"
  el1$Product[grep("?Commerce", el1$Product)] <- "Services_Commercial"
  el1$Product[grep("?Transport", el1$Product)] <- "Services_Transportation"
  el1$To[grepl("?Plant", el1$To)] <- "Electricity"
  el1$To[el1$From == "Import_Net"] <- "Electricity"
  el1$Product[el1$From == "Import_Net"] <- "Electricity_Flow"
  el1$From[grepl("?Grid", el1$From)] <- "Electricity"
  
  ## store combined values of different energy types for each end use sector: el1
  Electricity_Waste <- sum(el1$Value[grepl("?Plant", el1$From) & el1$To == "Rejected_Energy"])
  Elec_Resident <- sum(el1$Value[el1$From == "Electricity" & el1$To == "Residential"])
  NG_Resident <- sum(el1$Value[el1$From == "Natural_Gas" & el1$To == "Residential"])
  Resident_Service <- sum(el1$Value[el1$From == "Residential" & el1$To == "Energy_Services"])
  Resident_Waste <- sum(el1$Value[el1$From == "Residential" & el1$To == "Rejected_Energy"])
  Elec_Commerce <- sum(el1$Value[el1$From == "Electricity" & el1$To == "Commercial"])
  NG_Commerce <- sum(el1$Value[el1$From == "Natural_Gas" & el1$To == "Commercial"])
  Commerce_Service <- sum(el1$Value[el1$From == "Commercial" & el1$To == "Energy_Services"])
  Commerce_Waste <- sum(el1$Value[el1$From == "Commercial" & el1$To == "Rejected_Energy"])
  Elec_Transport <- sum(el1$Value[el1$From == "Electricity" & el1$To == "Transportation"])
  Petrol_Transport <- sum(el1$Value[el1$From == "Petroleum" & el1$To == "Transportation"])
  NG_Transport <- sum(el1$Value[el1$From == "Natural_Gas" & el1$To == "Transportation"])
  Transport_Service <- sum(el1$Value[el1$From == "Transportation" & el1$To == "Energy_Services"])
  Transport_Waste <- sum(el1$Value[el1$From == "Transportation" & el1$To == "Rejected_Energy"])
  
  ## delete duplicated values: el2
  el2 <- el1 %>% subset(!(grepl("?Plant", el1$From)|grepl("?Grid", el1$To))) %>%
    subset(!((From == "Electricity" & To == "Transportation")|(From == "Petroleum" & To == "Transportation")|(From == "Natural_Gas" & To == "Transportation")|(From == "Transportation" & To == "Energy_Services")|(From == "Transportation" & To == "Rejected_Energy"))) %>% 
    subset(!((From == "Electricity" & To == "Residential")|(From == "Natural_Gas" & To == "Residential")|(From == "Residential" & To == "Energy_Services")|(From == "Residential" & To == "Rejected_Energy"))) %>%
    subset(!((From == "Electricity" & To == "Commercial")|(From == "Natural_Gas" & To == "Commercial")|(From == "Commercial" & To == "Energy_Services")|(From == "Commercial" & To == "Rejected_Energy")))
  
  ## add combined values: el3
  el3 <- el2 %>% rbind(data.frame(From = "Electricity", To = "Rejected_Energy", Value = Electricity_Waste, Product = "RejectedEnergy")) %>%
    rbind(data.frame(From = "Natural_Gas", To = "Residential", Value = NG_Resident, Product = "NaturalGas_Flow")) %>%
    rbind(data.frame(From = "Electricity", To = "Residential", Value = Elec_Resident, Product = "Electricity_Flow")) %>%
    rbind(data.frame(From = "Residential", To = "Energy_Services", Value = Resident_Service, Product = "Services_Residential")) %>%
    rbind(data.frame(From = "Residential", To = "Rejected_Energy", Value = Resident_Waste, Product = "Rejected_Energy")) %>%
    rbind(data.frame(From = "Natural_Gas", To = "Commercial", Value = NG_Commerce, Product = "NaturalGas_Flow")) %>%
    rbind(data.frame(From = "Electricity", To = "Commercial", Value = Elec_Commerce, Product = "Electricity_Flow")) %>%
    rbind(data.frame(From = "Commercial", To = "Energy_Services", Value = Commerce_Service, Product = "Services_Commercial")) %>%
    rbind(data.frame(From = "Commercial", To = "Rejected_Energy", Value = Commerce_Waste, Product = "Rejected_Energy")) %>%
    rbind(data.frame(From = "Electricity", To = "Transportation", Value = Elec_Transport, Product = "Electricity_Flow")) %>%
    rbind(data.frame(From = "Petroleum", To = "Transportation", Value = Petrol_Transport, Product = "Petroleum_Flow")) %>%
    rbind(data.frame(From = "Natural_Gas", To = "Transportation", Value = NG_Transport, Product = "NaturalGas_Flow")) %>%
    rbind(data.frame(From = "Transportation", To = "Energy_Services", Value = Transport_Service, Product = "Services_Transportation")) %>%
    rbind(data.frame(From = "Transportation", To = "Rejected_Energy", Value = Transport_Waste, Product = "Rejected_Energy"))
  
  ## order the primary energy: el4
  el4 <- el3[order(factor(el3$From, levels = c("Import_Net", "Solar", "Nuclear", "Hydro", "Wind", "Geothermal", "Natural_Gas", "Coal", "Biomass", "Petroleum", "Electricity", "Residential", "Commercial", "Industrial", "Transportation")), factor(el3$To, levels = c("Electricity", "Residential", "Commercial", "Industrial", "Transportation", "Energy_Services", "Rejected_Energy")), factor(el3$Product, levels = c("Import_Net_Electricity_Flow", "Solar_Flow", "Nuclear_Flow", "Hydro_Flow", "Wind_Flow", "Geothermal_Flow", "NaturalGas_Flow", "Coal_Flow", "Biomass_Flow", "Petroleum_Flow", "Electricity_Flow", "Services_Residential", "Services_Commercial", "Services_Industrial", "Services_Transportation", "Rejected_Energy"))),]
  
  ## generate edge list and node list: el5 and nl5
  el5 <- el4 %>% add_edge_ids() %>% add_node_ids()
  nl5 <- node_list(el5)
  
  ## set group for links and nodes: el5 and nl5
  el5$type <- el5$From
  el5$type[el5$To == "Energy_Services"] <- "Energy_Services"
  el5$type[el5$To == "Rejected_Energy"] <- "Rejected_Energy"
  nl5$type <- nl5$Node
  
  # generate list of edge_list and node_list
  result <- list("el" = el5, "nl" = nl5)
  
  return(result)

}