## This script creates outputs that are needed to make the Sankey diagram after all
## calculations have been performed based on the user's chosen inputs for 2050.
## Inputs are:
## 1) TWh generated from each electricity technology (can be combination of (i) total TWh generation and (ii) fraction of generation from each technology)
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

generate_FinalUVY_2050 <- function(RegionNumber,percent_ResidentialHeatPump,percent_ResidentialNG,Hourly_MW_NoStorage,Hourly_MW_AnnualStorage,PPdata_NoStorage,PPdata_AnnualStorage,percent_ElectricLDV,LDVmiles_current_region_2050,Total_AnnualMWh_LDV_EVs) {

UserFraction.HeatPump = percent_ResidentialHeatPump/100 ## This is ultimately the user input
UserFraction.NG = percent_ResidentialNG/100 ## This is ultimately the user input
UserFraction.other = 1 - UserFraction.HeatPump - UserFraction.NG  ## This is technically the fraction of Residential Households using ANY OTHER FUEL (wood, petroleum, geothermal, etc.) and technology besides (1) NG furnaces and (2) electric heat pumps (with emergency resistance heating at very cold temperatures)

## +++++++++++
## Read baseline data for 
## (1) types of heating in homes in the baseline (ResStock simulation "base" run) case
## (2) the baseline annual energy used for household heating as (i) electricity, (ii) natural gas, and (iii) "other"=propane + fuel oil, from ResStock "base" run
## +++++++++++
load("generate8760_data/Baseline_ResStock_Fraction_HeatingTypes_byEIoF.Rdata")
load("generate_FinalUVY_2050_data/AnnualResidentialHeating_EIoF.Rdata")


## +++++++++++
## Inputs (that will be) coming from the user to be input as a function
## +++++++++++
Base.Fraction.HeatPump = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracHeatPump[RegionNumber]  ## Fraction of houses in "base" ResStock simulation that uses "heat pumps" for Residential Space Heating
Base.Fraction.NG = Baseline_ResStock_Fraction_HeatingTypes_byEIoF$FracNG[RegionNumber]  ## Fraction of houses in "base" ResStock simulation that uses "NG" for Residential Space Heating
Base.Fraction.petroleum = 1 - Base.Fraction.HeatPump - Base.Fraction.NG  ## Fraction of houses in "base" ResStock simulation that uses petroleum (fuel oil + propane) for Residential Space Heating

# Read existing "baseline" U, V, and Y matrices for 2050 (before user inputs)
# These data are in units of "Btu" consumed in the year 2050.
regions <- c('NW','CA','MN','SW','CE','TX','MW','AL','MA','SE','FL','NY','NE')  ## EIoF regions
Reg = regions[RegionNumber] 

load("generate_FinalUVY_2050_data/Base_UV_Matrices.Rdata")  ## This loads baseline U, V, and Y matrices with values independent of user's inputs
U2016_baseline = U_2016_list[[RegionNumber]]
U2050_PerUser = U_2050_list[[RegionNumber]]
V2050_PerUser = V_2050_list[[RegionNumber]]
U_NoStorage <- U2050_PerUser
V_NoStorage <- V2050_PerUser
U_AnnualStorage <- U2050_PerUser
V_AnnualStorage <- V2050_PerUser


# ##Set first column of matrices as row names
rownames(U_NoStorage) <- U_NoStorage[,1]
U_NoStorage <- U_NoStorage[,-1]
rownames(V_NoStorage) <- V_NoStorage[,1]
V_NoStorage <- V_NoStorage[,-1]
rownames(U_AnnualStorage) <- U_AnnualStorage[,1]
U_AnnualStorage <- U_AnnualStorage[,-1]
rownames(V_AnnualStorage) <- V_AnnualStorage[,1]
V_AnnualStorage <- V_AnnualStorage[,-1]
rownames(U2050_PerUser) <- U2050_PerUser[,1]
U2050_PerUser <- U2050_PerUser[,-1]
rownames(V2050_PerUser) <- V2050_PerUser[,1]
V2050_PerUser <- V2050_PerUser[,-1]

## +++++++++++
## Read heat rate data to convert electricity generation desired (in TWh) into Btu
## These are the assumed heat rates in EIA's Annual Energy Outlook 2019 reference case.
## The heat rates for NG, coal, and petroleum for 2018-2049 are linearly interpolated between values calculated from for 2017 and 2050,
## using data from the EIA reference case runs "set1.1116a", Datekey = "d111618a", in supplementary tables 2 and 3 that have:
## Census level "Energy Consumption by Sector and Source - Middle Atlantic" (for example).
## +++++++++++
load("generate_FinalUVY_2050_data/heat_rates.Rdata")
heat_rate_AvgFossil.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="average_fossilfuel_AEO2019reference")]  ## heat rate in units of "btu/kWh"
heat_rate_biomass.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="biomass")] ## heat rate in units of "btu/kWh"
heat_rate_nuclear.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="nuclear")] ## heat rate in units of "btu/kWh"
heat_rate_geothermal.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="geothermal")] ## heat rate in units of "btu/kWh"
heat_rate_coal.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="coal_linear_2017_to_2050")] ## heat rate in units of "btu/kWh"
heat_rate_NG.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="naturalgas_linear_2017_to_2050")] ## heat rate in units of "btu/kWh"
heat_rate_petroleum.2050 = heat_rate$X2050[which(heat_rate$HeatRates_btu_per_kwh=="petroleum_linear_2017_to_2050")] ## heat rate in units of "btu/kWh"
Btu_per_kwh_engineering = 3412.14 #3412.14  This is the assumed pure engineering conversion from kWh to Btu

## ++++++++++++++++++
## No Storage case (U matrix)
## ++++++++++++++++++
## Adjust values associated with Power Plants
U_NoStorage['Solar_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="PV")]*Btu_per_kwh_engineering*1e9 + PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="CSP")]*Btu_per_kwh_engineering*1e9  ## This shoud end up in units of Btu
U_NoStorage['Nuclear_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Nuclear")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Hydro_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="HydroDispatch")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Wind_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Wind")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Geothermal_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Geothermal")]*Btu_per_kwh_engineering*1e9
U_NoStorage['NaturalGas_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="NGCC")]*Btu_per_kwh_engineering*1e9 + PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="NGCT")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Coal_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Coal")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Biomass_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Biomass")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Petroleum_Electricity', 'Electricity_Grid']= PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="PetroleumCC")]*Btu_per_kwh_engineering*1e9
U_NoStorage['Import_Net_Electricity', 'Electricity_Grid'] = 0 ## We do assume imports (of wind and solar electricity) from other regions, but are not specifically calculating it in this cell
U_NoStorage['Solar_Flow','Solar_Plant']=U_NoStorage['Solar_Electricity', 'Electricity_Grid']*(heat_rate_AvgFossil.2050/Btu_per_kwh_engineering)  ## This is units = quads*( (Btu/kwh EIA conversion) / (Btu/kWh engineering equvalent)) = quads 
U_NoStorage['Nuclear_Flow','Nuclear_Plant']=U_NoStorage['Nuclear_Electricity', 'Electricity_Grid']*(heat_rate_nuclear.2050/Btu_per_kwh_engineering) 
U_NoStorage['Hydro_Flow','Hydro_Plant']=U_NoStorage['Hydro_Electricity', 'Electricity_Grid']*(heat_rate_AvgFossil.2050/Btu_per_kwh_engineering)
U_NoStorage['Wind_Flow', 'Wind_Plant']=U_NoStorage['Wind_Electricity', 'Electricity_Grid']*(heat_rate_AvgFossil.2050/Btu_per_kwh_engineering) 
U_NoStorage['Geothermal_Flow', 'Geothermal_Plant']=U_NoStorage['Geothermal_Electricity', 'Electricity_Grid']*(heat_rate_geothermal.2050/Btu_per_kwh_engineering) 
U_NoStorage['NaturalGas_Flow', 'Natural_Gas_Plant']=U_NoStorage['NaturalGas_Electricity', 'Electricity_Grid']*(heat_rate_NG.2050/Btu_per_kwh_engineering) 
U_NoStorage['Coal_Flow', 'Coal_Plant']=U_NoStorage['Coal_Electricity', 'Electricity_Grid']*(heat_rate_coal.2050/Btu_per_kwh_engineering) 
U_NoStorage['Biomass_Flow', 'Biomass_Plant']=U_NoStorage['Biomass_Electricity', 'Electricity_Grid']*(heat_rate_biomass.2050/Btu_per_kwh_engineering) 
U_NoStorage['Petroleum_Flow', 'Petroleum_Plant']=U_NoStorage['Petroleum_Electricity', 'Electricity_Grid']*(heat_rate_petroleum.2050/Btu_per_kwh_engineering) 

## +++
## Adjust values associated with Residential Heating
## +++
## Add changes to residential heating per user inputs
## From creating the "baseline" 2050 U and V matrices, we know how much NG, fuel oil, and propane
## is needed for 'Resident_SpaceHeating_XXX'.

## NOTE: By design the online tool only allows the user to increase (1) NG heating or (2) electric heating (via heat pumps)
## The user is not allowed to increase heating from "other" means, and this includes heating via electric furnaces, boilers, and other "electrical resisitance" heaters
## THere are three equations to solve for three unknown weightings of the three ResStock runs: weight.base, weight.NG, & weight.HeatPump
## Constraint on user "NG" choice, Eqn (1):       Base.Fraction.NG*weight.base + weight.NG = UserFraction.NG
## Constraint on user "HeatPump" choice, Eqn (2): Base.Fraction.HeatPump*weight.base + weight.HeatPump = UserFraction.HeatPump
## Constraint on user implied "other" that is petroleum, Eqn (3):   Base.Fraction.petroleum*weight.base = 1 - UserFraction.NG - UserFraction.HeatPump
## Solving these three equations is as follows (Each of Equations (1) and (2) can be solved once Equation 3 is solved)
if ((UserFraction.HeatPump + UserFraction.NG) < (Base.Fraction.HeatPump+Base.Fraction.NG)) {
  stop("Error: the user is not allowed to specify heating methods such that the fracton of homes using (1) natural gas and (2) heat pumps for heating is lower than in the baseline assumed housing stock.")
}
weight.base = (1-UserFraction.HeatPump-UserFraction.NG)/Base.Fraction.petroleum ## The weighting given to the "base" ResStock results to which to add to the "NG" and "HeatPump" ResStock results
weight.HeatPump = UserFraction.HeatPump - Base.Fraction.HeatPump*weight.base ## The weighting given to the "NG" ResStock results to which to add to the "base" and "HeatPump" ResStock results 
weight.NG = UserFraction.NG - Base.Fraction.NG*weight.base ## The weighting given to the "HeatPump" ResStock results to which to add to the "NG" and "base" ResStock results
Btu_per_therm = 99976.1

## +++
## Now calculate the changes to the U and V matrices for residential space heating, as 
## determined by the user, for (1) electricity, (2) natural gas, and (3) "other" = propane + fuel oil.
## NOTE: In the real world (and EIA SEDS data), "other" fuels include biomass, solar, geothermal but these other fuels are not included in the ResStock simulations.
## +++
AnnualResidentialHeating_now <- AnnualResidentialHeating_EIoF_2050[which(AnnualResidentialHeating_EIoF_2050$Region==regions[RegionNumber]),]
HH_heating_NG <- weight.base*AnnualResidentialHeating_now$ResStock_baserun_NG_btu + 
  weight.NG*AnnualResidentialHeating_now$ResStock_NGrun_NG_btu + 
  weight.HeatPump*AnnualResidentialHeating_now$ResStock_HeatPumprun_NG_btu
HH_heating_petroleum <- weight.base*AnnualResidentialHeating_now$ResStock_baserun_other_btu + 
  weight.NG*AnnualResidentialHeating_now$ResStock_NGrun_other_btu + 
  weight.HeatPump*AnnualResidentialHeating_now$ResStock_HeatPumprun_other_btu
HH_heating_elec <- weight.base*AnnualResidentialHeating_now$ResStock_baserun_elec_btu + 
  weight.NG*AnnualResidentialHeating_now$ResStock_NGrun_elec_btu + 
  weight.HeatPump*AnnualResidentialHeating_now$ResStock_HeatPumprun_elec_btu
U_NoStorage['NaturalGas_Flow', 'Resident_SpaceHeating_NG']=HH_heating_NG
U_NoStorage['Electricity_Flow', 'Resident_SpaceHeating_Elec']=HH_heating_elec

## +++
## Petroleum (and other) fuels are used for (1) household heating and (2) other uses (e.g., cooking) so we must only adjust the portion of ['Petroleum_Flow', 'Resident_Other'] that is associated with residential space heating
## NOTE: The code "Create_Sankey_Inputs_for2050_AEOCensusDivisions.R" is what sets up the 2050 projections
## for Census Divisions, that then inform the U and V Sankey matrices for 2050 EIoF Regions. And in that
## code, the 2050 values have been scaled to projections from the EIA AEO 2019 "reference case" scneario.
## Thus, to we adjust the "baseline" 2050 assumed consumption of "other" fuels for residential heating by the 
## fraction = (UserFraction.other/Base.Fraction.petroleum).  The online interface will be programmed
## to prevent "UserFraction.other" from being >  "Base.Fraction.petroleum"
## +++
U_NoStorage['Petroleum_Flow', 'Resident_Other'] = (UserFraction.other/Base.Fraction.petroleum)*U2050_PerUser['Petroleum_Flow', 'Resident_Other']
U_NoStorage['Biomass_Flow', 'Resident_Other']=(UserFraction.other/Base.Fraction.petroleum)*U2050_PerUser['Biomass_Flow', 'Resident_Other']
U_NoStorage['Coal_Flow', 'Resident_Other']=(UserFraction.other/Base.Fraction.petroleum)*U2050_PerUser['Coal_Flow', 'Resident_Other']
U_NoStorage['Geothermal_Flow', 'Resident_Other']=(UserFraction.other/Base.Fraction.petroleum)*U2050_PerUser['Geothermal_Flow', 'Resident_Other']
##U_NoStorage['NaturalGas_Flow', 'Resident_Other']=U2050_PerUser['NaturalGas_Flow', 'Resident_Other'] ## Make NO changes to 'Resident_Other' for Natural Gas flow because NG for space heating is changed under column 'Resident_SpaceHeating_NG'

## +++
## Adjust values associated with Electric Vehicles (EVs)
## FUNCTION INPUTS FOR THIS CALCULATION
## 1. Total_AnnualMWh_LDV_EVs = the annual MWh for charging LDVs in this user scenario
## 2. percent_ElectricLDV = % (0 to 100) of annual LDV miles driven on electricity
## 3. LDVmiles_current_region_2050 = the LDVs miles driven for the current region
## +++
gallon_per_BBL = 42
btu_per_gallon_gasoline = 5.054*1e6/gallon_per_BBL ## BTUs in one gallon of gasoline, EIA Table A3 of Monthly Energy Review, for year 2019, states 5.054 Million Btu per BBL of gasoline
btu_per_gallon_diesel = 5.772*1e6/gallon_per_BBL ## BTUs in one gallon of diesel (Distillate Fuel Oil), EIA Table A3 of Monthly Energy Review, for year 2019, states 5.772 Million Btu per BBL of DFO
fraction_gasoline = 7.13/(7.13+3.97) ## "fraction of petroluem fuels that is gasoline": EIA 2019 Reference case, Table 11. Petroleum and Other Liquids Supply and Disposition, calculates in 2050, 7.13 million BBL/day of gasoline consumption and 3.97 million BBL/day of Distilate Fuel Oil (includes biodiesel) for ALL sectors and uses (not only transportation or only LDVs)
btu_per_gallon_petroleum = fraction_gasoline*btu_per_gallon_gasoline + (1-fraction_gasoline)*btu_per_gallon_diesel  ## BTUs in one gallon of average petroleum fuel (diesel, gasoline)
btu_per_gallon_biodiesel = btu_per_gallon_diesel
btu_per_gallon_ethanol = 3.553*1e6/gallon_per_BBL ## BTUs in one gallon of ethanol.  EIA Table A3 of Monthly Energy Review, for year 2019, states 3.553 Million Btu per BBL of ethanol
fraction_ethanol = 0.88/(0.88+0.13) ## "fraction of biofuels that is ethanol": EIA 2019 Reference case, Table 11. Petroleum and Other Liquids Supply and Disposition, calculates in 2050, 0.88 million BBL/day of ethanol consumption and 0.13 million BBL/day of biodiesel for ALL sectors and uses (not only transportation or only LDVs)
btu_per_gallon_biofuel = fraction_ethanol*btu_per_gallon_ethanol + (1-fraction_ethanol)*btu_per_gallon_biodiesel ## BTUs in one gallon of average biofuel fuel (biodiesel, ethanol).
EIA_AEO2019_LDVmpg_2050 = 38.541634  ## miles per gallon average for all light duty vehicles (using liquid fuels) in 2050
fraction_LDVmiles_petrol_plus_biofuel = 1 - percent_ElectricLDV/100  ## percent of LDV miles driving on average liquid fuel mix (petroleum + biofuels)
fraction_LDVmiles_petrol = 0.9*fraction_LDVmiles_petrol_plus_biofuel  ## assume that miles driven on average liquid fuel (petroleum + biofuel) are 90% due to petroleum
fraction_LDVmiles_biofuel = 1 - percent_ElectricLDV/100 - fraction_LDVmiles_petrol
ldv_petrol_btu =  fraction_LDVmiles_petrol*LDVmiles_current_region_2050/EIA_AEO2019_LDVmpg_2050*btu_per_gallon_petroleum ## Quadrillion Btu of petroleum consumed for LDV miles 
ldv_biofuel_btu = fraction_LDVmiles_biofuel*LDVmiles_current_region_2050/EIA_AEO2019_LDVmpg_2050*btu_per_gallon_biofuel ## Quadrillion Btu of biofuel consumed for LDV miles 
Total_Btu_LDV_petrol = U_NoStorage['Petroleum_Flow','Transport_LDV_Petrol']  ## baseline value of petroleum energy for LDVs BEFORE user's inputs
Total_Btu_LDV_biofuel = U_NoStorage['Biomass_Flow','Transport_LDV_Ethanol']  ## baseline value of biofuel energy for LDVs BEFORE user's inputs
Total_Btu_LDV_EVs = Total_AnnualMWh_LDV_EVs*1e3*Btu_per_kwh_engineering
ldv_elec_quads = Total_Btu_LDV_EVs
#ldv_btu_total = (ldv_elec_quads + ldv_petrol_quads + ldv_biofuel_quads)
U_NoStorage['Electricity_Flow', 'Transport_LDV_Elec'] = Total_Btu_LDV_EVs  ## Btus of electricity for charging EVs
U_NoStorage['Petroleum_Flow','Transport_LDV_Petrol'] = ldv_petrol_btu  ## baseline value of petroleum energy for LDVs BEFORE user's inputs
U_NoStorage['Biomass_Flow','Transport_LDV_Ethanol'] = ldv_biofuel_btu ## baseline value of biofuel energy for LDVs BEFORE user's inputs
# percent_ldv_elec_quads = 100*ldv_elec_quads/ldv_quads_total
# percent_ldv_petrol_quads = 100*ldv_petrol_quads/ldv_quads_total
# percent_ldv_biofuel_quads = 100*ldv_biofuel_quads/ldv_quads_total


## ++++++++++++++++++
## No Storage case (V matrix)
## ++++++++++++++++++
V_NoStorage['Solar', 'Solar_Flow']= sum(U_NoStorage['Solar_Flow',]) ## Total primary solar energy = total 'Solar_Flow' in Use (U) matrix
V_NoStorage['Solar_Plant', 'Solar_Electricity']=U_NoStorage['Solar_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='SOEGP']*Btu_per_kwh_engineering)/1000
V_NoStorage['Nuclear', 'Nuclear_Flow']=sum(U_NoStorage['Nuclear_Flow',]) ## Total primary nuclear energy = total 'Nuclear_Flow' in Use (U) matrix
V_NoStorage['Nuclear_Plant', 'Nuclear_Electricity']=U_NoStorage['Nuclear_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='NUEGP']*Btu_per_kwh_engineering)/1000
V_NoStorage['Hydro', 'Hydro_Flow']=sum(U_NoStorage['Hydro_Flow',]) ## Total primary hydro energy = total 'Hydro_Flow' in Use (U) matrix
V_NoStorage['Hydro_Plant', 'Hydro_Electricity']=U_NoStorage['Hydro_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='HYEGP']*Btu_per_kwh_engineering)/1000
V_NoStorage['Wind', 'Wind_Flow']=sum(U_NoStorage['Wind_Flow',]) ## Total primary wind energy = total 'Wind_Flow' in Use (U) matrix
V_NoStorage['Wind_Plant', 'Wind_Electricity']=U_NoStorage['Wind_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='WYEGP']*Btu_per_kwh_engineering)/1000
V_NoStorage['Geothermal', 'Geothermal_Flow']=sum(U_NoStorage['Geothermal_Flow',]) ## Total primary geothermal energy = total 'Geothermal_Flow' in Use (U) matrix
V_NoStorage['Geothermal_Plant', 'Geothermal_Electricity']=U_NoStorage['Geothermal_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='GEEGP']*Btu_per_kwh_engineering)/1000
V_NoStorage['Natural_Gas', 'NaturalGas_Flow']=sum(U_NoStorage['NaturalGas_Flow',]) ## Total primary NG energy = total 'NaturalGas_Flow' in Use (U) matrix
V_NoStorage['Natural_Gas_Plant', 'NaturalGas_Electricity']=U_NoStorage['NaturalGas_Electricity', 'Electricity_Grid']  ## Original data read from EIA are in units of 1000s MWh, this converts to Billions of Btu
V_NoStorage['Coal', 'Coal_Flow']=sum(U_NoStorage['Coal_Flow',]) ## Total primary coal energy = total 'Coal_Flow' in Use (U) matrix
V_NoStorage['Coal_Plant', 'Coal_Electricity']=U_NoStorage['Coal_Electricity', 'Electricity_Grid']
V_NoStorage['Biomass', 'Biomass_Flow']=sum(U_NoStorage['Biomass_Flow',]) ## Total primary biomass energy = total 'Biomass_Flow' in Use (U) matrix
V_NoStorage['Biomass_Plant', 'Biomass_Electricity']=U_NoStorage['Biomass_Electricity', 'Electricity_Grid']
V_NoStorage['Petroleum', 'Petroleum_Flow']=sum(U_NoStorage['Petroleum_Flow',]) ## Total primary petroleum energy = total 'Petroleum_Flow' in Use (U) matrix
V_NoStorage['Petroleum_Plant', 'Petroleum_Electricity']=U_NoStorage['Petroleum_Electricity', 'Electricity_Grid']
V_NoStorage['Import_Net', 'Import_Net_Electricity']=U_NoStorage['Import_Net_Electricity', 'Electricity_Grid']
# V_NoStorage['Resident_SpaceHeating_NG', 'Services_Resident_SpaceHeating_NG']=0  ## No numbers to input (or update)
# V_NoStorage['Resident_SpaceHeating_Elec', 'Services_Resident_SpaceHeating_Elec']=0  ## No numbers to input (or update)
# V_NoStorage['Transport_LDV_Petrol', 'Services_Transport_LDV_Petrol']=0
# V_NoStorage['Transport_LDV_Elec', 'Services_Transport_LDV_Elec']=0
# V_NoStorage['Transport_LDV_Ethanol', 'Services_Transport_LDV_Ethanol']=0
# V_NoStorage['Transport_Other_Petrol', 'Services_Transport_Other_Petrol']=0
# V_NoStorage['Transport_Other_NG', 'Services_Transport_Other_NG']=0
# V_NoStorage['Transport_Other_Other', 'Services_Transport_Other_Other']=0


## ++++++++++++++++++
## Annual Storage case (U matrix)
## ++++++++++++++++++
## +++
## Adjust values associated with Power Plants
## +++
U_AnnualStorage['Solar_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="PV")]*Btu_per_kwh_engineering*1e9 + PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="CSP")]*Btu_per_kwh_engineering*1e9  ## This shoud end up in units of Btu
U_AnnualStorage['Nuclear_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Nuclear")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Hydro_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="HydroDispatch")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Wind_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Wind")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Geothermal_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Geothermal")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['NaturalGas_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="NGCC")]*Btu_per_kwh_engineering*1e9 + PPdata_NoStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="NGCT")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Coal_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Coal")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Biomass_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="Biomass")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Petroleum_Electricity', 'Electricity_Grid']= PPdata_AnnualStorage$TWhGeneration[which(PPdata_NoStorage$Technology=="PetroleumCC")]*Btu_per_kwh_engineering*1e9
U_AnnualStorage['Import_Net_Electricity', 'Electricity_Grid'] = 0 ## We do assume imports (of wind and solar electricity) from other regions, but are not specifically calculating it in this cell
U_AnnualStorage['Solar_Flow','Solar_Plant']=U_AnnualStorage['Solar_Electricity', 'Electricity_Grid']*(heat_rate_AvgFossil.2050/Btu_per_kwh_engineering)  ## This is units = quads*( (Btu/kwh EIA conversion) / (Btu/kWh engineering equvalent)) = quads 
U_AnnualStorage['Nuclear_Flow','Nuclear_Plant']=U_AnnualStorage['Nuclear_Electricity', 'Electricity_Grid']*(heat_rate_nuclear.2050/Btu_per_kwh_engineering) 
U_AnnualStorage['Hydro_Flow','Hydro_Plant']=U_AnnualStorage['Hydro_Electricity', 'Electricity_Grid']*(heat_rate_AvgFossil.2050/Btu_per_kwh_engineering)
U_AnnualStorage['Wind_Flow', 'Wind_Plant']=U_AnnualStorage['Wind_Electricity', 'Electricity_Grid']*(heat_rate_AvgFossil.2050/Btu_per_kwh_engineering) 
U_AnnualStorage['Geothermal_Flow', 'Geothermal_Plant']=U_AnnualStorage['Geothermal_Electricity', 'Electricity_Grid']*(heat_rate_geothermal.2050/Btu_per_kwh_engineering) 
U_AnnualStorage['NaturalGas_Flow', 'Natural_Gas_Plant']=U_AnnualStorage['NaturalGas_Electricity', 'Electricity_Grid']*(heat_rate_NG.2050/Btu_per_kwh_engineering) 
U_AnnualStorage['Coal_Flow', 'Coal_Plant']=U_AnnualStorage['Coal_Electricity', 'Electricity_Grid']*(heat_rate_coal.2050/Btu_per_kwh_engineering) 
U_AnnualStorage['Biomass_Flow', 'Biomass_Plant']=U_AnnualStorage['Biomass_Electricity', 'Electricity_Grid']*(heat_rate_biomass.2050/Btu_per_kwh_engineering) 
U_AnnualStorage['Petroleum_Flow', 'Petroleum_Plant']=U_AnnualStorage['Petroleum_Electricity', 'Electricity_Grid']*(heat_rate_petroleum.2050/Btu_per_kwh_engineering) 

## Other and Residential energy demands are the same for "NoStorage" and "AnnualStorage" of electricity on electric grid
U_AnnualStorage['NaturalGas_Flow', 'Resident_SpaceHeating_NG']=U_NoStorage['NaturalGas_Flow', 'Resident_SpaceHeating_NG']
U_AnnualStorage['Electricity_Flow', 'Resident_SpaceHeating_Elec']=U_NoStorage['Electricity_Flow', 'Resident_SpaceHeating_Elec']
U_AnnualStorage['Petroleum_Flow', 'Resident_Other'] = U_NoStorage['Petroleum_Flow', 'Resident_Other']
U_AnnualStorage['Biomass_Flow', 'Resident_Other']=U_NoStorage['Biomass_Flow', 'Resident_Other']
U_AnnualStorage['Coal_Flow', 'Resident_Other']=U_NoStorage['Coal_Flow', 'Resident_Other']
U_AnnualStorage['Geothermal_Flow', 'Resident_Other']=U_NoStorage['Geothermal_Flow', 'Resident_Other']

## +++
## Adjust values associated with Transportation (LDVs specfically need updating)
## +++
U_AnnualStorage['Electricity_Flow', 'Transport_LDV_Elec'] = U_NoStorage['Electricity_Flow', 'Transport_LDV_Elec'] ## Btus of electricity for charging EVs
U_AnnualStorage['Petroleum_Flow','Transport_LDV_Petrol'] = U_NoStorage['Petroleum_Flow', 'Transport_LDV_Petrol']  ## baseline value of petroleum energy for LDVs BEFORE user's inputs
U_AnnualStorage['Biomass_Flow','Transport_LDV_Ethanol'] = U_NoStorage['Biomass_Flow','Transport_LDV_Ethanol']  ## baseline value of biofuel energy for LDVs BEFORE user's inputs


## ++++++++++++++++++
## Annual Storage case (V matrix)
## ++++++++++++++++++
V_AnnualStorage['Solar', 'Solar_Flow']= sum(U_AnnualStorage['Solar_Flow',]) ## Total primary solar energy = total 'Solar_Flow' in Use (U) matrix
V_AnnualStorage['Solar_Plant', 'Solar_Electricity']=U_AnnualStorage['Solar_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='SOEGP']*Btu_per_kwh_engineering)/1000
V_AnnualStorage['Nuclear', 'Nuclear_Flow']=sum(U_AnnualStorage['Nuclear_Flow',]) ## Total primary nuclear energy = total 'Nuclear_Flow' in Use (U) matrix
V_AnnualStorage['Nuclear_Plant', 'Nuclear_Electricity']=U_AnnualStorage['Nuclear_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='NUEGP']*Btu_per_kwh_engineering)/1000
V_AnnualStorage['Hydro', 'Hydro_Flow']=sum(U_AnnualStorage['Hydro_Flow',]) ## Total primary hydro energy = total 'Hydro_Flow' in Use (U) matrix
V_AnnualStorage['Hydro_Plant', 'Hydro_Electricity']=U_AnnualStorage['Hydro_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='HYEGP']*Btu_per_kwh_engineering)/1000
V_AnnualStorage['Wind', 'Wind_Flow']=sum(U_AnnualStorage['Wind_Flow',]) ## Total primary wind energy = total 'Wind_Flow' in Use (U) matrix
V_AnnualStorage['Wind_Plant', 'Wind_Electricity']=U_AnnualStorage['Wind_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='WYEGP']*Btu_per_kwh_engineering)/1000
V_AnnualStorage['Geothermal', 'Geothermal_Flow']=sum(U_AnnualStorage['Geothermal_Flow',]) ## Total primary geothermal energy = total 'Geothermal_Flow' in Use (U) matrix
V_AnnualStorage['Geothermal_Plant', 'Geothermal_Electricity']=U_AnnualStorage['Geothermal_Electricity', 'Electricity_Grid'] ##(newSEDS$Data[newSEDS$MSN=='GEEGP']*Btu_per_kwh_engineering)/1000
V_AnnualStorage['Natural_Gas', 'NaturalGas_Flow']=sum(U_AnnualStorage['NaturalGas_Flow',]) ## Total primary NG energy = total 'NaturalGas_Flow' in Use (U) matrix
V_AnnualStorage['Natural_Gas_Plant', 'NaturalGas_Electricity']=U_AnnualStorage['NaturalGas_Electricity', 'Electricity_Grid']  ## Original data read from EIA are in units of 1000s MWh, this converts to Billions of Btu
V_AnnualStorage['Coal', 'Coal_Flow']=sum(U_AnnualStorage['Coal_Flow',]) ## Total primary coal energy = total 'Coal_Flow' in Use (U) matrix
V_AnnualStorage['Coal_Plant', 'Coal_Electricity']=U_AnnualStorage['Coal_Electricity', 'Electricity_Grid']
V_AnnualStorage['Biomass', 'Biomass_Flow']=sum(U_AnnualStorage['Biomass_Flow',]) ## Total primary biomass energy = total 'Biomass_Flow' in Use (U) matrix
V_AnnualStorage['Biomass_Plant', 'Biomass_Electricity']=U_AnnualStorage['Biomass_Electricity', 'Electricity_Grid']
V_AnnualStorage['Petroleum', 'Petroleum_Flow']=sum(U_AnnualStorage['Petroleum_Flow',]) ## Total primary petroleum energy = total 'Petroleum_Flow' in Use (U) matrix
V_AnnualStorage['Petroleum_Plant', 'Petroleum_Electricity']=U_AnnualStorage['Petroleum_Electricity', 'Electricity_Grid']
V_AnnualStorage['Import_Net', 'Import_Net_Electricity']=U_AnnualStorage['Import_Net_Electricity', 'Electricity_Grid']
# V_AnnualStorage['Resident_SpaceHeating_NG', "Services_Resident_SpaceHeating_NG"]=0  ## No numbers to input (or update)
# V_AnnualStorage['Resident_SpaceHeating_Elec', "Services_Resident_SpaceHeating_Elec"]=0  ## No numbers to input (or update)
# V_AnnualStorage['Transport_LDV_Petrol', 'Services_Transport_LDV_Petrol']=0
# V_AnnualStorage['Transport_LDV_Elec', 'Services_Transport_LDV_Elec']=0
# V_AnnualStorage['Transport_LDV_Ethanol', 'Services_Transport_LDV_Ethanol']=0
# V_AnnualStorage['Transport_Other_Petrol', 'Services_Transport_Other_Petrol']=0
# V_AnnualStorage['Transport_Other_NG', 'Services_Transport_Other_NG']=0
# V_AnnualStorage['Transport_Other_Other', 'Services_Transport_Other_Other']=0


## ++++++++++++++++++
## Write new U, V, and Y Sankey matrices for year 2050 based on user's influence on 8760 electricity profile and heating choices
## ++++++++++++++++++
output_list <- list("U_NoStorage_2050_CurrentRegion"=U_NoStorage,
                    "V_NoStorage_2050_CurrentRegion"=V_NoStorage,
                    "U_AnnualStorage_2050_CurrentRegion"=U_AnnualStorage,
                    "V_AnnualStorage_2050_CurrentRegion"=V_AnnualStorage)
return(output_list)

} ## end of function