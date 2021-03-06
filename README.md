# Energy Futures Dashboard (EFD)

![Image of UT Energy Institute Logo](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/7941cb97ccaf1fb48a02933b714a1b71381cd66c/images/RGB_formal_Energy_Institute.png)

This GitHub repository contains the source code for the Energy Futures Dashboard (EFD), developed by researchers at The University of Texas Energy Institute. The EFD is a web-based tool with a user friendly interface that can be located [here](http://energyfuturesdashboard.energy.utexas.edu/).

## Contents

* [Introduction](#Introduction)
* [Workflow](#Workflow)
* [Downloading the code](#Download)
* [Initial Setup](#Setup)
* [Operating the EFD](#Operating)
* [Documentation](#Documentation)
* [Google Sheets](#GoogleSheets)
* [Contact](#Contact)

<a name="Introduction"></a>
# Introduction
The EFD is a product of the [Energy Infrastructure of the Futures Study](https://energy.utexas.edu/policy/eiof). It is a [web based tool](http://energyfuturesdashboard.energy.utexas.edu/) for modeling the growth, utilization, and impacts of energy on the U.S. economy. This tool is intended to provide access to a non-expert audience that has interest in environmental and economic energy-related tradeoffs but might lack the training, experience, or time to utilize more complex modeling tools.

The EFD divides the continental United States into 13 geographic regions (see Figure 1). The regional definitions enable us to investigate broad geographical differences in energy infrastructure capacities, supply costs, electricity demand and climate profiles, and number of customers. The EFD takes in, as inputs, a n integer from 1 - 13 identifying a region,  a 2050 power sector generation mix of up to 10 different fuel and technology types (coal, natural gas, nuclear, petroleum, biomass, geothermal, wind, PV, CSP, and hydro), the 2050 percentage of electrified light duty vehicles, and the 2050 percentage of household heating from either natural gas, electricity, or other technologies.

![Image of EIoF regions](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/8f2b11b6118319aec0d16d7081773b9dcb66086a/images/eiof_regions.png)

Figure 1. Regional definitions used in the Energy Futures Dashboard. Northwest (NW), California (CA), Mountain North (MN), Southwest (SW), Central (CE), Texas (TX), Midwest (MW), Arkansas Louisiana (AL) Mid-Atlantic (MA), Southeast (SE), Florida (FL), New York (NY), New England (NE)


Documentation for the EFD model and its assumptions can be located [here](https://energy.utexas.edu/energy-futures-dashboard-documentation).
The file running_EFD_source_code.pptx, located in the repository, contains a step by step walkthrough for setting up and operating the EFD.

<a name="Workflow"></a>
# Workflow

![Image of workflow diagram](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/9a9da91fe97eb504fcac38e3701dc84f328d37fb/images/flow_diagram.png)

Figure 3. Flow diagram showing the workflow from start to finish of the EFD source code

<a name="Setup"></a>
# Initial Setup (_configure.R_)

Before running the EFD for the first time, you need to run the file _config.R_ in RStudio. This executes a setup process that is needed in order to operate the EFD. This process does 4 things:
  1. Install all necessary R packages needed to operate the EFD
  2. Upload the excel cashflow models for each region to a user's google drive account and convert them to google sheets
  3. Authorize R to read and write to google sheets on a user's google drive account
  4. Save a _config.csv_ file with any configuration settings that may be needed in the model.

<a name="Operating"></a>
# Operating the EFD (Option 1: batch runs via _run_simulations.R_)

The EFD runs simulations in batches defined in simulations_{batch name}.csv. The batch name should be unique and is specified when executing simulations. This file is organized by one simulation per row where each row contains all the necessary user inputs to operate the EFD (see Figure 2). The open source code for the EFD is driven by the file _run_simulations.R_. This code reads in the specified simulations_{batch name}.csv and executes the simulations consecutively, row by row. The following lines are an example of how to execute a batch of simulations: 
    
    ## On a Mac
    cd path/to/UT_EIoF_webtool
    rscript run_simulations.R -b test

    ## On Windows PC using the Windows command prompt
    "path\to\UT_EIoF_webtool\Rscript" run_simulations.R -b test


In this example, test is the batch name which is indicated by the -b flag. Both must be included for the simulations to execute. Once a simulation has completed, the output data will be saved as an Rdata file in /master_r_scripts/simulations/batch name. 

![Image of example simulations file](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/8f2b11b6118319aec0d16d7081773b9dcb66086a/images/simulation_test.png)

Figure 2. Every column is an input and every row represents a unique simulation

# Operating the EFD (Option 2: single simulation vai _master_EIoF_FOR_CODE_TESTING.R_)

The code master_EIoF_FOR_CODE_TESTING.R (a script code) is the same code as master_EIoF.R (a function) that calls all of the necessary subfunctions, but it runs as a script and not as a function. Thus, you can enter your EFD inputs at the beginning of this code in the lines as follows:

region_id = 9  # 1:NW-Northwest, 2:CA-California, 3: MN-Mountain North, 4:SW-Southwest, 5:CE-Central; 6:TX-Texas; 7:MW-Midwest; 8:AL-Arkansas and Louisiana; 9:MA-Mid Atlantic; 10:SE-Southeast; 11:FL-Florida; 12:NY-New York; 13:NE-New England
coal_percent = 10
PV_percent = 30
CSP_percent = 0
wind_percent = 30
biomass_percent = 0
hydro_percent = 0
petroleum_percent = 0
nuclear_percent = 10
geothermal_percent = 0
ng_percent = 20
ldv_e = 20  # percent of light duty vehicles that are electric
r_sh_e = 20  # percent of residential space heating this is electric (heat pumps)
r_sh_ng = 58  # percent of residential space heating that is natural gas furnaces

<a name="Documentation"></a>
# Documentation
The full documentation for the EFD and its assumptions can be located on the [UT Energy Institute website](https://energy.utexas.edu/energy-futures-dashboard-documentation). 


<a name="GoogleSheets"></a>
# Google Sheets

The R code of the EFD interfaces with a set of Google Sheets. There are two worksheets for each region, for a total of 26 worksheets. Each Google Sheet file has a series of worksheets that house all cost assumptions, historical data on power plant generation and capacity, and calculate costs using a cash flow approach from 2020 to 2050 based on the user inputs.  The EFD reads all output from the ???Aggregation??? worksheet for display to the user on the website.  Each region has 2 versions, one for ???no curtailment (with storage)??? and one for ???full curtailment (no storage)???.  There is no difference in these two versions, and they are created only to be able to run them in parallel rather than in series

To allow each user exclusive access to their own set of Google Sheets, these exist locally in the repo as excel files. They are easily uploaded and coverted to Google Sheets using the _config.R_ file. 

<a name="Contact"></a>
# Contact

For questions regarding the EFD, its documentation, or its source code, please reach out to Dr. Carey King at The University of Texas Energy Institute

Carey King
careyking@mail.utexas.edu
