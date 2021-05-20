# Energy Futures Dashboard (EFD)

![Image of UT Energy Institute Logo](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/7941cb97ccaf1fb48a02933b714a1b71381cd66c/images/RGB_formal_Energy_Institute.png)

This GitHub repository contains the source code for the Energy Futures Dashboard, developed by researchers at The University of Texas Energy Institute. The EFD is a web-based tool with a user friendly interface that can be located [here](http://energyfuturesdashboard.energy.utexas.edu/).

## Contents

* [Introduction](#Introduction)
* [Workflow](#Workflow)
* [Initial Setup](#Initial Setup)
* [Operating the EFD](#Operating the EFD)
* [Google Sheets](#Google Sheets)
* [Contact](#Contact)


<a name="Introduction"></a>
# Introduction
The EFD is a product of the [Energy Infrastructure of the Futures Study](https://energy.utexas.edu/policy/eiof). It is a [web based tool](http://energyfuturesdashboard.energy.utexas.edu/) for modeling the growth, utilization, and impacts of energy on the U.S. economy. This tool is intended to provide access to a non-expert audience that has interest in environmental and economic energy-related tradeoffs but might lack the training, experience, or time to utilize more complex modeling tools.

![Image of EIoF regions](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/7941cb97ccaf1fb48a02933b714a1b71381cd66c/images/eiof_regions.png)

The EFD divides the continental United States into 13 geographic regions (see Figure 1). The regional definitions enable us to investigate broad geographical differences in energy infrastructure capacities, supply costs, electricity demand and climate profiles, and number of customers. The EFD takes in, as inputs, a n integer from 1 - 13 identifying a region,  a 2050 power sector generation mix of up to 10 different fuel and technology types (coal, natural gas, nuclear, petroleum, biomass, geothermal, wind, PV, CSP, and hydro), the 2050 percentage of electrified light duty vehicles, and the 2050 percentage of household heating from either natural gas, electricity, or other technologies.

Documentation for the EFD model and its assumptions can be located [here](https://energy.utexas.edu/energy-futures-dashboard-documentation).
The file running_EFD_source_code.pptx, located in the repository, contains a step by step walkthrough for setting up and operating the EFD.

<a name="Workflow"></a>
# Workflow

Describe the workflow here.

<a name="Initial Setup"></a>
# Initial Setup

Before running the EFD for the first time, you need to run the file _config.R_ This executes a setup process that is needed in order to operate the EFD. This process does 4 things:
  1. Install all necessary R packages needed to operate the EFD
  2. Upload the excel cashflow models for each region to a user's google drive account and convert them to google sheets
  3. Authorize R to read and write to google sheets on a user's google drive account
  4. Save a _config.csv_ file with any configuration settings that may be needed in the model.

<a name="Operting the EFD"></a>
# Operating the EFD

The EFD runs simulations in batches defined in simulations_{batch name}.csv. The batch name should be unique and is specified when executing simulations. This file is organized by one simulation per row where each row contains all the necessary user inputs to operate the EFD (see Figure 2). The open source code for the EFD is driven by the file _run_simulations.R_. This code reads in the specified simulations_{batch name}.csv and executes the simulations consecutively, row by row. The following lines are an example of how to execute a batch of simulations: 
    
    `cd path/to/UT_EIoF_webtool`
    `rscript run simulations.R -b test`
    
In this example, test is the batch name which is indicated by the -b flag. Both must be included for the simulations to execute. Once a simulation has completed, the output data will be saved as an Rdata file in /master_r_scripts/simulations/batch name. 

![Image of example simulations file](https://github.com/ut-energy-institute/UT_EIoF_webtool/blob/7941cb97ccaf1fb48a02933b714a1b71381cd66c/images/simulation_test.png)

<a name="Google Sheets"></a>
# Google Sheets

Describe the Google Sheets here

<a name="Contact"></a>
# Contact

Put contact info here
