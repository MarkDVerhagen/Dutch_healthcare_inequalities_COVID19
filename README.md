# Frey, Tilstra, and Verhagen (2023): Inequalities in Healthcare use during the COVID-19 pandemic

All results presented here are based on own calculations based on non-public registry data from Centraal Bureau voor de Statistiek (CBS)CBS, accessed through the Remote Access environment.

### Introduction

This repository contains all code underlying the results in 'Inequalities in Healthcare Use during the COVID-19 Pandemic' by Frey, Tiltra, and Verhagen (2023). The paper can be found [here](https://www.medrxiv.org/content/10.1101/2023.04.26.23289095v1).

>The COVID-19 pandemic has led to severe reductions in non-COVID related healthcare use, but little is known whether this burden is shared equally across the population. This study investigates whether the reduction in administered care disproportionately affected certain sociodemographic strata, in particular marginalised groups. Using detailed medical claims data from the Dutch universal health care system and rich registry data that cover all residents in The Netherlands, we predict expected healthcare use based on pre-pandemic trends (2017– Feb 2020) and compare these expectations with observed healthcare use in 2020. Our findings reveal a substantial 10% decline in the number of weekly treated patients in 2020 relative to prior years. Furthermore, declines in healthcare use are unequally distributed and are more pronounced for individuals below the poverty line, females, the elderly, and foreign-born individuals, with cumulative relative risk ratios ranging from 1.09 to 1.22 higher than individuals above the poverty line, males, young, and native-born. These inequalities stem predominantly from declines in middle and low urgency procedures, and indicate that the pandemic has not only had an unequal toll in terms of the direct health burden of the pandemic, but has also had a differential impact on the use of non-COVID healthcare.

### Repo structure

Preparatory analyses has been done within the Remote Access (RA) environment of Statistics Netherlands (CBS). Data access is conditional on eligibility constrains from CBS and approval from [VEKTIS](https://www.vektis.nl/) for the medical claims data. The folder `src_ra` reflects all scripts run within the RA environment and the folder `src` reflects all scripts run after data has been exported from the RA environment. See below for a flow structure:

![Code and data flow](./www/repo_flow.png)

### Remote Access Environment

All intermittent data files are generated using the scripts in the `./src_ra/` folder, ran on the RA environment of the CBS. The scripts assume the following data structure within the RA environment:

    └── health_access
        └── data
            └── raw
                ├── health_activities
                │   ├── MSZZorgactiviteitenVEKT2016_trim.rds
                │   ├── MSZZorgactiviteitenVEKT2017_trim.rds
                │   ├── MSZZorgactiviteitenVEKT2018_trim.rds
                │   ├── MSZZorgactiviteitenVEKT2019_trim.rds
                │   ├── MSZZorgactiviteitenVEKT2020_trim.rds
                │   └── MSZZorgactiviteitenVEKT2021_trim.rds
                ├── health_procedures
                │   ├── health_codes_2016.rds
                │   ├── health_codes_2017.rds
                │   ├── health_codes_2018.rds
                │   ├── health_codes_2019.rds
                │   ├── health_codes_2020.rds
                │   └── health_codes_2021.rds
                ├── demographics
                │   ├── 2016
                │   │   └── rin_demog_2016.rds
                │   ├── 2017
                │   │   └── rin_demog_2017.rds
                │   ├── 2018
                │   │   └── rin_demog_2018.rds
                │   ├── 2019
                │   │   └── rin_demog_2019.rds
                │   ├── 2020
                │   │   └── rin_demog_2020.rds
                │   └── 2021
                │       └── rin_demog_2021.rds
                ├── Urgentielijst medisch-specialistische zorg - 29 mei 2020.xlsx
                ├── ReflijstZorgactiviteiten.csv
                ├── LBZBASIS2020TABV1.rds
                ├── LBZBASIS2021TABV1.rds
                └── 20200319; versie 4-1F patientengroepen NZa.xlsx


### Run

To generate the plots, run `main.R`. To generate the tables, run `desc.R`. In the files, you need to specify: 1. Data files 2. Treatment year 3. Dependent variable 4. Variable Groups 5. Whether or not to exclude COVID

### Project structure

    ├── LICENSE
    ├── README.md
    ├── data
    │   └── timeseries
    ├── desc.R
    ├── figs
    ├── plot_functions.R
    ├── main.R
    └── tables