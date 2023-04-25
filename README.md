# Frey, Tilstra, and Verhagen (2023): Inequalities in Healthcare use during the COVID-19 pandemic

All results presented here are based on own calculations based on non-public registry data from Centraal Bureau voor de Statistiek (CBS)CBS, accessed through the Remote Access environment.

### Data

All data files are generated using the scripts in the `/src/` folder, ran on the RA (Remote Access) environment of the CBS. The scripts assume the following data structure in the RA environment:

    ├── health_access
    │   └── data
    │       └── raw
    │           ├── health_activities
    │           │   ├── MSZZorgactiviteitenVEKT2016_trim.rds
    │           │   ├── MSZZorgactiviteitenVEKT2017_trim.rds
    │           │   ├── MSZZorgactiviteitenVEKT2018_trim.rds
    │           │   ├── MSZZorgactiviteitenVEKT2019_trim.rds
    │           │   └── MSZZorgactiviteitenVEKT2020_trim.rds
    │           ├── health_procedures
    │           │   ├── health_codes_2016.rds
    │           │   ├── health_codes_2017.rds
    │           │   ├── health_codes_2018.rds
    │           │   ├── health_codes_2019.rds
    │           │   └── health_codes_2020.rds
    │           ├── demographics
    │           │   ├── 2016
    │           │   │   └── rin_demog_2016.rds
    │           │   ├── 2017
    │           │   │   └── rin_demog_2017.rds
    │           │   ├── 2018
    │           │   │   └── rin_demog_2018.rds
    │           │   ├── 2019
    │           │   │   └── rin_demog_2019.rds
    │           │   └── 2020
    │           │       └── rin_demog_2020.rds
    │           ├── Urgentielijst medisch-specialistische zorg - 29 mei 2020.xlsx
    │           ├── ReflijstZorgactiviteiten.csv
    │           ├── LBZBASIS2020TABV1.rds
    │           └── 20200319; versie 4-1F patientengroepen NZa.xlsx
    └── data
        ├── numeric_income
        │  ├── 2017
        │   ├── 2018
        │   ├── 2019
        │   └── 2020 
        └── 05r_output.R

### Run

To generate the plots, run `plots.R`. To generate the tables, run `desc.R`. In the files, you need to specify: 1. Data files 2. Treatment year 3. Dependent variable 4. Variable Groups 5. Whether or not to exclude COVID

### Project structure

    ├── LICENSE
    ├── README.md
    ├── data
    ├── desc.R
    ├── figs
    │   └── main
    │       ├── activities
    │       │   ├── 2019
    │       │   │   ├── by_type
    │       │   │   └── by_urgency
    │       │   └── 2020
    │       │       ├── by_type
    │       │       └── by_urgency
    │       └── individuals
    │           ├── 2019
    │           │   ├── by_type
    │           │   └── by_urgency
    │           └── 2020
    │               ├── by_type
    │               └── by_urgency
    ├── old
    │   ├── 04r_main.R
    │   └── 05r_output.R
    ├── plot_fn.R
    ├── plots.R
    └── tables
        └── main
            ├── activities
            │   ├── 2019
            │   │   ├── by_type
            │   │   └── by_urgency
            │   └── 2020
            │       ├── by_type
            │       └── by_urgency
            └── individuals
                ├── 2019
                │   ├── by_type
                │   └── by_urgency
                └── 2020
                    ├── by_type
                    └── by_urgency
