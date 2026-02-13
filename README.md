# hhlink

`hhlink` contains the code and data for the **household-and-Hausdorff-distance-based record linkage method**, a two-step framework for matching households and individuals across databases:

1. **Household matching:** Uses the Hausdorff distance as input to a supervised method to identify matching households.  
2. **Individual matching:** Uses the matched households as input to supervised learning and linear programming optimization to identify one-to-one matches between individuals.

---

## Repository Structure

```text
hhlink/
├─ main/                       
│  ├─ Basic Variables.R         # Functions to compute basic database statistics used throughout the code (e.g., ranges of variables)
│  ├─ LinearProgrammingIndividuals.R  # Functions to run linear programming optimization for individual matches
│  ├─ Pairs1.R                  # Code to generate all possible pairs of individuals in the first database
│  ├─ Pairs2.R                  # Code to generate all possible pairs of individuals in the second database
│  └─ Code.R                    # Main script to fit the model and save all results
├─ main/                        
│  ├─ Matches Ind.RDS           # True individual matches
│  ├─ Matches.RDS               # True household matches
│  ├─ distance_matrix_features.RDS  # List of distance matrices for each feature (rows: individuals in file A; columns: individuals in file B)
│  ├─ data_mod_individuals_Italy.RDS  # Data for fitting the individual model (matched households only)
│  └─ Data Italy - Final.RDS    # Italian Survey of Household Income and Wealth 2014 & 2016
└─ 2016-2020/                   
   ├─ Matches Ind.RDS
   ├─ Matches.RDS
   ├─ distance_matrix_features.RDS
   ├─ data_mod_individuals_Italy.RDS
   └─ Data Italy - Final.RDS
```
---

# Scripts Overview
* Basic Variables.R – Computes database summary information used throughout the workflow, such as variable ranges.
* LinearProgrammingIndividuals.R – Performs linear programming optimisation to estimate one-to-one matches between individuals.
* Pairs1.R & Pairs2.R – Generate all possible individual pairs in the two databases for the matching process.
* Code.R – Main script: fits the model and saves results to the repository.

# Data Overview

* Matches Ind.RDS – True matching information at the individual level.
* Matches.RDS – True matching information at the household level.
* distance_matrix_features.RDS – Each list element contains a distance matrix for one feature; rows correspond to individuals in file A, columns to individuals in file B. Distance definitions are described in the corresponding paper.
* data_mod_individuals_Italy.RDS – Data used to fit the individual matching model (households already matched).
* Data Italy - Final.RDS – Original survey data from the Italian Survey of Household Income and Wealth.

The 2016-2020 folder mirrors the structure of the main folder, but all files correspond to the 2016–2020 survey waves.

**Important note:** Some data files are very large. If you encounter any issues accessing them, please contact me at thaispacheco1207@gmail.com
.
