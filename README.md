This is code related to the paper, "Differences in Time Usage as a Competing Hypothesis for Observed Group Differences in Accuracy with an Application to Observed Gender Differences in PISA Data", paper [here](https://onlinelibrary.wiley.com/doi/10.1111/jedm.12419). The code in this repository can be used to recreate the simulation and empirical results from the paper. Data for empirical results is available on the PISA website. 

### Installing PISAhelper package
Please install the PISAhelper package to assist with the analysis -- the package can be used to simulate a dataset, calculate observed group score differences, and estimate group score differences due to capacity:

```
## Install necessary packages
list.of.packages=c('kdensity', 'MASS', 'splines', 'dplyr', 'fixest', 'devtools')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Install PISAhelper from github
library(devtools)
devtools::install_github("rkap786/PISAhelper", ref="main")

```

### Relevant code

1. Folder "Sim" contains the code for generating data for simulation, for data analysis, as well as recreating plots from the paper (Figures 3 & 4)
2. Folder "Analysis" contains the code for generating the empirical results. 
3. Folder "Plots" contains the code to plot results from the empirical analysis (Figures 5-8)
4. Folder "Data processing" has code for converting PISA 2018 data from OECD website to a format that works with the code above
