### Setup ############################################################

# This file sets up directories and loads packages

##########################################################################################

### 0. Setup

# clear workspace
rm(list = ls())

# set directories
setwd("/Users/jack/git_repos/covid_UK/Code") # main directory
rawdatdir <- c("/Users/jack/Dropbox/Documents/Projects/covid_UK/Raw data") # directory containing raw data
datdir <- c("/Users/jack/Dropbox/Documents/Projects/covid_UK/Data") # directory containing other data

# Load packages
library(tidyverse) # all tidyverse packages (dplyr,tidyr,readr,ggplot2)
#library(readxl) # for importing excel
library(lubridate) # for dealing with dates
#library(rlist) # convenient list manipulations
#library(stringdist) # string distance metric