# Load all packages that may need in this simulation
library(knitr)
library(janitor)
library(lubridate)
library(tidyverse)
library(tidyr)
# To simulate our data, we need to create a dataset with nine columns: 
# ‘by residence’, ‘year’, 'no_education', 'primary_incomplete', and etc. 
# that are listed below. In the case of 'by_residence', reasonable values could 
# be 'Mainland', "total_urban' and six more.

set.seed(234)# We randomly set values for set seed

simulated_Tanzania_data <- 
  tibble(
    By_residence = 
      c(                 # Randomly choose one of eight options, we repeat the name of each residence 20 times with rep()
        rep('Mainland', 20),
        rep('Total_urban', 20),
        rep('Dares_Salaam_city', 20),
        rep('Other_urban', 20),
        rep('Total_rural', 20),
        rep('Zanzibar', 20),
        rep('Pemba', 20),
        rep('Unguja', 20)
      ),
    year =              # Randomly select years from 1990 to 2009 
      rep(c(1990:2009), 8),
    
    no_education = 
      runif(n = 160,    # we draw from the uniform distribution with runif() to simulate an estimated value with size 160
            min = 10.2, # set the minimum and maximum values, and 
            max = 50.04), # these values are from observing our raw data. The purpose is to let R generate similar values as raw dataset 
    primary_incompleted = 
      runif(n = 160,    # repeat the similar process as above but this time generate another variable.
            min = 13.2, 
            max = 50.3),
    
    primary_completed = # repeat the same process
      runif(n = 160,
            min = 10.2, 
            max = 50.3),
    secondery_or_higher = # repeat the same process
      runif(n = 160,
            min = 11.2, 
            max = 50.3),
    
    dont_know =           # repeat the same process
      runif(n = 160,
            min = 0, 
            max = 3),
    median_year_of_schooling =  # repeat the same process
      runif(n = 160,
            min = 0, 
            max = 8),
    total_number =      # repeat the same process until all nine variables are generated. Then we can use this simulated data
      runif(n = 160,
            min = 900, 
            max = 1200)
  )