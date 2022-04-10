library(knitr)
library(janitor)
library(lubridate)
library(tidyverse)
library(tidyr)

set.seed(234)

simulated_Tanzania_data <- 
  tibble(
    By_residence = 
      c(
        rep('Mainland', 20),
        rep('Total_urban', 20),
        rep('Dares_Salaam_city', 20),
        rep('Other_urban', 20),
        rep('Total_rural', 20),
        rep('Zanzibar', 20),
        rep('Pemba', 20),
        rep('Unguja', 20)
      ),
    year = 
      rep(c(1990:2009), 8),
    
    percent_male_no_education = 
      runif(n = 160,
            min = 10.2, 
            max = 50.04),
    percent_male_primary_education = 
      runif(n = 160,
            min = 13.2, 
            max = 50.3)
    
  )