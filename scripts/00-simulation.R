library(knitr)
library(janitor)
library(lubridate)
library(tidyverse)
library(tidyr)



df1_new<-as.data.frame(t(data_region))
df1_new


data_region%>%
  summarise(mean_no_edu = mean(no_education), mean_higher_edu = mean(completed_primary))
#test_stat = 31.79-27.335 = 4.455

mean_data <- data.frame (characteristic  = c("no_education", "completed_prim"),
                         means = c("31.79", "27.335")
)%>%mutate(means = as.numeric(means))

test_stat = as.numeric(mean_data%>%summarise(test_stat = diff(means)))
test_stat


repetitions = 1000
simulated_stats = rep(NA, repetitions)
for (i in 1:repetitions)
{
  dat =mean_data%>% mutate(characteristic=sample(characteristic))
  y = dat %>%
    summarise(sim_test_stat = diff(means))
  x[i]=as.numeric(y)
}
sim = data.frame(mean_diff=x)

sim%>%
  ggplot(aes(x=mean_diff))+geom_histogram()




t <- (6.82381	-13.4)/(5.586124/sqrt(43))
pvalue <- 1-pt(t,1000-1)
pvalue


















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
    
    no_education = 
      runif(n = 160,
            min = 10.2, 
            max = 50.04),
    primary_incompleted = 
      runif(n = 160,
            min = 13.2, 
            max = 50.3),
    
    primary_completed = 
      runif(n = 160,
            min = 10.2, 
            max = 50.3)
  )