
library(janitor)
library(pdftools)
library(purrr)
library(tidyverse)
library(stringi)
setwd("/Volumes/GoogleDrive-115907163826663808515/我的云端硬盘/ray+/课/304/2022 W/Paper 4")
#install.packages("pdftools")
library("pdftools")
all_content <- pdf_text("VOLUME III KPHC 2019.pdf")
just_page_i <- stri_split_lines(all_content[[30]])[[1]]  # see pg 30
just_page_i
just_page_i <- just_page_i[just_page_i != ""] # delete empty line
just_page_i
just_page_i[3] |> str_squish()
# Grab the name of the location
area <- just_page_i[3] |> str_squish()
area
area <- str_to_title(area)
area
# Grab the type of table
type_of_table <- just_page_i[2] |> str_squish()
type_of_table

# Get rid of the top matter
# Manually for now, but could create some rules if needed
just_page_i_no_header <- just_page_i[5:length(just_page_i)]
just_page_i_no_header
# Get rid of the bottom matter
# Manually for now, but could create some rules if needed
just_page_i_no_header_no_footer <- just_page_i_no_header[1:62] 
just_page_i_no_header_no_footer

# Convert into a tibble
demography_data <- tibble(all = just_page_i_no_header_no_footer)
demography_data

# Split columns
demography_data |>
  mutate(all = str_squish(all)) # Any space more than two spaces is reduced
  
demography_data <-
  demography_data |>
  mutate(all = str_squish(all)) |> # Any space more than two spaces is reduced
  mutate(all = str_replace(all, "10 -14", "10-14")) |> # One specific issue
  mutate(all = str_replace(all, "Not Stated", "NotStated")) |> # And another
  separate(col = all,
           into = c("age", "male", "female", "total", "age_2", "male_2", "female_2", "total_2"),
           sep = " ", # Works fine because the tables are nicely laid out
           remove = TRUE,
           fill = "right",
           extra = "drop"
  )

demography_data

# They are side by side at the moment, need to append to bottom
demography_data_long <-
  rbind(demography_data |> select(age, male, female, total),
        demography_data |>
          select(age_2, male_2, female_2, total_2) |>
          rename(age = age_2, male = male_2, female = female_2, total = total_2)
  )
demography_data_long

# There is one row of NAs, so remove it
demography_data_long <- 
  demography_data_long |> 
  remove_empty(which = c("rows"))

# Add the area and the page
demography_data_long$area <- area
demography_data_long$table <- type_of_table
demography_data_long$page <- i
demography_data_long

########### 
get_data <- function(i){
  # i = 467
  # Just look at the page of interest
  # Based on Bob Rudis: https://stackoverflow.com/a/47793617
  just_page_i <- stri_split_lines(all_content[[i]])[[1]] 
  
  just_page_i <- just_page_i[just_page_i != ""]
  
  # Grab the name of the location
  area <- just_page_i[3] |> str_squish()
  area <- str_to_title(area)
  
  # Grab the type of table
  type_of_table <- just_page_i[2] |> str_squish()
  
  # Get rid of the top matter
  # Manually for now, but could create some rules if needed
  just_page_i_no_header <- just_page_i[5:length(just_page_i)] 
  
  # Get rid of the bottom matter
  # Manually for now, but could create some rules if needed
  just_page_i_no_header_no_footer <- just_page_i_no_header[1:62] 
  
  # Convert into a tibble
  demography_data <- tibble(all = just_page_i_no_header_no_footer)
  
  # Split columns
  demography_data <-
    demography_data |>
    mutate(all = str_squish(all)) |> # Any space more than two spaces is reduced
    mutate(all = str_replace(all, "10 -14", "10-14")) |> # One specific issue
    mutate(all = str_replace(all, "Not Stated", "NotStated")) |> # And another
    separate(col = all,
             into = c("age", "male", "female", "total", "age_2", "male_2", "female_2", "total_2"),
             sep = " ", # Works fine because the tables are nicely laid out
             remove = TRUE,
             fill = "right",
             extra = "drop"
    )
  
  # They are side by side at the moment, need to append to bottom
  demography_data_long <-
    rbind(demography_data |> select(age, male, female, total),
          demography_data |>
            select(age_2, male_2, female_2, total_2) |>
            rename(age = age_2, male = male_2, female = female_2, total = total_2)
    )
  
  # There is one row of NAs, so remove it
  demography_data_long <- 
    demography_data_long |> 
    remove_empty(which = c("rows"))
  
  # Add the area and the page
  demography_data_long$area <- area
  demography_data_long$table <- type_of_table
  demography_data_long$page <- i
  
  rm(just_page_i,
     i,
     area,
     type_of_table,
     just_page_i_no_header,
     just_page_i_no_header_no_footer,
     demography_data)
  
  return(demography_data_long)
}

# Run through each relevant page and get the data
pages <- c(30:513)
all_tables <- map_dfr(pages, get_data)
all_tables


############ Clean the data
# Need to convert male, female, and total to integers
# First find the characters that should not be in there
all_tables |> 
  select(male, female, total) |>
  mutate_all(~str_remove_all(., "[:digit:]")) |> 
  mutate_all(~str_remove_all(., ",")) |>
  mutate_all(~str_remove_all(., "_")) |>
  mutate_all(~str_remove_all(., "-")) |> 
  distinct()

# We clearly need to remove ",", "_", and "-". 
# This also highlights a few issues on p. 185 that need to be manually adjusted
# https://twitter.com/RohanAlexander/status/1244337583016022018
all_tables$male[all_tables$male == "23-Jun"] <- 4923
all_tables$male[all_tables$male == "15-Aug"] <- 4611

all_tables <-
  all_tables |>
  mutate_at(vars(male, female, total), ~str_remove_all(., ",")) |>
  mutate_at(vars(male, female, total), ~str_replace(., "_", "0")) |>
  mutate_at(vars(male, female, total), ~str_replace(., "-", "0")) |>
  mutate_at(vars(male, female, total), ~as.integer(.))
all_tables

# Fix some area names
all_tables$area[all_tables$area == "Taita/ Taveta"] <- "Taita/Taveta"
all_tables$area[all_tables$area == "Elgeyo/ Marakwet"] <- "Elgeyo/Marakwet"
all_tables$area[all_tables$area == "Nairobi City"] <- "Nairobi"

all_tables$table |> 
  table()







###########
all_tables <- 
  all_tables |> 
  mutate(area_type = if_else(area %in% list_counties$area, "county", "sub-county"))

all_tables <- 
  all_tables |> 
  mutate(area_type = case_when(
    area == "Samburu" & page == 42 ~ "sub-county",
    area == "Tana River" & page == 56 ~ "sub-county",
    area == "Garissa" & page == 69 ~ "sub-county",
    area == "Isiolo" & page == 100 ~ "sub-county",
    area == "Machakos" & page == 154 ~ "sub-county",
    area == "Makueni" & page == 164 ~ "sub-county",
    area == "Kiambu" & page == 213 ~ "sub-county",
    area == "West Pokot" & page == 233 ~ "sub-county",
    area == "Vihiga" & page == 333 ~ "sub-county",
    area == "Busia" & page == 353 ~ "sub-county",
    area == "Siaya" & page == 360 ~ "sub-county",
    area == "Homa Bay" & page == 375 ~ "sub-county",
    TRUE ~ area_type
  )
  )

rm(list_counties)

all_tables
#> # A tibble: 59,532 × 8
#>    age     male female   total area    table  page area_type
#>    <chr>  <int>  <int>   <int> <chr>   <chr> <int> <chr>    
#>  1 Total 610257 598046 1208303 Mombasa Tabl…    30 county   
#>  2 0      15111  15009   30120 Mombasa Tabl…    30 county   
#>  3 1      15805  15308   31113 Mombasa Tabl…    30 county   
#>  4 2      15088  14837   29925 Mombasa Tabl…    30 county   
#>  5 3      14660  14031   28691 Mombasa Tabl…    30 county   
#>  6 4      14061  13993   28054 Mombasa Tabl…    30 county   
#>  7 0-4    74725  73178  147903 Mombasa Tabl…    30 county   
#>  8 5      13851  14023   27874 Mombasa Tabl…    30 county   
#>  9 6      12889  13216   26105 Mombasa Tabl…    30 county   
#> 10 7      13268  13203   26471 Mombasa Tabl…    30 county   
#> # … with 59,522 more rows

table(all_tables$age) |> head()
#> 
#>     0   0-4     1    10 10-14 10-19 
#>   484   484   484   484   482     1
unique(all_tables$age) |> head()
#> [1] "Total" "0"     "1"     "2"     "3"     "4"

# Looks like there should be 484, so need to follow up on some:
all_tables$age[all_tables$age == "NotStated"] <- "Not Stated"
all_tables$age[all_tables$age == "43594"] <- "5-9"
all_tables$age[all_tables$age == "43752"] <- "10-14"
all_tables$age[all_tables$age == "9-14"] <- "5-9"
all_tables$age[all_tables$age == "10-19"] <- "10-14"


all_tables$age_type <-
  if_else(str_detect(all_tables$age, c("-")), "age-group", "single-year")
all_tables$age_type <-
  if_else(str_detect(all_tables$age, c("Total")),
          "age-group",
          all_tables$age_type)

all_tables$age <- as_factor(all_tables$age)
# Table 2.3
table_2_3 <- all_tables |> 
  filter(table == "Table 2.3: Distribution of Population by Age, Sex*, County and Sub- County")
table_2_4a <- all_tables |> 
  filter(table == "Table 2.4a: Distribution of Rural Population by Age, Sex* and County")
table_2_4b <- all_tables |> 
  filter(table == "Table 2.4b: Distribution of Urban Population by Age, Sex* and County")
both_2_4s <-
  full_join(
    table_2_4a,
    table_2_4b,
    by = c("age", "area", "area_type"),
    suffix = c("_rural", "_urban")
  )

all <-
  full_join(
    table_2_3,
    both_2_4s,
    by = c("age", "area", "area_type"),
    suffix = c("_all", "_")
  )

all <-
  all |>
  mutate(
    page = glue::glue(
      'Total from p. {page}, rural from p. {page_rural}, urban from p. {page_urban}'
    )
  ) |>
  select(
    -page,
    -page_rural,
    -page_urban,-table,
    -table_rural,
    -table_urban,-age_type_rural,
    -age_type_urban
  )


#############################

all2<- all |>
  rename(male_total = male,
         female_total = female,
         total_total = total) |>
  pivot_longer(
    cols = c(
      male_total,
      female_total,
      total_total,
      male_rural,
      female_rural,
      total_rural,
      male_urban,
      female_urban,
      total_urban
    ),
    names_to = "type",
    values_to = "number"
  ) 
all|>
  rename(male_total = male,
         female_total = female,
         total_total = total)
all2


all <-
  all |>
  rename(male_total = male,
         female_total = female,
         total_total = total) |>
  pivot_longer(
    cols = c(
      male_total,
      female_total,
      total_total,
      male_rural,
      female_rural,
      total_rural,
      male_urban,
      female_urban,
      total_urban
    ),
    names_to = "type",
    values_to = "number"
  ) |>
  separate(
    col = type,
    into = c("gender", "part_of_area"),
    sep = "_"
  ) |>
  select(area, area_type, part_of_area, age, age_type, gender, number)


