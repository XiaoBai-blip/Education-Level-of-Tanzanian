#tinytex::install_tinytex()
#install.packages("pdftools")
#install.packages("janitor")
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("scales")
#install.packages("pointblank")
library(janitor)
library(pdftools)
library(purrr)
library(tidyverse)
library(stringi)
all_content <- pdf_text("male.pdf")
just_page_i <- stri_split_lines(all_content[[38]])[[1]]
just_page_i <- just_page_i[just_page_i != ""] # delete empty line

just_page_i_no_header <- just_page_i[9:52]
just_page_i_no_header

demography_data <- tibble(all = just_page_i_no_header)
demography_data
write_csv(demography_data, "outputs/data/raw_data.csv" )
