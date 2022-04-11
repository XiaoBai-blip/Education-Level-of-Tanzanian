# Cleaning process
demography_data <- read.csv("raw_data.csv")
demography_data <-
  demography_data |>
  mutate(all = str_replace(all, ",", "."))|>
  mutate(all = str_squish(all)) |> # Any space more than two spaces is reduced
  mutate(all = str_replace(all, "Total urban", "Total_urban")) |> # One specific issue
  mutate(all = str_replace(all, "Dares Salaam city", "Dares_Salaam_city")) |> # And another
  mutate(all = str_replace(all, "Other urban", "Other_urban")) |> 
  mutate(all = str_replace(all, "D ares Salaam", "Dar_es_Salaam")) |>
  mutate(all = str_replace(all, "I 1.7", "11.7")) |>
  mutate(all = str_replace(all, "Total rural", "Total_rural")) |> # One specific issue
  separate(col = all,
           into = c("Background Characteristic", "No education", "Primary incomplete", "Completed primary", "Some secondary and higher", "Don't know", "Total", "Median year of schooling", "Total number"),
           sep = " ", # Works fine because the tables are nicely laid out
           remove = TRUE,
           fill = "right",
           extra = "drop"
  )

demography_data
# save as the csv document
write_csv(demography_data, "outputs/data/cleaned_data.csv")


# Use pointblank to put together tests
demography_data %>%
  mutate_at(vars("No education", "Primary incomplete", "Completed primary", "Some secondary and higher", "Don't know", "Total", "Median year of schooling", "Total number"), ~as.integer(.))
library(pointblank)

agent <-
  create_agent(tbl = demography_data) |>
  col_is_character(columns = vars("Background Characteristic")) |>
  col_is_character(columns = vars("No education", "Primary incomplete", "Completed primary", "Some secondary and higher", "Don't know", "Total", "Median year of schooling", "Total number")) |>
  interrogate()
agent  