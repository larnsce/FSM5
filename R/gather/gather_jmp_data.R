# header ------------------------------------------------------------------

# R script to download and tidy data from JMP WASH website www.washdata.org
# Created by Lars Schoebitz
# MIT Licence
# 16.01.2018

# comments ----------------------------------------------------------------

## this script downloads the world file from the JMP database
## this script manipulates the data to produce a final tidy dataset for further analysis

# libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)

# load data ---------------------------------------------------------------

## create temporary file with file extension xlsx
temp_file <- tempfile(fileext = ".xlsx")

## download world data file to temporary file
download.file("https://washdata.org/data/country/WLD/download", destfile = temp_file)

## read sheet 3 of downloade excel file into R
data <- openxlsx::read.xlsx(xlsxFile = temp_file, sheet = 3, startRow = 4, colNames = FALSE)

data2 <- as_tibble(data)

# manipulate data ---------------------------------------------------------

## tidy the dataset

jmp_data_tidy <- data2 %>% 
    select(1:4, 26:46) %>% 
    gather(key = variable, value = percent, X26:X46) %>%
    mutate(
        variable = case_when(
            variable == "X26" ~ "safely_managed_n",
            variable == "X27" ~ "disposed_in_situ_n",
            variable == "X28" ~ "emptied_and_treated_n",
            variable == "X29" ~ "wastewater_treated_n",
            variable == "X30" ~ "latrines_and_others_n",
            variable == "X31" ~ "septic_tanks_n",
            variable == "X32" ~ "sewer_connections_n",
            variable == "X33" ~ "safely_managed_r",
            variable == "X34" ~ "disposed_in_situ_r",
            variable == "X35" ~ "emptied_and_treated_r",
            variable == "X36" ~ "wastewater_treated_r",
            variable == "X37" ~ "latrines_and_others_r",
            variable == "X38" ~ "septic_tanks_r",
            variable == "X39" ~ "sewer_connections_r",
            variable == "X40" ~ "safely_managed_u",
            variable == "X41" ~ "disposed_in_situ_u",
            variable == "X42" ~ "emptied_and_treated_u",
            variable == "X43" ~ "wastewater_treated_u",
            variable == "X44" ~ "latrines_and_others_u",
            variable == "X45" ~ "septic_tanks_u",
            variable == "X46" ~ "sewer_connections_u"
        )
    ) %>% 
    mutate(
        residence = case_when(
            variable = str_detect(variable, "_n") == TRUE ~ "national",
            variable = str_detect(variable, "_r") == TRUE ~ "rural",
            variable = str_detect(variable, "_u") == TRUE ~ "urban"
        )
    ) %>%
    mutate(variable = str_replace(variable, pattern =  "_n", replacement = "")) %>% 
    mutate(variable = str_replace(variable, pattern =  "_r", replacement = "")) %>% 
    mutate(variable = str_replace(variable, pattern =  "_u", replacement = "")) %>% 
    rename(
        country = X1,
        iso3 = X2,
        year = X3,
        population = X4
    ) %>% 
    mutate(percent = "is.na<-"(percent, percent == "-")) %>%   ## https://stackoverflow.com/a/27909247/6816220
    mutate(percent = as.double(percent)) %>% 
    mutate(population = population * 1000) %>% 
    filter(!is.na(percent))


# write data

write_csv(jmp_data_tidy, "data/jmp_2017_database_tidy.csv")






