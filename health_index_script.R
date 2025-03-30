# ETL Pipeline: Extracting Transforming & Loading

# Project Aim: To compare health index scores across different regions in England 
# during the mid-COVID era, identifying key areas for intervention and assessing 
# regional disparities in health outcomes.

# Get Health Index Score data from ONS
# Install and load necessary packages

install.packages(c("readxl", "dplyr", "tidyr", "writexl"))
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(tidyr)   # For handling messy data
library(writexl) # For saving cleaned data

# Load the dataset 
getwd() # confirm working directory
list.files() # confirm folder existence
list.files("Health Index Score England [ONS]")

healthindex <- read_excel("Health Index Score England [ONS]/healthindexscoresengland.xlsx")
head(healthindex) # verify data

# need 2020 and 2021 sheets to assess mid-covid era
excel_sheets("Health Index Score England [ONS]/healthindexscoresengland.xlsx") #needed data in 6 and 7

# assign df to 2021 and 2020 data
index_2021 <- read_excel("Health Index Score England [ONS]/healthindexscoresengland.xlsx", sheet = 6)
index_2020 <- read_excel("Health Index Score England [ONS]/healthindexscoresengland.xlsx", sheet = 7)

#confirm data
head(index_2021)
head(index_2020)

# From output, data is messy and starts from row 5. let's fix that :-)
index_2021 <- read_excel("Health Index Score England [ONS]/healthindexscoresengland.xlsx", sheet = 6, skip = 4)
index_2020 <- read_excel("Health Index Score England [ONS]/healthindexscoresengland.xlsx", sheet = 7, skip = 4)
head(index_2020)
head(index_2021)

# Looks good, but we need country-level data out
# Exclude rows where 'Area Type' is "Country"
index_2020 <- index_2020 %>% filter(`Area Type [Note 3]` != "Country")
index_2021 <- index_2021 %>% filter(`Area Type [Note 3]` != "Country")
head(index_2020)

# Looks fab, but region-level data are mixed with its locale data. Let's segregate
# create a new column called Region that propagates the region name downwards
# Identify regions (where 'Area Type' == "Region")
index_2020 <- index_2020 %>%
  mutate(Region = if_else(`Area Type` == "Region", `Area Name`, NA_character_))
# install zoo pkg for time series analysis and manipulation
install.packages("zoo")
library(zoo)

# Fill down the region name (carry region down to towns)
index_2020$Region <- zoo::na.locf(index_2020$Region, na.rm=FALSE) # first region row should be NA to prevent miscalculation of cumulative

# Check the updated data
head(index_2020) # can't find the region column so let's check for unique values in Area Type

unique(index_2020$`Area Type [Note 3]`)
# Check if 'Region' column exists
"Region" %in% colnames(index_2020)

# Let's rename Area Type Column
index_2020 <- index_2020 %>%
  rename(`Area Type` = `Area Type [Note 3]`)

# For each region, set the first row to NA again
index_2020 <- index_2020 %>%
  group_by(Region) %>%
  mutate(Region = if_else(row_number() == 1, NA_character_, Region)) %>%
  ungroup()

# Check the result
head(index_2020)


# Check the change
colnames(index_2020)

View(index_2020) # 2020 data ready to be loaded to SQL database

# repeat same cleaning process for 2021 data
index_2021 <- index_2021 %>%
  rename(`Area Type` = `Area Type [Note 3]`)

index_2021 <- index_2021 %>%
  mutate(Region = if_else(`Area Type` == "Region", `Area Name`, NA_character_))

index_2021$Region <- zoo::na.locf(index_2021$Region, na.rm=FALSE)

index_2021 <- index_2021 %>%
  group_by(Region) %>%
  mutate(Region = if_else(row_number() == 1, NA_character_, Region)) %>%
  ungroup()

View(index_2021)

# prepare to load
library(writexl)

library(writexl)

# Save both datasets into one Excel file with separate sheets
write_xlsx(list(
  "Health_Index_2021" = index_2021,  # Replace with actual 2021 dataset
  "Health_Index_2020" = index_2020   # Replace with actual 2022 dataset
), "Cleaned_Health_Index.xlsx")

# save as csv, best for quick laoding and analysis, Saves each dataset separately in CSV format.
write.csv(index_2020, "Health_Index_2020.csv", row.names = FALSE)
write.csv(index_2021, "Health_Index_2021.csv", row.names = FALSE)

# install pkg for bigquery
install.packages("bigrquery")
library(bigrquery)

library(bigrquery)

# Authenticate using your service account key and enter Authcode from Tidyverse APi pkg
bq_auth(path = "C:\\Users\\temiloluwa.jokotola\\Downloads\\temiloluwa-jokotola-projects-843995bfa3d1.json")

write.csv(index_2020, "Health_Index_2020.csv", row.names = FALSE)
write.csv(index_2021, "Health_Index_2021.csv", row.names = FALSE)

# Set up bq connection
library(bigrquery)
install.packages("readr")
library(readr)

# Set project and dataset details
project_id <- "temiloluwa-jokotola-projects"
dataset_id <- "england_health_index_dataset" 

# Read CSVs into R as data frames
health_index_2020 <- read_csv("Health_Index_2020.csv")
health_index_2021 <- read_csv("Health_Index_2021.csv")

# Upload Health Index data ... create table if not existing...replace existing data.. use WRITE_APPEND if adding new data
# Upload data frame to BigQuery
bq_table_upload(
  bq_table(project_id, dataset_id, "Health_Index_2020"),
  health_index_2020,  # Pass the data frame instead of file path
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_TRUNCATE"
)

bq_table_upload(
  bq_table(project_id, dataset_id, "Health_Index_2021"),
  health_index_2021,  # Pass the data frame instead of file path
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_TRUNCATE"
)

# loading error due to colnames error. clean colnames with janitor and rerun
install.packages("janitor")
library(janitor)

# Read CSVs into R
health_index_2020 <- read_csv("Health_Index_2020.csv") %>%
  clean_names()  # Fix column names

health_index_2021 <- read_csv("Health_Index_2021.csv") %>%
  clean_names()  # Fix column names
View(health_index_2020)

# Upload cleaned data to BigQuery
bq_table_upload(
  bq_table(project_id, dataset_id, "Health_Index_2020"),
  health_index_2020,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_TRUNCATE"
)

bq_table_upload(
  bq_table(project_id, dataset_id, "Health_Index_2021"),
  health_index_2021,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_TRUNCATE"
)

# Voila!!! I hope you find this interesting and the comments helpful :-)