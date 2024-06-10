# Data collection: variable selection with vroom
#https://www.tidyverse.org/blog/2019/05/vroom-1-0-0/#column-selection

setwd("C:/Users/LG/Desktop/TFM/FINAL-COUNTRY-DATA") 


##########LIBRARIES
library(vroom)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)

# Function to process CSV files for a given country
process_country <- function(country_code, folder_path) {
  # Set the path to the country's folder containing the CSV files
  country_folder <- paste0(folder_path, "/", country_code)
  
  # List all CSV files in the country's folder
  csv_files <- list.files(country_folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to store data frames
  data_list <- list()
  
  # Loop through each CSV file
  for (csv_file in csv_files) {
    # Read the CSV file and select the desired columns
    if (file.exists(csv_file)) {
      year <- substr(csv_file, nchar(csv_file) - 9, nchar(csv_file) - 6)
      
      if (!is.na(year) && as.numeric(year) < 2011) {
        df <- vroom(csv_file, col_select = c("COUNTRY", "YEAR", "ISCO88_3D", 
                                             "SEX", "EMPSTAT", "HOMEWORK", "FTPT", 
                                              "SUPVISOR", "HWUSUAL", "EXTRAHRS", "SHIFTWK",
                                             "EVENWK", "NIGHTWK","SATWK", "SUNWK"))
      } else {
        df <- vroom(csv_file, col_select = c("COUNTRY", "YEAR", "ISCO08_3D", 
                                             "SEX", "EMPSTAT", "HOMEWORK", "FTPT", 
                                             "SUPVISOR", "HWUSUAL","EXTRAHRS","SHIFTWK",
                                             "EVENWK", "NIGHTWK","SATWK", "SUNWK"))
      }
      
      # Store the data frame in the list
      data_list[[csv_file]] <- df
    }
  }
  
  # Standardize column names across all data frames in data_list
  data_list <- lapply(data_list, function(df) {
    colnames(df) <- c("COUNTRY", "YEAR", "ISCO", 
                      "SEX", "EMPSTAT", "HOMEWORK", "FTPT", 
                      "SUPVISOR", "HWUSUAL","EXTRAHRS",
                      "SHIFTWK","EVENWK", "NIGHTWK", 
                      "SATWK", "SUNWK")
    return(df)
  })
  
  # Combine data frames from all CSV files into a single data frame
  country_final <- do.call(rbind, data_list)
  
  # Write the final dataset to a CSV file
  write.csv(country_final, paste0(country_code, ".csv"), row.names = FALSE)
}

# Specify the folder path
folder_path <- "C:/Users/LG/Desktop/TFM/LFS - microdata"

# List of country codes
country_codes <- c("BE-Belgium","DE-Germany","DK-Denmark","EL-Greece","ES-Spain","FI-Finland",
                   "FR-France","HU-Hungary","IT-Italy","LT-Lithuania","NL-Netherlands","NO-Norway",
                   "PL-Poland","PT-Portugal","RO-Romania","SE-Sweden")

# Process each country
for (country_code in country_codes) {
  process_country(country_code, folder_path)
}


