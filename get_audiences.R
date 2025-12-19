# Get command-line arguments
tf <- commandArgs(trailingOnly = TRUE)

here::i_am("wtm_kr.Rproj")

source("utils.R")

library(tidyverse)
library(lubridate)
library(metatargetr)

sets <- jsonlite::fromJSON("settings.json")

if(Sys.info()[["sysname"]]=="Windows"){
  ### CHANGE ME WHEN LOCAL!
  tf <- "30"
  print(paste0("TF: ", tf))
}

# Determine the date to use for get_targeting_db
# Use a date 3 days ago (as per metatargetr requirement) or use latest available
target_date <- Sys.Date() - lubridate::days(3)
target_date_str <- as.character(target_date)

# Try to get the latest available date from existing data
try({
  latest_elex <- readRDS(paste0("data/election_dat", tf, ".rds"))
}, silent = TRUE)

if(!exists("latest_elex")){
  latest_elex <- tibble()
}

if(!("ds" %in% names(latest_elex))){
  latest_elex <- latest_elex %>% mutate(ds = "")
}

latest_ds <- latest_elex %>% 
  filter(!is.na(ds), ds != "") %>% 
  arrange(ds) %>% 
  slice(1) %>% 
  pull(ds)

if(length(latest_ds)==0 || latest_ds == ""){
  latest_ds <- as.character(target_date)
}

tstamp <- Sys.time()
write_lines(lubridate::as_date(tstamp), "tstamp.txt")

# Retrieve targeting data using metatargetr
cat("\n\nRetrieving targeting data using metatargetr...\n\n")

try({
  # Get targeting data for the specified country and timeframe
  election_dat <- get_targeting_db(
    the_cntry = sets$cntry, 
    tf = tf, 
    ds = target_date_str,
    remove_nas = TRUE,
    verbose = TRUE
  )
  
  # Ensure we have the necessary columns
  if(!("internal_id" %in% names(election_dat)) && "page_id" %in% names(election_dat)){
    election_dat <- election_dat %>% 
      rename(internal_id = page_id)
  }
  
  # Add timestamp
  election_dat <- election_dat %>% 
    mutate(tstamp = tstamp)
  
  # Process total_spend_formatted if it exists
  if("total_spend_formatted" %in% names(election_dat)){
    election_dat <- election_dat %>%
      mutate_at(vars(contains("total_spend_formatted")), ~parse_number(as.character(.x)))
  }
  
  # Ensure we have page_id column (use internal_id if page_id doesn't exist)
  if(!"page_id" %in% names(election_dat) && "internal_id" %in% names(election_dat)){
    election_dat <- election_dat %>% 
      rename(page_id = internal_id)
  }
  
  # Get unique advertisers
  all_dat <- election_dat %>%
    distinct(page_id, page_name, .keep_all = TRUE) %>%
    select(page_id, page_name) %>%
    filter(!is.na(page_id), !is.na(page_name)) %>%
    mutate(sources = "metatargetr")
  
  # Save advertiser list
  saveRDS(all_dat, "data/all_dat.rds")
  
  # Save the full election data
  dir.create(paste0("historic/", target_date_str), recursive = TRUE)
  current_date_path <- paste0("historic/", target_date_str, "/", tf, ".rds")
  saveRDS(election_dat, file = current_date_path)
  
  # Save to main data file
  saveRDS(election_dat, paste0("data/election_dat", tf, ".rds"))
  
  cat("\n\nData retrieval complete!\n\n")
  cat("Total rows:", nrow(election_dat), "\n")
  cat("Unique advertisers:", nrow(all_dat), "\n")
  
}, error = function(e){
  cat("\n\nError retrieving data:", conditionMessage(e), "\n\n")
  # Create empty structure if retrieval fails
  election_dat <- tibble(
    internal_id = character(),
    page_id = character(),
    page_name = character(),
    ds = character(),
    tstamp = as.POSIXct(character())
  )
  all_dat <- tibble(
    page_id = character(),
    page_name = character(),
    sources = character()
  )
  saveRDS(election_dat, paste0("data/election_dat", tf, ".rds"))
  saveRDS(all_dat, "data/all_dat.rds")
})

##### combinations ####

# Process historical data if it exists
minimum_date <- tryCatch({
  dir("historic", recursive = TRUE, full.names = FALSE) %>%
    keep(~str_detect(.x, paste0(tf, "\\.rds$"))) %>% 
    str_remove("/.*") %>%
    as.Date() %>%
    min(na.rm = TRUE)
}, error = function(e) {
  as.Date("2023-01-01")
})

if("ds" %in% names(election_dat) && nrow(election_dat) > 0){
  
  try({
    
    latest_ds <- election_dat %>% 
      filter(!is.na(ds), ds != "") %>%
      arrange(ds) %>% 
      slice(1) %>% 
      pull(ds) %>% 
      as.Date()
    
    if(length(latest_ds) > 0 && !is.na(latest_ds)){
      
      begintf <- as.Date(latest_ds) - lubridate::days(as.numeric(tf))
      
      date_vector <- vector()
      current_date <- latest_ds
      index <- 1
      
      while(current_date > minimum_date && index < 100) {  # Limit to prevent infinite loops
        
        date_vector[index] <- current_date
        
        current_date <- current_date - lubridate::days(as.numeric(tf))
        
        index <- index + 1
        
      }
      
      if(length(date_vector) > 0){
        
        combined_dat <- paste0("historic/", as_date(date_vector), "/", tf, ".rds") %>%
          map_dfr(~{
            if(!file.exists(.x)){
              return(tibble(ds = as.character(begintf), missing_report = TRUE))
            } else {
              tryCatch({
                readRDS(.x)
              }, error = function(e) {
                tibble(ds = as.character(begintf), missing_report = TRUE)
              })
            }
          })
        
        if(nrow(combined_dat) > 0){
          saveRDS(combined_dat, file = paste0("data/combined_dat", tf, ".rds"))
          
          # Aggregate data
          if("total_spend_formatted" %in% names(combined_dat)){
            aggr <- combined_dat  %>%
              mutate(total_spend = readr::parse_number(as.character(total_spend_formatted))) %>%
              mutate(total_spend = ifelse(total_spend == 50, 50, total_spend)) %>%
              mutate(total_spend = total_spend * total_spend_pct) %>%
              group_by(page_id, value, type, location_type, detailed_type, custom_audience_type, is_exclusion) %>%
              summarize(
                total_spend = sum(total_spend, na.rm = TRUE),
                num_ads = sum(num_ads, na.rm = TRUE),
                num_obfuscated = sum(num_obfuscated, na.rm = TRUE)
              ) %>%
              ungroup()
            
            saveRDS(aggr, file = paste0("data/election_dat_aggr", tf, ".rds"))
          }
        }
      }
    }
    
  }, silent = TRUE)
  
}

unlink("node_modules", recursive = TRUE, force = TRUE)
unlink("out", recursive = TRUE, force = TRUE)
