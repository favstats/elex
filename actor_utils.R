# Actor utilities - refactored from party_utils.R
# Removed all party-based logic, now works with advertisers directly

library(tidyverse)
library(lubridate)

here::i_am("elex.Rproj")

# cntry.R is empty, so we don't need to source it
# source(here::here("cntry.R"))

sets <- jsonlite::fromJSON(here::here("settings.json"))

options(scipen = 999)

# Function to set colors for advertisers
setColors <- function(df) {
  # Check if the 'color' column exists
  if (!"color" %in% names(df)) {
    df <- df %>% mutate(color = NA)
  }
  
  # Function to generate a random color
  generateRandomColor <- function() {
    sprintf("#%06X", sample(0:16777215, 1)) # Generates a random hex color
  }
  
  # Apply the function to each row
  df$color <- sapply(df$color, function(color) {
    if (is.na(color) || nchar(color) < 5) {
      return(generateRandomColor())
    } else {
      return(color)
    }
  })
  
  return(df)
}

# Load election data (30 and 7 day timeframes)
try({
  election_dat30 <- readRDS(paste0("data/election_dat30.rds"))
}, silent = TRUE)

try({
  election_dat7 <- readRDS(paste0("data/election_dat7.rds"))
}, silent = TRUE)

# If data doesn't exist, create empty structure
if(!exists("election_dat30")){
  election_dat30 <- tibble(
    internal_id = character(),
    page_id = character(),
    page_name = character(),
    ds = character()
  )
}

if(!exists("election_dat7")){
  election_dat7 <- tibble(
    internal_id = character(),
    page_id = character(),
    page_name = character(),
    ds = character()
  )
}

# Ensure we have internal_id column
if(!"internal_id" %in% names(election_dat30) && "page_id" %in% names(election_dat30)){
  election_dat30 <- election_dat30 %>% 
    mutate(internal_id = page_id)
}

if(!"internal_id" %in% names(election_dat7) && "page_id" %in% names(election_dat7)){
  election_dat7 <- election_dat7 %>% 
    mutate(internal_id = page_id)
}

# Create color scheme for advertisers
# Get unique advertisers from the data
all_advertisers <- bind_rows(
  election_dat30 %>% select(page_id, page_name) %>% distinct(),
  election_dat7 %>% select(page_id, page_name) %>% distinct()
) %>%
  distinct(page_id, .keep_all = TRUE) %>%
  filter(!is.na(page_id), !is.na(page_name)) %>%
  arrange(page_name)

# Generate colors for advertisers
if(nrow(all_advertisers) > 0){
  # Use colorspace for better color distribution
  if(requireNamespace("colorspace", quietly = TRUE)){
    n_colors <- min(nrow(all_advertisers), 20)  # Limit to 20 distinct colors
    color_palette <- colorspace::qualitative_hcl(n_colors, palette = "Dark 3")
    
    # If we have more advertisers than colors, cycle through
    if(nrow(all_advertisers) > n_colors){
      color_palette <- rep(color_palette, length.out = nrow(all_advertisers))
    }
  } else {
    # Fallback to random colors
    color_palette <- replicate(nrow(all_advertisers), 
                               sprintf("#%06X", sample(0:16777215, 1)))
  }
  
  color_dat <- all_advertisers %>%
    mutate(advertiser = page_name) %>%
    mutate(colors = color_palette[1:nrow(.)]) %>%
    select(advertiser, colors)
} else {
  color_dat <- tibble(
    advertiser = character(),
    colors = character()
  )
}

saveRDS(color_dat, here::here("data/color_dat.rds"))

# Process election data - remove party filtering, work with advertisers directly
if("no_data" %in% names(election_dat30)){
  election_dat30 <- election_dat30 %>%
    filter(is.na(no_data) | no_data == FALSE)
}

election_dat30 <- election_dat30 %>%
  mutate(total_spend_formatted = ifelse("dollar_spend" %in% names(.), 
                                        dollar_spend, 
                                        ifelse("total_spend_formatted" %in% names(.), 
                                               total_spend_formatted, 
                                               0)))

if("no_data" %in% names(election_dat7)){
  election_dat7 <- election_dat7 %>%
    filter(is.na(no_data) | no_data == FALSE)
}

election_dat7 <- election_dat7 %>%
  mutate(total_spend_formatted = ifelse("dollar_spend" %in% names(.), 
                                        dollar_spend, 
                                        ifelse("total_spend_formatted" %in% names(.), 
                                               total_spend_formatted, 
                                               0)))

# Create date strings for display
if(nrow(election_dat30) > 0 && "ds" %in% names(election_dat30) && 
   !all(is.na(election_dat30$ds)) && any(election_dat30$ds != "")){
  
  fin <- as.Date(election_dat30$ds[!is.na(election_dat30$ds) & election_dat30$ds != ""][1]) - lubridate::days(1)
  begin7 <- fin - lubridate::days(6)
  begin30 <- fin - lubridate::days(29)
  
} else {
  # Default to current date if no data
  fin <- Sys.Date()
  begin7 <- fin - lubridate::days(6)
  begin30 <- fin - lubridate::days(29)
}

tibble(fin, begin7, begin30) %>% 
  write_csv(here::here("dates.csv"))

# Function to create date strings with suffixes
create_date <- function(x) {
  the_date <- format(x, "%e %b")
  return(trimws(the_date))
}

last7days_string <- paste0(create_date(begin7), " - ", create_date(fin), " ", lubridate::year(fin)) 
last30days_string <- paste0(create_date(begin30), " - ", create_date(fin), " ", lubridate::year(fin)) 

write_lines(last7days_string, "last7days_string.txt")
write_lines(last30days_string, "last30days_string.txt")

# Filter data to keep only relevant advertisers (no party filtering)
only_keep_these30 <- election_dat30 %>% 
  count(internal_id, cntry) %>% 
  add_count(internal_id, sort = TRUE) %>% 
  distinct(internal_id, .keep_all = TRUE) %>% 
  select(-n, -contains("nn"))

only_keep_these7 <- election_dat7 %>% 
  count(internal_id, cntry) %>% 
  add_count(internal_id, sort = TRUE) %>% 
  distinct(internal_id, .keep_all = TRUE) %>% 
  select(-n, -contains("nn"))

election_dat30 <- election_dat30 %>% 
  inner_join(only_keep_these30, by = c("internal_id", "cntry"))

election_dat7 <- election_dat7 %>% 
  inner_join(only_keep_these7, by = c("internal_id", "cntry"))

# Currency symbol (default to $, can be updated based on data)
currency_symbol <- "$"

if(nrow(election_dat30) > 0 && "main_currency" %in% names(election_dat30)){
  the_currency <- election_dat30 %>%
    filter(!is.na(main_currency)) %>%
    count(main_currency, sort = TRUE) %>%
    slice(1) %>%
    pull(main_currency)
  
  if(length(the_currency) > 0 && !is.na(the_currency)){
    # Try to get currency symbol
    if(requireNamespace("priceR", quietly = TRUE)){
      try({
        currency_symbol <- priceR::currency_info %>%
          filter(iso_code == the_currency) %>%
          pull(symbol)
        
        if(length(currency_symbol) == 0 || is.na(currency_symbol)){
          currency_symbol <- the_currency
        }
      }, silent = TRUE)
    } else {
      currency_symbol <- the_currency
    }
  }
}

# Scale functions for advertisers (replacing party-based scales)
scale_fill_advertisers <- function(...){
  if(nrow(color_dat) > 0){
    ggplot2:::manual_scale(
      'fill',
      values = setNames(color_dat$colors, color_dat$advertiser),
      ...
    )
  } else {
    ggplot2::scale_fill_discrete(...)
  }
}

scale_color_advertisers <- function(...){
  if(nrow(color_dat) > 0){
    ggplot2:::manual_scale(
      'color',
      values = setNames(color_dat$colors, color_dat$advertiser),
      ...
    )
  } else {
    ggplot2::scale_color_discrete(...)
  }
}

# For backward compatibility, keep old function names but map to advertisers
scale_fill_parties <- scale_fill_advertisers
scale_color_parties <- scale_color_advertisers

