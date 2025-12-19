# Usage Instructions

## Overview

The workflow has been refactored to:
- Use `metatargetr::get_targeting_db()` instead of direct Facebook scraping
- Use `dashboardr` package instead of Quarto for dashboard generation
- Work with advertisers directly (no party information required)

## Prerequisites

1. **Install required packages:**
   ```r
   if (!requireNamespace("remotes", quietly = TRUE)) {
     install.packages("remotes")
   }
   
   remotes::install_github("favstats/metatargetr")
   remotes::install_github("favstats/dashboardr")
   ```

2. **Ensure you have the necessary R packages:**
   - `tidyverse`
   - `lubridate`
   - `here`
   - `pacman` (for package management)

## How to Use

### Option 1: Run Full Workflow (All Countries)

Simply run:
```r
source("start.R")
```

This will:
1. Loop through all countries in the country list
2. For each country:
   - Retrieve 30-day and 7-day targeting data using `metatargetr::get_targeting_db()`
   - Process the data using `actor_utils.R` (generates colors, date strings, etc.)
   - Generate a dashboard using `dashboardr` via `create_dashboard.R`
   - Save output to `docs/{COUNTRY_CODE}/`

### Option 2: Test with a Single Country

To test with a single country, modify `start.R` temporarily:

```r
# In start.R, change this line:
# for (cntryy in full_cntry_list$iso2c) {

# To this (for testing):
for (cntryy in "US") {  # or any country code like "NL", "DE", etc.
```

Or create a test script:

```r
# test_single_country.R
source("utils.R")

# Set country
sets <- jsonlite::fromJSON("settings.json")
sets$cntry <- "US"  # Change to your test country
sets$the_country <- "United States"
jsonlite::write_json(sets, "settings.json", simplifyVector = TRUE)

# Get data
target_date <- Sys.Date() - lubridate::days(3)
target_date_str <- as.character(target_date)

library(metatargetr)
election_dat30 <- get_targeting_db("US", "30", target_date_str, remove_nas = TRUE, verbose = TRUE)
election_dat7 <- get_targeting_db("US", "7", target_date_str, remove_nas = TRUE, verbose = TRUE)

# Ensure column names
if(!"internal_id" %in% names(election_dat30) && "page_id" %in% names(election_dat30)){
  election_dat30 <- election_dat30 %>% rename(internal_id = page_id)
}
if(!"page_id" %in% names(election_dat30) && "internal_id" %in% names(election_dat30)){
  election_dat30 <- election_dat30 %>% rename(page_id = internal_id)
}

# Same for election_dat7
if(!"internal_id" %in% names(election_dat7) && "page_id" %in% names(election_dat7)){
  election_dat7 <- election_dat7 %>% rename(internal_id = page_id)
}
if(!"page_id" %in% names(election_dat7) && "internal_id" %in% names(election_dat7)){
  election_dat7 <- election_dat7 %>% rename(page_id = internal_id)
}

# Save data
saveRDS(election_dat30, "data/election_dat30.rds")
saveRDS(election_dat7, "data/election_dat7.rds")

# Process data
source("actor_utils.R")

# Generate dashboard
source("create_dashboard.R")
```

### Option 3: Manual Step-by-Step

1. **Get data for a country:**
   ```r
   library(metatargetr)
   target_date <- Sys.Date() - lubridate::days(3)
   
   election_dat30 <- get_targeting_db("US", "30", as.character(target_date))
   election_dat7 <- get_targeting_db("US", "7", as.character(target_date))
   
   # Save
   saveRDS(election_dat30, "data/election_dat30.rds")
   saveRDS(election_dat7, "data/election_dat7.rds")
   ```

2. **Update settings.json:**
   ```r
   sets <- jsonlite::fromJSON("settings.json")
   sets$cntry <- "US"
   sets$the_country <- "United States"
   jsonlite::write_json(sets, "settings.json", simplifyVector = TRUE)
   ```

3. **Process data:**
   ```r
   source("actor_utils.R")
   ```

4. **Generate dashboard:**
   ```r
   source("create_dashboard.R")
   ```

## Output

The dashboard will be generated in:
- `docs/` - Main output directory
- `docs/{COUNTRY_CODE}/` - Country-specific dashboards

The dashboard includes:
- **Spending page** - Total spend and ad counts by advertiser
- **Targeting page** - Targeting methods breakdown
- **Demographics page** - Age/gender targeting (if data available)
- **Location page** - Geographic targeting
- **Detailed page** - Interest-based targeting
- **About page** - Information about the dashboard

## Important Notes

1. **Date requirement**: `get_targeting_db()` requires a date that is at least 3 days old (data is archived with a 3-day delay)

2. **No party data**: The workflow no longer relies on "who targets me" data. All visualizations work with advertiser names directly.

3. **Data structure**: The data from `metatargetr` should have columns like:
   - `page_id` / `internal_id` - Advertiser ID
   - `page_name` - Advertiser name
   - `type` - Targeting type (age, gender, location, detailed, etc.)
   - `value` - Targeting value
   - `total_spend_formatted` - Spending amount
   - `total_num_ads` - Number of ads

4. **Error handling**: The scripts include error handling for missing data, but if you encounter issues, check:
   - Data files exist in `data/` directory
   - `settings.json` has correct country code
   - Date is at least 3 days old

## Troubleshooting

- **No data retrieved**: Check if the date is at least 3 days old and the country code is valid
- **Dashboard not generating**: Check if `election_dat30.rds` and `election_dat7.rds` exist in `data/` directory
- **Missing columns**: The scripts handle column name variations (page_id vs internal_id), but if you see errors, check the data structure












