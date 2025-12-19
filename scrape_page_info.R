# =================================================================
# Scrape Missing Page Info
# =================================================================
# This script runs BEFORE dashboard generation to scrape page info
# for advertisers that are missing from the MetaPageInfos releases.
# =================================================================

library(tidyverse)
library(arrow)
library(glue)
library(cli)
library(here)

# Source the scraping function from meta_ad_targeting
# This provides get_page_insights2() function
tryCatch({
  source("https://raw.githubusercontent.com/favstats/meta_ad_targeting/refs/heads/main/utils.R")
  cli_alert_success("Loaded meta_ad_targeting utils")
}, error = function(e) {

  cli_alert_warning("Could not load meta_ad_targeting utils: {e$message}")
  cli_alert_info("Make sure you have internet access")
})

# =================================================================
# Configuration
# =================================================================

DATA_DIR <- here("data/30")
UPLOAD_TO_GITHUB <- FALSE  # Set to TRUE to upload to GitHub releases

# =================================================================
# Helper: Load existing page info from GitHub releases
# =================================================================
load_page_info <- function(iso2c) {
  url <- glue(
    "https://github.com/favstats/meta_ad_targeting/releases/",
    "download/MetaPageInfos/{iso2c}-page_info.parquet"
  )
  
  tryCatch({
    info <- arrow::read_parquet(url)
    
    # Filter out no_data entries if column exists
    if ("no_data" %in% names(info)) {
      info <- info %>% filter(is.na(no_data))
    }
    
    info %>%
      mutate(page_id = as.character(page_id)) %>%
      distinct(page_id, .keep_all = TRUE)
  }, error = function(e) {
    cli_alert_warning("Could not load page info for {iso2c}: {e$message}")
    tibble(page_id = character())
  })
}

# =================================================================
# Helper: Scrape missing page info for a country
# =================================================================
scrape_missing_page_info <- function(iso2c, dat, existing_info = NULL) {
  # Load existing info if not provided
  if (is.null(existing_info)) {
    existing_info <- load_page_info(iso2c)
  }
  
  # Get all unique page IDs from targeting data
  all_page_ids <- dat %>%
    distinct(page_id) %>%
    pull(page_id) %>%
    as.character()
  
  # Find which ones are missing
  known_ids <- existing_info$page_id
  missing_ids <- setdiff(all_page_ids, known_ids)
  
  if (length(missing_ids) == 0) {
    cli_alert_success("All {length(all_page_ids)} pages have info for {iso2c}")
    return(existing_info)
  }
  
  cli_alert_info("Scraping {length(missing_ids)} missing pages for {iso2c}")
  
  # Check if get_page_insights2 function exists
  if (!exists("get_page_insights2")) {
    cli_alert_danger("get_page_insights2() not available - cannot scrape")
    return(existing_info)
  }
  
  # Scrape missing pages with progress
  new_info <- map_dfr(missing_ids, function(page_id) {
    tryCatch({
      result <- get_page_insights2(page_id, include_info = "page_info")
      if (!is.null(result) && nrow(result) > 0) {
        result %>% mutate_all(as.character)
      } else {
        tibble(page_id = page_id, no_data = TRUE)
      }
    }, error = function(e) {
      cli_alert_warning("Failed to scrape {page_id}: {e$message}")
      tibble(page_id = page_id, no_data = TRUE)
    })
  }, .progress = TRUE)
  
  # Combine and dedupe
  combined <- bind_rows(existing_info, new_info) %>%
    filter(is.na(no_data) | !no_data) %>%
    distinct(page_id, .keep_all = TRUE)
  
  cli_alert_success("Now have {nrow(combined)} pages with info for {iso2c}")
  
  # Optionally upload back to GitHub releases
  if (UPLOAD_TO_GITHUB && nrow(new_info) > 0) {
    tryCatch({
      outfile <- glue("{iso2c}-page_info.parquet")
      arrow::write_parquet(combined, outfile)
      piggyback::pb_upload(
        outfile, 
        repo = "favstats/meta_ad_targeting", 
        tag = "MetaPageInfos"
      )
      unlink(outfile)
      cli_alert_success("Uploaded updated page info to GitHub releases")
    }, error = function(e) {
      cli_alert_warning("Failed to upload to GitHub: {e$message}")
    })
  }
  
  combined
}

# =================================================================
# Main: Process all countries
# =================================================================
scrape_all_countries <- function(countries = NULL) {
  cli_h1("Scrape Missing Page Info")
  
  # Get list of countries with data
  country_files <- list.files(DATA_DIR, "\\.rds$", full.names = FALSE)
  available_countries <- gsub("\\.rds$", "", country_files)
  
  if (!is.null(countries)) {
    available_countries <- intersect(available_countries, countries)
  }
  
  cli_alert_info("Processing {length(available_countries)} countries")
  
  results <- map(available_countries, function(iso2c) {
    cli_h2("Processing {iso2c}")
    
    tryCatch({
      # Load country data
      dat <- readRDS(file.path(DATA_DIR, paste0(iso2c, ".rds")))
      
      # Ensure page_id column exists
      if (!"page_id" %in% names(dat) && "internal_id" %in% names(dat)) {
        dat <- dat %>% mutate(page_id = as.character(internal_id))
      } else if ("page_id" %in% names(dat)) {
        dat <- dat %>% mutate(page_id = as.character(page_id))
      } else {
        cli_alert_warning("No page_id or internal_id column in {iso2c} data")
        return(NULL)
      }
      
      # Scrape missing info
      scrape_missing_page_info(iso2c, dat)
    }, error = function(e) {
      cli_alert_danger("Error processing {iso2c}: {e$message}")
      NULL
    })
  }) %>%
    set_names(available_countries)
  
  cli_h1("Complete")
  cli_alert_success("Processed {length(compact(results))} countries successfully")
  
  invisible(results)
}

# =================================================================
# Run if executed directly
# =================================================================
if (sys.nframe() == 0) {
  # Process all countries (or specify a subset)
  # scrape_all_countries(countries = c("US", "DE", "GB"))
  scrape_all_countries()
}
