# Test script to check what's in the actual data file
library(tidyverse)
library(arrow)
library(cli)

cli_h1("Testing Currency Join with Actual Data")

# Test with NL data
iso2c <- "NL"
ds <- "2025-12-11"
timeframe <- "30"

url <- paste0(
  "https://github.com/favstats/meta_ad_targeting/releases/download/",
  iso2c, "-last_", timeframe, "_days/",
  ds, ".parquet"
)

cli_alert_info("Fetching data from: {url}")

tryCatch({
  dat <- arrow::read_parquet(url)
  
  cli_alert_success("Successfully loaded data")
  cat("Rows:", nrow(dat), "\n")
  cat("Columns:", paste(names(dat), collapse = ", "), "\n\n")
  
  # Check main_currency column
  if ("main_currency" %in% names(dat)) {
    cli_h2("main_currency Column Analysis")
    
    # Check for NA values
    na_count <- sum(is.na(dat$main_currency))
    cat("NA values:", na_count, "\n")
    
    # Get unique currencies
    unique_currencies <- dat %>%
      distinct(main_currency) %>%
      filter(!is.na(main_currency)) %>%
      pull(main_currency)
    
    cat("Unique currencies:", length(unique_currencies), "\n")
    cat("Currencies:", paste(head(unique_currencies, 20), collapse = ", "), "\n")
    if (length(unique_currencies) > 20) cat("... and", length(unique_currencies) - 20, "more\n")
    
    # Check data type
    cat("\nData type:", class(dat$main_currency), "\n")
    
    # Check if any are factors
    if (is.factor(dat$main_currency)) {
      cat("Factor levels:", paste(levels(dat$main_currency), collapse = ", "), "\n")
    }
    
    # Check case
    cat("\nCase check:\n")
    upper_currencies <- unique(str_to_upper(unique_currencies))
    cat("After str_to_upper, unique count:", length(upper_currencies), "\n")
    
    # Now test the join
    cli_h2("Testing Join with Conversion Rates")
    
    # Get conversion rates (same as main script)
    date_str <- as.character(Sys.Date())
    rates_url <- paste0(
      "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@",
      date_str,
      "/v1/currencies/usd.json"
    )
    
    tryCatch({
      rates <- jsonlite::fromJSON(rates_url)
      conversion_rates <- rates$usd %>%
        enframe() %>%
        unnest(value) %>%
        distinct(name, .keep_all = TRUE) %>%
        mutate(
          main_currency = str_to_upper(name),
          conversion_rate = value
        ) %>%
        filter(str_length(main_currency) == 3) %>%
        select(main_currency, conversion_rate)
      
      cat("Conversion rates loaded:", nrow(conversion_rates), "currencies\n")
      cat("USD in rates?", "USD" %in% conversion_rates$main_currency, "\n\n")
      
      # Test join with sample of data
      test_data <- dat %>%
        select(main_currency) %>%
        distinct() %>%
        filter(!is.na(main_currency)) %>%
        slice_head(n = 20)
      
      cat("Sample currencies from data:\n")
      print(test_data)
      
      # Try join
      test_join <- test_data %>%
        mutate(main_currency_upper = str_to_upper(main_currency)) %>%
        left_join(
          conversion_rates %>% mutate(main_currency_upper = main_currency),
          by = "main_currency_upper"
        )
      
      cat("\nAfter join:\n")
      print(test_join)
      
      # Check which ones got NA
      na_after_join <- test_join %>%
        filter(is.na(conversion_rate)) %>%
        pull(main_currency)
      
      if (length(na_after_join) > 0) {
        cli_alert_warning("Currencies with NA after join: {paste(na_after_join, collapse = ', ')}")
      } else {
        cli_alert_success("All sample currencies found conversion rates")
      }
      
    }, error = function(e) {
      cli_alert_danger("Error fetching conversion rates: {e$message}")
    })
    
  } else {
    cli_alert_warning("main_currency column not found in data!")
    cat("Available columns:", paste(names(dat), collapse = ", "), "\n")
  }
  
}, error = function(e) {
  cli_alert_danger("Error loading data: {e$message}")
  # Try yesterday
  cli_alert_info("Trying yesterday's date...")
  ds_yesterday <- as.character(as.Date(ds) - 1)
  url_yesterday <- paste0(
    "https://github.com/favstats/meta_ad_targeting/releases/download/",
    iso2c, "-last_", timeframe, "_days/",
    ds_yesterday, ".parquet"
  )
  # ... could try this too
})
