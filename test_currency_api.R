# Test script to check currency conversion API behavior
library(tidyverse)
library(jsonlite)
library(cli)

# Test date (using today)
test_date <- Sys.Date()
date_str <- as.character(test_date)

cli_h1("Testing Currency Conversion API")
cli_alert_info("Date: {date_str}")

# Build URL
url <- paste0(
  "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@",
  date_str,
  "/v1/currencies/usd.json"
)

cli_alert_info("Fetching from: {url}")

# Fetch data
tryCatch({
  rates <- jsonlite::fromJSON(url)
  
  cli_alert_success("Successfully fetched rates")
  
  # Check structure
  cli_h2("API Response Structure")
  cat("Names in rates object:", paste(names(rates), collapse = ", "), "\n")
  cat("Type of rates$usd:", class(rates$usd), "\n")
  cat("Length of rates$usd:", length(rates$usd), "\n\n")
  
  # Convert to data frame
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
  
  cli_h2("Processed Conversion Rates")
  cat("Number of currencies:", nrow(conversion_rates), "\n")
  cat("Sample currencies:\n")
  print(head(conversion_rates, 10))
  
  # Check if USD is in the table
  cli_h2("USD Check")
  if ("USD" %in% conversion_rates$main_currency) {
    cli_alert_success("USD is in conversion_rates table")
    usd_rate <- conversion_rates %>% filter(main_currency == "USD") %>% pull(conversion_rate)
    cat("USD conversion_rate:", usd_rate, "\n")
  } else {
    cli_alert_warning("USD is NOT in conversion_rates table (this is the problem!)")
    cat("This means when data has main_currency='USD', the join will return NA\n")
  }
  
  # Check what currencies are available
  cli_h2("Available Currencies")
  cat("Total currencies:", nrow(conversion_rates), "\n")
  cat("First 20 currencies:", paste(head(conversion_rates$main_currency, 20), collapse = ", "), "\n")
  
  # Test what happens with a sample join
  cli_h2("Join Test")
  test_data <- tibble(
    main_currency = c("USD", "EUR", "GBP", "JPY", "XYZ"),
    total_spend_formatted = c(100, 85, 75, 15000, 50)
  )
  
  cat("Test data:\n")
  print(test_data)
  
  cat("\nAfter left_join with conversion_rates:\n")
  test_result <- test_data %>%
    left_join(conversion_rates, by = "main_currency")
  print(test_result)
  
  # Check which ones got NA
  na_currencies <- test_result %>%
    filter(is.na(conversion_rate)) %>%
    pull(main_currency)
  
  if (length(na_currencies) > 0) {
    cli_alert_warning("Currencies with NA conversion_rate: {paste(na_currencies, collapse = ', ')}")
  } else {
    cli_alert_success("All test currencies found conversion rates")
  }
  
  # Check conversion_rate values
  cli_h2("Conversion Rate Values")
  cat("Min conversion_rate:", min(conversion_rates$conversion_rate, na.rm = TRUE), "\n")
  cat("Max conversion_rate:", max(conversion_rates$conversion_rate, na.rm = TRUE), "\n")
  cat("Mean conversion_rate:", mean(conversion_rates$conversion_rate, na.rm = TRUE), "\n")
  cat("Any zero rates?", any(conversion_rates$conversion_rate == 0, na.rm = TRUE), "\n")
  cat("Any NA rates?", any(is.na(conversion_rates$conversion_rate)), "\n")
  
}, error = function(e) {
  cli_alert_danger("Error fetching rates: {e$message}")
  cli_alert_info("Trying yesterday's date...")
  
  # Try yesterday
  yesterday_date <- test_date - 1
  yesterday_str <- as.character(yesterday_date)
  url_yesterday <- paste0(
    "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@",
    yesterday_str,
    "/v1/currencies/usd.json"
  )
  
  tryCatch({
    rates <- jsonlite::fromJSON(url_yesterday)
    cli_alert_success("Successfully fetched rates for {yesterday_str}")
    # Run same checks...
  }, error = function(e2) {
    cli_alert_danger("Failed for yesterday too: {e2$message}")
  })
})
