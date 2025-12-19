# =================================================================
# Worldwide Election Ad Targeting Dashboard (Enhanced)
# =================================================================
# Creates a main map dashboard + individual country dashboards
# with comprehensive visualizations, value boxes, enhanced tooltips,
# and professional landing pages.
# =================================================================

library(dashboardr)
library(tidyverse)
library(arrow)
library(countrycode)
library(here)
library(glue)
library(cli)
library(httr)
library(rvest)

# Source utility functions (contains calc_targeting)
source(here("utils.R"))

OUTPUT_DIR <- here("worldwide")
DATA_DIR <- here("data/30")

# =================================================================
# Configuration
# =================================================================
TEST_MODE <- TRUE        # Set to FALSE for full run
TEST_SAMPLE_SIZE <- 1    # Number of countries to process in test mode
REFRESH_DATA <- TRUE     # Set to TRUE to check for latest data from GitHub
FORCE_REFRESH <- FALSE   # Set to TRUE to re-download even if cache is current
SAVE_REFRESHED <- TRUE   # Save refreshed data to DATA_DIR

# Use dollar_spend for consistent cross-country comparison
USE_DOLLAR_SPEND <- TRUE

# =================================================================
# Color Palettes for Actor Types and Parties
# =================================================================
ACTOR_COLORS <- c(
  "Political Party" = "#2563eb",
  "political_party" = "#2563eb",
  "Politician" = "#7c3aed",
  "politician" = "#7c3aed",
  "Government" = "#0891b2",
  "government" = "#0891b2",
  "Public Official" = "#0d9488",
  "public_official" = "#0d9488",
  "Advocacy Group" = "#ea580c",
  "advocacy_group" = "#ea580c",
  "NGO" = "#16a34a",
  "ngo" = "#16a34a",
  "Non-Profit" = "#16a34a",
  "non_profit" = "#16a34a",
  "Union" = "#dc2626",
  "union" = "#dc2626",
  "PAC" = "#9333ea",
  "pac" = "#9333ea",
  "Media" = "#6366f1",
  "media" = "#6366f1",
  "News Organization" = "#6366f1",
  "news_organization" = "#6366f1",
  "Business" = "#64748b",
  "business" = "#64748b",
  "Individual" = "#a855f7",
  "individual" = "#a855f7",
  "Political Parties" = "#2563eb",
  "Politicians" = "#7c3aed",
  "Other" = "#94a3b8",
  "other" = "#94a3b8",
  "Unknown" = "#cbd5e1",
  "unknown" = "#cbd5e1"
)

VALUE_BOX_COLORS <- c("#2563eb", "#7c3aed", "#0891b2", "#ea580c", "#16a34a")

# =================================================================
# Formatting Helper Functions
# =================================================================

#' Format currency for display (K, M, B suffixes)
format_currency <- function(x, currency = "$", decimals = 1) {
  if (is.na(x) || is.null(x) || x == 0) return(paste0(currency, "0"))
  
  if (abs(x) >= 1e9) {
    return(paste0(currency, round(x / 1e9, decimals), "B"))
  } else if (abs(x) >= 1e6) {
    return(paste0(currency, round(x / 1e6, decimals), "M"))
  } else if (abs(x) >= 1e3) {
    return(paste0(currency, round(x / 1e3, decimals), "K"))
  } else {
    return(paste0(currency, round(x, 0)))
  }
}

#' Format number for display (K, M, B suffixes)
format_number <- function(x, decimals = 1) {
  if (is.na(x) || is.null(x) || x == 0) return("0")
  
  if (abs(x) >= 1e9) {
    return(paste0(round(x / 1e9, decimals), "B"))
  } else if (abs(x) >= 1e6) {
    return(paste0(round(x / 1e6, decimals), "M"))
  } else if (abs(x) >= 1e3) {
    return(paste0(round(x / 1e3, decimals), "K"))
  } else {
    return(as.character(round(x, 0)))
  }
}

# =================================================================
# Data Refresh: Fetch latest data from GitHub releases
# =================================================================

#' Get available countries and their latest dates from GitHub releases
get_available_releases <- function(timeframe = "30", countries = NULL) {
  if (!is.null(countries)) {
    full_cntry_list <- countries
    cli_alert_info("Querying GitHub releases for {length(countries)} countries...")
  } else {
    full_cntry_list <- countrycode::codelist %>%
      filter(!is.na(iso2c)) %>%
      distinct(iso2c) %>%
      pull(iso2c)
    cli_alert_info("Querying GitHub releases for ALL {length(full_cntry_list)} countries...")
  }
  
  releases <- map_dfr(full_cntry_list, function(iso2c) {
    tag <- paste0(iso2c, "-last_", timeframe, "_days")
    url <- paste0("https://github.com/favstats/meta_ad_targeting/releases/expanded_assets/", tag)
    
    tryCatch({
      resp <- httr::GET(url, httr::timeout(10))
      if (httr::status_code(resp) != 200) return(NULL)
      
      content <- httr::content(resp)
      rows <- rvest::html_elements(content, ".Box-row")
      
      if (length(rows) == 0) return(NULL)
      
      map_dfr(rows, function(row) {
        text <- rvest::html_text(row)
        lines <- strsplit(text, "\n")[[1]]
        lines <- trimws(lines)
        lines <- lines[lines != ""]
        
        if (length(lines) < 3) return(NULL)
        
        filename <- lines[1]
        if (filename == "Source code" || !grepl("\\.parquet$", filename)) return(NULL)
        
        tibble(
          iso2c = iso2c,
          tag = tag,
          filename = filename,
          ds = gsub("\\.parquet$", "", filename)
        )
      })
    }, error = function(e) NULL)
  }, .progress = TRUE)
  
  if (nrow(releases) == 0) {
    cli_alert_danger("No releases found!")
    return(NULL)
  }
  
  releases %>%
    arrange(desc(ds)) %>%
    group_by(iso2c) %>%
    slice(1) %>%
    ungroup()
}

#' Get current currency conversion rates
get_conversion_rates <- function(date = Sys.Date()) {
  date_str <- as.character(date)
  url <- paste0(
    "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@",
    date_str,
    "/v1/currencies/usd.json"
  )
  
  cli_alert_info("Fetching conversion rates for {date_str}...")
  
  tryCatch({
    rates <- jsonlite::fromJSON(url)
    
    rates$usd %>%
      enframe() %>%
      unnest(value) %>%
      distinct(name, .keep_all = TRUE) %>%
      mutate(
        main_currency = str_to_upper(name),
        conversion_rate = value
      ) %>%
      filter(str_length(main_currency) == 3) %>%
      select(main_currency, conversion_rate)
  }, error = function(e) {
    cli_alert_warning("Could not fetch rates for {date_str}, trying yesterday...")
    get_conversion_rates(date - 1)
  })
}

#' Fetch fresh data for a single country
fetch_country_data <- function(iso2c, ds, conversion_rates, timeframe = "30") {
  url <- paste0(
    "https://github.com/favstats/meta_ad_targeting/releases/download/",
    iso2c, "-last_", timeframe, "_days/",
    ds, ".parquet"
  )
  
  tryCatch({
    dat <- arrow::read_parquet(url)
    
    if (nrow(dat) == 0) return(NULL)
    
    dat <- dat %>%
      mutate(cntry = iso2c) %>%
      filter(is.na(no_data))
    
    # The conversion_rates API returns rates FROM USD, so USD itself won't be in the table
    # We need to add USD with rate 1.0 for the join to work
    if (!"USD" %in% conversion_rates$main_currency) {
      conversion_rates <- conversion_rates %>%
        bind_rows(tibble(main_currency = "USD", conversion_rate = 1.0))
    }
    
    # Normalize main_currency to uppercase for join (in case of case mismatches)
    dat <- dat %>%
      mutate(
        main_currency_upper = ifelse(
          is.na(main_currency),
          NA_character_,
          str_to_upper(main_currency)
        )
      )
    
    dat <- dat %>%
      left_join(
        conversion_rates %>% mutate(main_currency_upper = main_currency),
        by = "main_currency_upper"
      ) %>%
      select(-main_currency_upper)
    
    # Check for missing conversion rates
    missing_rates <- dat %>%
      filter(!is.na(main_currency) & is.na(conversion_rate)) %>%
      distinct(main_currency) %>%
      pull(main_currency)
    
    if (length(missing_rates) > 0) {
      cli_alert_warning("  Missing conversion rates for currencies: {paste(missing_rates, collapse = ', ')}. These rows will have NA dollar_spend.")
    }
    
    # Count rows with NA main_currency
    na_currency_count <- sum(is.na(dat$main_currency))
    if (na_currency_count > 0) {
      cli_alert_info("  Found {na_currency_count} rows with NA main_currency. These will have NA dollar_spend.")
    }
    
    dat <- dat %>%
      mutate(
        total_spend_formatted = ifelse(
          total_spend_formatted == 100, 1, total_spend_formatted
        )
      )
    
    # Calculate dollar_spend - handle NA conversion_rate gracefully
    # Only calculate if we have both total_spend_formatted and conversion_rate
    dat <- dat %>%
      mutate(
        dollar_spend = ifelse(
          is.na(conversion_rate) | conversion_rate == 0 | is.na(total_spend_formatted),
          NA_real_,
          total_spend_formatted / conversion_rate
        )
      )
    
    dat
  }, error = function(e) {
    cli_alert_warning("Failed to fetch {iso2c}: {e$message}")
    NULL
  })
}

#' Check if cached data is up-to-date
check_cached_data <- function(iso2c, latest_ds) {
  cache_file <- file.path(DATA_DIR, paste0(iso2c, ".rds"))
  
  if (!file.exists(cache_file)) {
    return(list(up_to_date = FALSE, reason = "no cache"))
  }
  
  tryCatch({
    cached <- readRDS(cache_file)
    cached_ds <- unique(cached$ds)[1]
    
    if (is.na(cached_ds)) {
      return(list(up_to_date = FALSE, reason = "no date in cache"))
    }
    
    if (cached_ds >= latest_ds) {
      return(list(up_to_date = TRUE, data = cached, cached_ds = cached_ds))
    } else {
      return(list(up_to_date = FALSE, reason = paste("outdated:", cached_ds, "vs", latest_ds)))
    }
  }, error = function(e) {
    list(up_to_date = FALSE, reason = paste("read error:", e$message))
  })
}

#' Refresh all country data (with smart caching)
refresh_all_data <- function(countries = NULL, timeframe = "30", force = FALSE) {
  cli_h1("Refreshing Data from GitHub")
  
  releases <- get_available_releases(timeframe, countries = countries)
  if (is.null(releases) || nrow(releases) == 0) return(NULL)
  
  cli_alert_success("Found {nrow(releases)} countries with data")
  
  latest_date <- max(as.Date(releases$ds), na.rm = TRUE)
  cli_alert_info("Latest data date: {latest_date}")
  
  conversion_rates <- get_conversion_rates(latest_date)
  cli_alert_success("Loaded {nrow(conversion_rates)} currency conversion rates")
  
  cli_h2("Fetching country data")
  
  n_cached <- 0
  n_fetched <- 0
  n_failed <- 0
  
  data_list <- releases$iso2c %>%
    set_names() %>%
    map(function(iso2c) {
      ds <- releases$ds[releases$iso2c == iso2c]
      
      if (!force) {
        cache_check <- check_cached_data(iso2c, ds)
        
        if (cache_check$up_to_date) {
          cli_alert_success("  {iso2c}: Using cached data ({cache_check$cached_ds})")
          n_cached <<- n_cached + 1
          return(cache_check$data)
        }
      }
      
      cli_alert_info("  {iso2c}: Fetching fresh data ({ds})...")
      dat <- fetch_country_data(iso2c, ds, conversion_rates, timeframe)
      
      if (is.null(dat)) {
        n_failed <<- n_failed + 1
        return(NULL)
      }
      
      n_fetched <<- n_fetched + 1
      
      if (SAVE_REFRESHED) {
        dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
        saveRDS(dat, file.path(DATA_DIR, paste0(iso2c, ".rds")))
      }
      
      dat
    }, .progress = TRUE) %>%
    compact()
  
  cli_h2("Refresh Summary")
  cli_alert_success("Cached (up-to-date): {n_cached}")
  cli_alert_success("Fetched (new/updated): {n_fetched}")
  if (n_failed > 0) cli_alert_warning("Failed: {n_failed}")
  cli_alert_info("Total countries: {length(data_list)}")
  cli_alert_info("Data date: {latest_date}")
  
  list(
    data = data_list,
    date = latest_date,
    conversion_rates = conversion_rates,
    stats = list(cached = n_cached, fetched = n_fetched, failed = n_failed)
  )
}

# =================================================================
# Helper: Load page info from GitHub releases
# =================================================================
load_page_info <- function(iso2c) {
  url <- glue(
    "https://github.com/favstats/meta_ad_targeting/releases/",
    "download/MetaPageInfos/{iso2c}-page_info.parquet"
  )
  tryCatch({
    info <- arrow::read_parquet(url)
    
    if ("no_data" %in% names(info)) {
      info <- info %>% filter(is.na(no_data))
    }
    
    cols_to_select <- intersect(
      c("page_id", "page_name", "page_category"), 
      names(info)
    )
    if (length(cols_to_select) == 0) return(NULL)
    
    info %>%
      select(all_of(cols_to_select)) %>%
      mutate(page_id = as.character(page_id)) %>%
      distinct(page_id, .keep_all = TRUE)
  }, error = function(e) {
    NULL
  })
}

# =================================================================
# Helper: Standardize data structure
# =================================================================
standardize_data <- function(dat, iso2c) {
  if (!"page_id" %in% names(dat)) {
    cli_alert_warning("No page_id column in {iso2c} data")
    return(NULL)
  }
  
  dat <- dat %>%
    mutate(
      page_id = as.character(page_id),
      internal_id = page_id
    )
  
  if (USE_DOLLAR_SPEND && "dollar_spend" %in% names(dat)) {
    dat <- dat %>%
      mutate(
        total_spend = dollar_spend,
        total_spend_local = total_spend_formatted,
        total_spend_formatted = dollar_spend
      )
  } else {
    dat <- dat %>%
      mutate(total_spend = as.numeric(total_spend_formatted))
  }
  
  if ("party" %in% names(dat)) {
    known_parties <- dat %>%
      distinct(page_id, .keep_all = TRUE) %>%
      filter(!is.na(party), party != "unknown") %>%
      nrow()
    
    total_pages <- dat %>% distinct(page_id) %>% nrow()
    
    if (known_parties / total_pages < 0.1) {
      cli_alert_info("  Few known parties ({known_parties}/{total_pages}), trying page_info...")
      page_info <- load_page_info(iso2c)
      
      if (!is.null(page_info) && "page_category" %in% names(page_info)) {
        dat <- dat %>%
          left_join(
            page_info %>% select(page_id, page_category), 
            by = "page_id", 
            suffix = c("", "_info")
          ) %>%
          mutate(
            party = case_when(
              !is.na(party) & party != "unknown" ~ party,
              !is.na(page_category) & str_detect(page_category, regex("Party", ignore_case = TRUE)) ~ "Political Parties",
              !is.na(page_category) & str_detect(page_category, regex("Politician|Candidate", ignore_case = TRUE)) ~ "Politicians",
              !is.na(page_category) & str_detect(page_category, regex("Government", ignore_case = TRUE)) ~ "Government",
              TRUE ~ "Other"
            )
          )
      }
    }
  } else {
    dat <- dat %>% mutate(party = "unknown")
  }
  
  dat
}

# =================================================================
# Helper: Get top parties for tabs
# =================================================================
get_party_tabs <- function(dat, min_spend_pct = 0.01) {
  party_summary <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    filter(!is.na(party), party != "unknown") %>%
    group_by(party) %>%
    summarize(
      total_spend = sum(total_spend, na.rm = TRUE),
      n_advertisers = n(),
      .groups = "drop"
    ) %>%
    mutate(spend_pct = total_spend / sum(total_spend)) %>%
    filter(spend_pct >= min_spend_pct) %>%
    arrange(desc(total_spend))
  
  if (nrow(party_summary) >= 2) {
    return(party_summary$party)
  }
  
  NULL
}

# =================================================================
# Helper: Calculate targeting summary
# =================================================================
calc_country_targeting <- function(dat, iso2c) {
  sets <- list(cntry = iso2c)
  assign("sets", sets, envir = .GlobalEnv)
  
  tryCatch({
    targeting <- calc_targeting(dat)
    
    targeting %>%
      mutate(target = case_when(
        target == "custom_audience" ~ "Custom Audiences",
        target == "lookalike_audience" ~ "Lookalike Audiences",
        target == "interest" ~ "Detailed Targeting",
        target == "regions" ~ "GEOGRAPHY: Regions",
        target == "CITY" ~ "GEOGRAPHY: City",
        target == "zips" ~ "GEOGRAPHY: Postal Code",
        target == "age" ~ "Age",
        target == "language" ~ "Language",
        target == "countries" ~ "GEOGRAPHY: Country",
        str_detect(target, "Gender:") ~ target,
        TRUE ~ target
      )) %>%
      filter(target != "Unknown", !is.na(target)) %>%
      # Add ranking and comparative stats
      mutate(
        rank = row_number(desc(perc)),
        n_methods = n(),
        percentile = round((rank - 1) / n_methods * 100, 0)
      ) %>%
      arrange(desc(perc))
  }, error = function(e) {
    cli_alert_warning("calc_targeting failed for {iso2c}: {e$message}")
    tibble(target = character(), spend_per = numeric(), perc = numeric())
  })
}

# =================================================================
# Helper: Prepare age data with enhanced stats
# =================================================================
prepare_age_data <- function(dat) {
  total_budget <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_spend, na.rm = TRUE)) %>%
    pull(total)
  
  dat %>%
    filter(type == "age") %>%
    filter(!value %in% as.character(13:17)) %>%
    mutate(
      age_group = case_when(
        value %in% as.character(18:24) ~ "18-24",
        value %in% as.character(25:34) ~ "25-34",
        value %in% as.character(35:44) ~ "35-44",
        value %in% as.character(45:54) ~ "45-54",
        value %in% as.character(55:64) ~ "55-64",
        TRUE ~ "65+"
      ),
      spend = total_spend * total_spend_pct
    ) %>%
    group_by(age_group) %>%
    summarize(
      spend = sum(spend, na.rm = TRUE),
      n_ads = n(),
      .groups = "drop"
    ) %>%
    filter(spend > 0) %>%
    mutate(
      age_group = factor(
        age_group, 
        levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
      ),
      perc = spend / total_budget * 100,
      avg_spend_per_ad = ifelse(n_ads > 0, spend / n_ads, 0),
      rank = dense_rank(desc(spend)),
      n_groups = n(),
      percentile = round((rank - 1) / n_groups * 100, 0),
      total_budget = total_budget
    ) %>%
    arrange(age_group)
}

# =================================================================
# Helper: Prepare gender data with enhanced stats
# =================================================================
prepare_gender_data <- function(dat) {
  total_budget <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_spend, na.rm = TRUE)) %>%
    pull(total)
  
  dat %>%
    filter(type == "gender", value != "All") %>%
    mutate(spend = total_spend * total_spend_pct) %>%
    group_by(value) %>%
    summarize(
      spend = sum(spend, na.rm = TRUE),
      n_ads = n(),
      .groups = "drop"
    ) %>%
    filter(spend > 0) %>%
    rename(gender = value) %>%
    mutate(
      perc = spend / total_budget * 100,
      avg_spend_per_ad = ifelse(n_ads > 0, spend / n_ads, 0),
      rank = dense_rank(desc(spend)),
      n_groups = n(),
      percentile = round((rank - 1) / n_groups * 100, 0),
      total_budget = total_budget
    ) %>%
    arrange(desc(spend))
}

# =================================================================
# Helper: Prepare location data with enhanced stats
# =================================================================
prepare_location_data <- function(dat) {
  total_budget <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_spend, na.rm = TRUE)) %>%
    pull(total)
  
  dat %>%
    filter(type == "location") %>%
    filter(!location_type %in% c("countries", "country_groups")) %>%
    filter(!is.na(value), value != "") %>%
    mutate(spend = total_spend * total_spend_pct) %>%
    group_by(value, location_type) %>%
    summarize(
      spend = sum(spend, na.rm = TRUE),
      n_ads = n(),
      n_advertisers = n_distinct(page_id),
      .groups = "drop"
    ) %>%
    filter(spend > 0) %>%
    arrange(desc(spend)) %>%
    slice_head(n = 30) %>%
    mutate(
      location = fct_reorder(value, spend),
      perc = spend / total_budget * 100,
      avg_spend_per_ad = ifelse(n_ads > 0, spend / n_ads, 0),
      rank = row_number(desc(spend)),
      n_locations = n(),
      percentile = round((rank - 1) / n_locations * 100, 0),
      total_budget = total_budget
    )
}

# =================================================================
# Helper: Prepare detailed targeting data with enhanced stats
# =================================================================
prepare_detailed_data <- function(dat) {
  total_budget <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_spend, na.rm = TRUE)) %>%
    pull(total)
  
  dat %>%
    filter(type == "detailed") %>%
    filter(!is.na(value), value != "") %>%
    mutate(spend = total_spend * total_spend_pct) %>%
    group_by(value, detailed_type) %>%
    summarize(
      spend = sum(spend, na.rm = TRUE),
      n_ads = n(),
      n_advertisers = n_distinct(page_id),
      .groups = "drop"
    ) %>%
    filter(spend > 0) %>%
    arrange(desc(spend)) %>%
    slice_head(n = 50) %>%
    mutate(
      interest = fct_reorder(value, spend),
      perc = spend / total_budget * 100,
      avg_spend_per_ad = ifelse(n_ads > 0, spend / n_ads, 0),
      rank = row_number(desc(spend)),
      n_interests = n(),
      percentile = round((rank - 1) / n_interests * 100, 0),
      total_budget = total_budget
    )
}

# =================================================================
# Helper: Prepare top spenders data with enhanced stats
# =================================================================
prepare_spending_data <- function(dat) {
  total_budget <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_spend, na.rm = TRUE)) %>%
    pull(total)
  
  total_ads <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_num_ads, na.rm = TRUE)) %>%
    pull(total)
  
  dat %>%
    filter(!is.na(page_name), page_name != "") %>%
    distinct(page_id, .keep_all = TRUE) %>%
    filter(!is.na(total_spend), total_spend > 0) %>%
    arrange(desc(total_spend)) %>%
    slice_head(n = 20) %>%
    mutate(
      advertiser = fct_reorder(page_name, total_spend),
      perc = total_spend / total_budget * 100,
      ads_perc = total_num_ads / total_ads * 100,
      avg_spend_per_ad = ifelse(total_num_ads > 0, total_spend / total_num_ads, 0),
      rank = row_number(desc(total_spend)),
      n_advertisers = n(),
      percentile = round((rank - 1) / n_advertisers * 100, 0),
      total_budget = total_budget,
      total_ads_all = total_ads
    )
}

# =================================================================
# Helper: Calculate summary statistics for value boxes
# =================================================================
calculate_summary_stats <- function(dat) {
  # Total spend (deduplicated by page_id)
  total_spend <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_spend, na.rm = TRUE)) %>%
    pull(total)
  
  # Total ads
  total_ads <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    summarize(total = sum(total_num_ads, na.rm = TRUE)) %>%
    pull(total)
  
  # Number of unique advertisers
  n_advertisers <- dat %>%
    distinct(page_id) %>%
    nrow()
  
  # Number of unique parties/actor types
  n_parties <- dat %>%
    filter(!is.na(party), party != "unknown") %>%
    distinct(party) %>%
    nrow()
  
  # Average spend per ad
  avg_spend_per_ad <- if (total_ads > 0) total_spend / total_ads else 0
  
  # Top spender
  top_spender <- dat %>%
    distinct(page_id, .keep_all = TRUE) %>%
    arrange(desc(total_spend)) %>%
    slice(1)
  
  # Date range
  if ("ds" %in% names(dat)) {
    latest_date <- max(as.Date(dat$ds), na.rm = TRUE)
    earliest_date <- min(as.Date(dat$ds), na.rm = TRUE)
    date_range <- paste0(format(earliest_date, "%b %d"), " - ", format(latest_date, "%b %d, %Y"))
  } else {
    latest_date <- Sys.Date()
    date_range <- "Date range not available"
  }
  
  list(
    total_spend = total_spend,
    total_ads = total_ads,
    n_advertisers = n_advertisers,
    n_parties = n_parties,
    avg_spend_per_ad = avg_spend_per_ad,
    top_spender_name = top_spender$page_name[1],
    top_spender_spend = top_spender$total_spend[1],
    latest_date = latest_date,
    date_range = date_range
  )
}

# =================================================================
# Create country dashboard with all pages + value boxes + tooltips
# =================================================================
create_country_dashboard <- function(iso2c, dat, country_name) {
  cli_h2("Creating dashboard for {country_name} ({iso2c})")
  
  # Calculate summary statistics for value boxes
  stats <- calculate_summary_stats(dat)
  cli_alert_info("  Summary: {format_currency(stats$total_spend)} spend, {format_number(stats$total_ads)} ads, {stats$n_advertisers} advertisers")
  
  # Get parties for tabs
  party_tabs <- get_party_tabs(dat)
  has_party_tabs <- !is.null(party_tabs) && length(party_tabs) >= 2
  
  if (has_party_tabs) {
    cli_alert_info("  Found {length(party_tabs)} parties for tabs: {paste(head(party_tabs, 5), collapse = ', ')}")
  }
  
  # === SPENDING PAGE ===
  cli_alert_info("  Preparing spending data...")
  spending_data <- prepare_spending_data(dat)
  
  if (nrow(spending_data) == 0) {
    cli_alert_warning("No spending data for {iso2c}")
    return(NULL)
  }
  
  # Create spending viz with enhanced tooltips
  if (has_party_tabs && "party" %in% names(spending_data)) {
    spending_viz <- create_viz(type = "bar", horizontal = TRUE)
    
    spending_viz <- spending_viz %>%
      add_viz(
        x_var = "advertiser",
        weight_var = "total_spend",
        tabgroup = "By Party/Total",
        title = "Top Advertisers by Spending",
        subtitle = "Hover for detailed spending breakdown including budget share, ad counts, and rankings",
        y_label = "Total Spend (USD)",
        tooltip_prefix = "💰 Spend: $",
        tooltip_suffix = " USD"
      )
    
    for (party_name in head(party_tabs, 6)) {
      party_data_count <- sum(spending_data$party == party_name, na.rm = TRUE)
      if (party_data_count > 0) {
        spending_viz <- spending_viz %>%
          add_viz(
            x_var = "advertiser",
            weight_var = "total_spend",
            filter = as.formula(paste0("~ party == '", party_name, "'")),
            tabgroup = paste0("By Party/", party_name),
            title = party_name,
            tooltip_prefix = "💰 Spend: $",
            tooltip_suffix = " USD"
          )
      }
    }
  } else {
    spending_viz <- create_viz(type = "bar", horizontal = TRUE) %>%
      add_viz(
        x_var = "advertiser",
        weight_var = "total_spend",
        title = paste("Top Advertisers in", country_name),
        subtitle = "Top 20 advertisers ranked by total advertising spend. Hover for detailed breakdown.",
        y_label = "Total Spend (USD)",
        tooltip_prefix = "💰 Spend: $",
        tooltip_suffix = " USD"
      )
  }
  
  # Spending page content with value boxes + filter
  spending_content <- create_content() %>%
    # Add summary value boxes
    add_value_box_row() %>%
    add_value_box(
      title = "Total Ad Spending",
      value = format_currency(stats$total_spend),
      bg_color = VALUE_BOX_COLORS[1],
      # icon = "ph:currency-dollar",
      description = paste0("Total amount spent on political advertising in ", country_name)
    ) %>%
    add_value_box(
      title = "Total Ads",
      value = format_number(stats$total_ads),
      bg_color = VALUE_BOX_COLORS[2],
      # icon = "ph:newspaper",
      description = "Total number of individual advertisements analyzed"
    ) %>%
    add_value_box(
      title = "Advertisers",
      value = as.character(stats$n_advertisers),
      bg_color = VALUE_BOX_COLORS[3],
      # icon = "ph:users",
      description = "Number of unique advertisers running political ads"
    ) %>%
    add_value_box(
      title = "Avg Spend/Ad",
      value = format_currency(stats$avg_spend_per_ad),
      bg_color = VALUE_BOX_COLORS[4],
      # icon = "ph:chart-bar",
      description = "Average amount spent per individual advertisement"
    ) %>%
    end_value_box_row() %>%
    # Add filter input
    add_input_row(style = "boxed") %>%
    add_input(
      input_id = "advertiser_filter", width = "600px",
      label = "Filter Advertiser:",
      type = "select_multiple",
      filter_var = "party_name",
      options_from = "party_name"
    ) %>%
    end_input_row()
  
  # === TARGETING PAGE ===
  cli_alert_info("  Calculating targeting summary...")
  targeting_data <- calc_country_targeting(dat, iso2c)
  
  targeting_viz <- NULL
  if (nrow(targeting_data) > 0) {
    targeting_data <- targeting_data %>%
      mutate(target = fct_reorder(target, perc))
    
    targeting_viz <- create_viz(type = "bar", horizontal = TRUE) %>%
      add_viz(
        x_var = "target",
        weight_var = "perc",
        title = "Budget Allocation by Targeting Method",
        subtitle = "How advertisers allocate their budgets across different targeting strategies. Higher percentages indicate more heavily used methods.",
        y_label = "% of Total Spend",
        tooltip_prefix = "📊 Budget Share: ",
        tooltip_suffix = "% of total advertising budget",
        color_palette = "#2563eb"
      )
  }
  
  # Targeting page content with value boxes
  targeting_content <- create_content() %>%
    add_value_box_row() %>%
    add_value_box(
      title = "Targeting Methods",
      value = as.character(nrow(targeting_data)),
      bg_color = VALUE_BOX_COLORS[1],
      logo_url = "ph:target",
      description = "Number of different targeting methods used"
    ) %>%
    add_value_box(
      title = "Top Method",
      value = if (nrow(targeting_data) > 0) as.character(targeting_data$target[1]) else "N/A",
      bg_color = VALUE_BOX_COLORS[2],
      logo_url = "ph:trophy",
      description = "Most frequently used targeting method"
    ) %>%
    end_value_box_row()
  
  # === DEMOGRAPHICS PAGE ===
  cli_alert_info("  Preparing demographics data...")
  age_data <- prepare_age_data(dat)
  gender_data <- prepare_gender_data(dat)
  
  demographics_viz <- NULL
  demo_data_list <- list()
  
  if (nrow(age_data) > 0) {
    demo_data_list$age_data <- age_data
  }
  if (nrow(gender_data) > 0) {
    demo_data_list$gender_data <- gender_data
  }
  
  if (length(demo_data_list) > 0) {
    demographics_viz <- create_viz(type = "bar", horizontal = TRUE)
    
    if (nrow(age_data) > 0) {
      demographics_viz <- demographics_viz %>%
        add_viz(
          data = "age_data",
          x_var = "age_group",
          weight_var = "spend",
          tabgroup = "Demographics/Age",
          title = "Age Group Targeting Analysis",
          subtitle = "How advertisers allocate their budgets across different age demographics. Reveals which age groups receive the most advertising attention.",
          y_label = "Ad Spend (USD)",
          tooltip_prefix = "💰 Spend: $",
          tooltip_suffix = " targeting this age group",
          icon = "ph:calendar"
        )
    }
    
    if (nrow(gender_data) > 0) {
      demographics_viz <- demographics_viz %>%
        add_viz(
          data = "gender_data",
          x_var = "gender",
          weight_var = "spend",
          tabgroup = "Demographics/Gender",
          title = "Gender Targeting Analysis",
          subtitle = "Budget allocation across gender demographics. Shows how advertisers prioritize different gender audiences.",
          y_label = "Ad Spend (USD)",
          tooltip_prefix = "💰 Spend: $",
          tooltip_suffix = " targeting this gender",
          icon = "ph:gender-intersex"
        )
    }
  }
  
  # Demographics content with value boxes
  demographics_content <- create_content() %>%
    add_value_box_row() %>%
    add_value_box(
      title = "Age Groups Targeted",
      value = as.character(nrow(age_data)),
      bg_color = VALUE_BOX_COLORS[1],
      # icon = "ph:calendar",
      description = "Number of distinct age groups receiving targeted ads"
    ) %>%
    add_value_box(
      title = "Top Age Group",
      value = if (nrow(age_data) > 0) as.character(age_data$age_group[which.max(age_data$spend)]) else "N/A",
      bg_color = VALUE_BOX_COLORS[2],
      logo_url = "ph:trophy",
      description = "Age group receiving the most ad spend"
    ) %>%
    add_value_box(
      title = "Gender Categories",
      value = as.character(nrow(gender_data)),
      bg_color = VALUE_BOX_COLORS[3],
      logo_url = "ph:gender-intersex",
      description = "Number of gender categories being targeted"
    ) %>%
    end_value_box_row()
  
  # === LOCATION PAGE ===
  cli_alert_info("  Preparing location data...")
  location_data <- prepare_location_data(dat)
  
  location_viz <- NULL
  location_content <- NULL
  if (nrow(location_data) > 0) {
    location_viz <- create_viz(type = "bar", horizontal = TRUE) %>%
      add_viz(
        x_var = "location",
        weight_var = "spend",
        title = "Geographic Targeting Hotspots",
        subtitle = "Top 30 locations receiving the most advertising spend. Reveals geographic strategy and regional priorities.",
        y_label = "Ad Spend (USD)",
        tooltip_prefix = "📍 Location Spend: $",
        tooltip_suffix = " invested in this location",
        color_palette = "#0891b2"
      )
    
    # Location content with value boxes
    location_content <- create_content() %>%
      add_value_box_row() %>%
      add_value_box(
        title = "Locations Targeted",
        value = as.character(nrow(location_data)),
        bg_color = VALUE_BOX_COLORS[1],
        # icon = "ph:map-pin",
        description = "Number of distinct locations being targeted"
      ) %>%
      add_value_box(
        title = "Top Location",
        value = if (nrow(location_data) > 0) as.character(location_data$value[1]) else "N/A",
        bg_color = VALUE_BOX_COLORS[2],
        # icon = "ph:trophy",
        description = "Location receiving the most ad spend"
      ) %>%
      add_value_box(
        title = "Top Location Spend",
        value = if (nrow(location_data) > 0) format_currency(location_data$spend[1]) else "$0",
        bg_color = VALUE_BOX_COLORS[3],
        # icon = "ph:currency-dollar",
        description = "Total spend in the top targeted location"
      ) %>%
      end_value_box_row()
  }
  
  # === DETAILED PAGE ===
  cli_alert_info("  Preparing detailed targeting data...")
  detailed_data <- prepare_detailed_data(dat)
  
  detailed_viz <- NULL
  detailed_content <- NULL
  if (nrow(detailed_data) > 0) {
    detailed_viz <- create_viz(type = "bar", horizontal = TRUE) %>%
      add_viz(
        x_var = "interest",
        weight_var = "spend",
        title = "Interest & Behavior Targeting Analysis",
        subtitle = "Top 50 interests and behaviors being targeted. Shows what audience segments advertisers prioritize most.",
        y_label = "Ad Spend (USD)",
        tooltip_prefix = "🎯 Interest Spend: $",
        tooltip_suffix = " targeting this interest/behavior",
        color_palette = "#7c3aed"
      )
    
    # Detailed content with value boxes and search
    detailed_content <- create_content() %>%
      add_value_box_row() %>%
      add_value_box(
        title = "Interests Targeted",
        value = as.character(nrow(detailed_data)),
        bg_color = VALUE_BOX_COLORS[1],
        # icon = "ph:target",
        description = "Number of unique interests/behaviors being targeted"
      ) %>%
      add_value_box(
        title = "Top Interest",
        value = if (nrow(detailed_data) > 0) {
          interest_name <- as.character(detailed_data$value[1])
          if (nchar(interest_name) > 20) paste0(substr(interest_name, 1, 17), "...") else interest_name
        } else "N/A",
        bg_color = VALUE_BOX_COLORS[2],
        # icon = "ph:star",
        description = "Most heavily targeted interest/behavior"
      ) %>%
      add_value_box(
        title = "Advertisers Using",
        value = if (nrow(detailed_data) > 0) as.character(max(detailed_data$n_advertisers)) else "0",
        bg_color = VALUE_BOX_COLORS[3],
        # icon = "ph:users",
        description = "Max advertisers targeting a single interest"
      ) %>%
      end_value_box_row() %>%
      add_input_row() %>%
      add_input(
        input_id = "interest_search",
        label = "Search Interests:",
        type = "text",
        filter_var = "interest"
      ) %>%
      end_input_row()
  }
  
  # === BUILD DASHBOARD ===
  cli_alert_info("  Building dashboard...")
  
  dashboard <- create_dashboard(
    title = paste("Ad Targeting:", country_name),
    output_dir = file.path(OUTPUT_DIR, iso2c),
    description = paste0("Comprehensive political advertising analysis for ", country_name),
    allow_inside_pkg = TRUE,
    theme = "cosmo",
    value_boxes = TRUE,
    mainfont = "Fira Sans",
    fontsize = "16px"
  )
  
  # Currency info for text
  currency_note <- if (USE_DOLLAR_SPEND) " • Spending in USD" else ""
  
  # Get data date if available
  data_date_note <- if (exists("data_date", envir = .GlobalEnv)) {
    paste0("*Data from: ", format(get("data_date", envir = .GlobalEnv), "%B %d, %Y"), currency_note, "*")
  } else if ("ds" %in% names(dat)) {
    paste0("*Data from: ", format(as.Date(dat$ds[1]), "%B %d, %Y"), currency_note, "*")
  } else {
    if (USE_DOLLAR_SPEND) "*Spending shown in USD.*" else ""
  }
  
  # Landing page intro text
  landing_intro <- paste0(
    "[← Back to World Map](../index.html#)\n\n",
    "# ", country_name, " Political Advertising Dashboard\n\n",
    "## Comprehensive Analysis of Digital Political Advertising\n\n",
    "This dashboard provides an **in-depth analysis** of political advertising spending and targeting strategies in ", country_name, ". ",
    "Explore how advertisers invest their budgets across different demographics, locations, and interest groups.\n\n",
    "### 🎯 Key Insights Available\n\n",
    "- **Top Advertisers**: See who's spending the most on political ads\n",
    "- **Targeting Methods**: Understand how budgets are allocated across targeting strategies\n",
    "- **Demographics**: Explore age and gender targeting patterns\n",
    "- **Geographic Focus**: Discover which locations receive the most ad spend\n",
    "- **Interest Targeting**: See what behaviors and interests are being targeted\n\n",
    "### 💡 How to Use This Dashboard\n\n",
    "**Hover over any chart element** to see detailed tooltips with spending amounts, percentages, rankings, and contextual information. ",
    "Use the navigation tabs above to explore different aspects of the advertising data.\n\n",
    data_date_note
  )
  
  # Add Spending page (landing page)
  dashboard <- dashboard %>%
    add_page(
      name = "Spending",
      data = spending_data,
      visualizations = spending_viz,
      content = spending_content,
      is_landing_page = TRUE,
      icon = "ph:currency-dollar",
      text = landing_intro
    )
  
  # Add Targeting page
  if (!is.null(targeting_viz)) {
    dashboard <- dashboard %>%
      add_page(
        name = "Targeting",
        data = targeting_data,
        visualizations = targeting_viz,
        content = targeting_content,
        icon = "ph:target",
        text = paste0(
          "## Targeting Methods Analysis\n\n",
          "This page shows how advertisers allocate their budgets across different targeting methods. ",
          "Higher percentages indicate methods that receive more advertising investment.\n\n",
          "**Key targeting categories include:**\n\n",
          "- **Custom Audiences**: Targeting based on advertiser's own data\n",
          "- **Lookalike Audiences**: Finding users similar to existing audiences\n",
          "- **Detailed Targeting**: Interest and behavior-based targeting\n",
          "- **Geographic**: Location-based targeting at various levels\n",
          "- **Demographics**: Age, gender, and other demographic attributes\n"
        )
      )
  }
  
  # Add Demographics page
  if (!is.null(demographics_viz) && length(demo_data_list) > 0) {
    dashboard <- dashboard %>%
      add_page(
        name = "Demographics",
        data = demo_data_list,
        visualizations = demographics_viz,
        content = demographics_content,
        icon = "ph:users-three",
        text = paste0(
          "## Demographic Targeting Analysis\n\n",
          "Explore how advertisers target different demographic groups. This reveals strategic decisions about which audiences ",
          "receive the most advertising attention.\n\n",
          "**Age targeting** shows investment across different generations, while **gender targeting** reveals any ",
          "differences in how campaigns reach men vs. women.\n"
        )
      )
  }
  
  # Add Location page
  if (!is.null(location_viz)) {
    dashboard <- dashboard %>%
      add_page(
        name = "Location",
        data = location_data,
        visualizations = location_viz,
        content = location_content,
        icon = "ph:map-trifold",
        text = paste0(
          "## Geographic Targeting Analysis\n\n",
          "Discover which locations receive the most political advertising investment. ",
          "This reveals geographic strategy and regional priorities.\n\n",
          "**Top locations** are ranked by total ad spend, showing where advertisers focus their budgets.\n"
        )
      )
  }
  
  # Add Detailed page
  if (!is.null(detailed_viz)) {
    dashboard <- dashboard %>%
      add_page(
        name = "Interests",
        data = detailed_data,
        visualizations = detailed_viz,
        content = detailed_content,
        icon = "ph:heart",
        text = paste0(
          "## Interest & Behavior Targeting\n\n",
          "This page reveals which interests, behaviors, and audience segments advertisers target most heavily. ",
          "These targeting options allow campaigns to reach specific voter segments based on their online activity and preferences.\n\n",
          "**Use the search box** below to find specific interests or behaviors.\n"
        )
      )
  }
  
  # Add branding
  dashboard <- dashboard %>%
    add_powered_by_dashboardr(size = "large")
  
  page_count <- length(dashboard$pages)
  cli_alert_success("  Dashboard created with {page_count} pages")
  
  dashboard
}

# =================================================================
# 1. Load or Refresh Data
# =================================================================
cli_h1("Worldwide Dashboard Generator (Enhanced)")

test_countries <- if (TEST_MODE) c("NL") else NULL

if (REFRESH_DATA) {
  refresh_result <- refresh_all_data(
    countries = test_countries,
    force = FORCE_REFRESH
  )
  
  if (is.null(refresh_result)) {
    cli_alert_danger("Failed to refresh data, falling back to cached files")
    REFRESH_DATA <- FALSE
  } else {
    data_date <- refresh_result$date
    stats <- refresh_result$stats
    cli_alert_success("Data ready: {stats$cached} cached, {stats$fetched} fetched, date: {data_date}")
  }
}

if (!REFRESH_DATA) {
  cli_alert_info("Loading cached data files from {DATA_DIR}...")
}

country_files <- list.files(DATA_DIR, "\\.rds$", full.names = FALSE)
available_countries <- gsub("\\.rds$", "", country_files)

if (TEST_MODE && !is.null(test_countries)) {
  available_countries <- intersect(test_countries, available_countries)
  if (length(available_countries) == 0) {
    set.seed(42)
    available_countries <- sample(
      gsub("\\.rds$", "", country_files), 
      min(TEST_SAMPLE_SIZE, length(country_files))
    )
  }
  cli_alert_warning("TEST MODE: Processing {length(available_countries)} countries: {paste(available_countries, collapse = ', ')}")
}

cli_alert_success("Found {length(available_countries)} countries with data files")

# Build country summary
country_summary <- map_dfr(available_countries, function(iso2c) {
  tryCatch({
    dat <- readRDS(file.path(DATA_DIR, paste0(iso2c, ".rds")))
    if (nrow(dat) == 0) return(NULL)
    
    spend_col <- if (USE_DOLLAR_SPEND && "dollar_spend" %in% names(dat)) {
      "dollar_spend"
    } else {
      "total_spend_formatted"
    }
    
    dat %>%
      distinct(page_id, .keep_all = TRUE) %>%
      summarize(
        iso2c = iso2c,
        country = countrycode(iso2c, "iso2c", "country.name", warn = FALSE) %||% iso2c,
        total_spend = sum(as.numeric(.data[[spend_col]]), na.rm = TRUE),
        total_ads = sum(as.numeric(total_num_ads), na.rm = TRUE),
        n_advertisers = n_distinct(page_name, na.rm = TRUE)
      )
  }, error = function(e) {
    cli_alert_warning("Error loading {iso2c}: {e$message}")
    NULL
  })
}, .progress = TRUE) %>%
  filter(total_ads > 0, total_spend > 0) %>%
  arrange(desc(total_spend))

cli_alert_success("Countries with data: {nrow(country_summary)}")
cat("\nTop countries by spend (USD):\n")
print(head(country_summary, 10))

# =================================================================
# 2. Create main map dashboard with value boxes
# =================================================================
cli_h1("Creating Main Map Dashboard")

# Calculate global stats for main dashboard
global_stats <- list(
  total_spend = sum(country_summary$total_spend, na.rm = TRUE),
  total_ads = sum(country_summary$total_ads, na.rm = TRUE),
  n_countries = nrow(country_summary),
  n_advertisers = sum(country_summary$n_advertisers, na.rm = TRUE),
  top_country = country_summary$country[1],
  top_country_spend = country_summary$total_spend[1]
)

main_viz <- create_viz() %>%
  add_viz(
    type = "map",
    value_var = "total_spend",
    join_var = "iso2c",
    click_url_template = "{iso2c}/index.html",
    title = "Worldwide Political Ad Spending",
    subtitle = "Click any country to explore detailed targeting analysis. Darker colors indicate higher spending.",
    color_palette = c("#f7fbff", "#2171b5", "#08306b"),
    tooltip_vars = c("country", "total_spend", "total_ads", "n_advertisers"),
    legend_title = "Total Spend (USD)"
  )

# Get data date for display
data_date_display <- if (exists("data_date")) {
  format(data_date, "%B %d, %Y")
} else {
  tryCatch({
    first_dat <- readRDS(file.path(DATA_DIR, paste0(available_countries[1], ".rds")))
    format(as.Date(first_dat$ds[1]), "%B %d, %Y")
  }, error = function(e) "Unknown")
}

# Main dashboard content with value boxes
main_content <- create_content() %>%
  add_value_box_row() %>%
  add_value_box(
    title = "Global Ad Spending",
    value = format_currency(global_stats$total_spend),
    bg_color = VALUE_BOX_COLORS[1],
    # icon = "ph:globe",
    description = "Total political advertising spend across all tracked countries"
  ) %>%
  add_value_box(
    title = "Total Ads Analyzed",
    value = format_number(global_stats$total_ads),
    bg_color = VALUE_BOX_COLORS[2],
    # icon = "ph:newspaper",
    description = "Number of individual political advertisements tracked globally"
  ) %>%
  add_value_box(
    title = "Countries Tracked",
    value = as.character(global_stats$n_countries),
    bg_color = VALUE_BOX_COLORS[3],
    # icon = "ph:flag",
    description = "Number of countries with political ad data available"
  ) %>%
  # add_value_box(
  #   title = "Top Country",
  #   value = global_stats$top_country,
  #   bg_color = VALUE_BOX_COLORS[4],
  #   # icon = "ph:trophy",
  #   description = paste0("Highest spending country: ", format_currency(global_stats$top_country_spend))
  # ) %>%
  end_value_box_row()

main_dashboard <- create_dashboard(
  title = "Who Targets Me - Worldwide",
  output_dir = OUTPUT_DIR,
  description = "Worldwide political ad targeting dashboard with comprehensive analytics",
  allow_inside_pkg = TRUE,
  theme = "cosmo",
  value_boxes = TRUE,
  mainfont = "Fira Sans",
  fontsize = "16px"
) %>%
  add_page(
    name = "World Map",
    data = country_summary,
    visualizations = main_viz,
    content = main_content,
    is_landing_page = TRUE,
    icon = "ph:globe"#,
    # text = paste0(
    #   "# Worldwide Political Ad Targeting Dashboard\n\n",
    #   "## Comprehensive Global Analysis\n\n",
    #   "This dashboard provides **in-depth analysis** of political advertising across ", global_stats$n_countries, " countries worldwide. ",
    #   "Click on any country to explore detailed targeting strategies, demographic breakdowns, and spending patterns.\n\n",
    #   "### 🌍 What You'll Discover\n\n",
    #   "- **Top Advertisers**: Who's spending the most in each country\n",
    #   "- **Targeting Strategies**: How budgets are allocated across different methods\n",
    #   "- **Demographic Focus**: Age and gender targeting patterns\n",
    #   "- **Geographic Priorities**: Which regions receive the most attention\n",
    #   "- **Interest Targeting**: What behaviors and interests campaigns prioritize\n\n",
    #   "### 📊 Global Overview\n\n",
    #   "- **", format_currency(global_stats$total_spend), "** in total political advertising tracked\n",
    #   "- **", format_number(global_stats$total_ads), "** individual advertisements analyzed\n",
    #   "- **", global_stats$n_countries, "** countries with available data\n",
    #   "- **", global_stats$top_country, "** leads with ", format_currency(global_stats$top_country_spend), " in spending\n\n",
    #   "### 💡 How to Use\n\n",
    #   "**Click on any country** on the map to open its detailed dashboard. Each country dashboard includes:\n\n",
    #   "- Value boxes with key statistics\n",
    #   "- Interactive charts with detailed tooltips\n",
    #   "- Multiple analysis pages covering different aspects\n",
    #   "- Filtering and search capabilities\n\n",
    #   "*Data from: ", data_date_display, " • Spending shown in USD*"
    # )
  ) %>%
  add_powered_by_dashboardr(size = "large")

# =================================================================
# 3. Create country dashboards with all pages
# =================================================================
cli_h1("Creating Country Dashboards")

country_dashboards <- country_summary$iso2c %>%
  set_names() %>%
  map(function(iso2c) {
    country_name <- country_summary$country[country_summary$iso2c == iso2c]
    
    tryCatch({
      dat <- readRDS(file.path(DATA_DIR, paste0(iso2c, ".rds")))
      dat <<- standardize_data(dat, iso2c)
      
      if (is.null(dat)) {
        cli_alert_warning("Could not standardize data for {iso2c}")
        return(NULL)
      }
      
      create_country_dashboard(iso2c, dat, country_name)
    }, error = function(e) {
      cli_alert_danger("Error creating dashboard for {iso2c}: {e$message}")
      NULL
    })
  }) %>%
  compact()

cli_alert_success("Created {length(country_dashboards)} country dashboards")

# =================================================================
# 4. Generate all dashboards
# =================================================================
cli_h1("Generating Dashboards")

all_dashboards <- c(list(main = main_dashboard), country_dashboards)

results <- generate_dashboards(
  all_dashboards,
  render = TRUE,
  continue_on_error = TRUE,
  linked = TRUE,
  open = TRUE
)

# =================================================================
# Summary
# =================================================================
successful <- sum(sapply(results, function(r) isTRUE(r$success)))
cli_h1("Generation Complete")
cli_alert_success("Successfully generated: {successful} / {length(results)} dashboards")
cli_alert_info("Output directory: {OUTPUT_DIR}")
cli_alert_info("Main dashboard: {file.path(OUTPUT_DIR, 'docs', 'index.html')}")