# Dashboard generation script using dashboardr
# Replaces the Quarto-based dashboard generation

library(dashboardr)
library(tidyverse)
library(here)

# Load color palette libraries
if(!requireNamespace("RColorBrewer", quietly = TRUE)){
  install.packages("RColorBrewer", repos = "https://cloud.r-project.org")
}
library(RColorBrewer)

source(here("utils.R"))
source(here("actor_utils.R"))

sets <- jsonlite::fromJSON(here("settings.json"))

# Load data
try({
  election_dat30 <- readRDS(paste0("data/election_dat30.rds"))
}, silent = TRUE)

try({
  election_dat7 <- readRDS(paste0("data/election_dat7.rds"))
}, silent = TRUE)

if(!exists("election_dat30")){
  election_dat30 <- tibble()
}

if(!exists("election_dat7")){
  election_dat7 <- tibble()
}

# Load color data if available
try({
  color_dat <- readRDS(here("data/color_dat.rds"))
}, silent = TRUE)

if(!exists("color_dat")){
  color_dat <- tibble(advertiser = character(), colors = character())
}

# Prepare data summaries
if(nrow(election_dat30) > 0){
  sum30 <- election_dat30 %>%
    distinct(internal_id, .keep_all = TRUE) %>%
    mutate(
      total_spend_num = ifelse(is.character(total_spend_formatted), 
                               readr::parse_number(as.character(total_spend_formatted)), 
                               as.numeric(total_spend_formatted)),
      total_num_ads_num = as.numeric(total_num_ads)
    ) %>%
    summarize(
      total_spend_formatted = sum(total_spend_num, na.rm = TRUE),
      total_num_ads = sum(total_num_ads_num, na.rm = TRUE)
    )
} else {
  sum30 <- tibble(total_spend_formatted = 0, total_num_ads = 0)
}

if(nrow(election_dat7) > 0){
  sum7 <- election_dat7 %>%
    distinct(internal_id, .keep_all = TRUE) %>%
    mutate(
      total_spend_num = ifelse(is.character(total_spend_formatted), 
                               readr::parse_number(as.character(total_spend_formatted)), 
                               as.numeric(total_spend_formatted)),
      total_num_ads_num = as.numeric(total_num_ads)
    ) %>%
    summarize(
      total_spend_formatted = sum(total_spend_num, na.rm = TRUE),
      total_num_ads = sum(total_num_ads_num, na.rm = TRUE)
    )
} else {
  sum7 <- tibble(total_spend_formatted = 0, total_num_ads = 0)
}

# Read date strings
try({
  last30days_string <- read_lines("last30days_string.txt")
  last7days_string <- read_lines("last7days_string.txt")
}, silent = TRUE)

if(!exists("last30days_string")){
  last30days_string <- "Last 30 Days"
}

if(!exists("last7days_string")){
  last7days_string <- "Last 7 Days"
}

# Create dashboard
dashboard <- create_dashboard(
  title = ifelse(is.null(sets$dashboard_title), "Targeting Dashboard", sets$dashboard_title),
  output_dir = "docs",
  description = paste("Ad targeting analysis for", ifelse(is.null(sets$the_country), sets$cntry, sets$the_country)),
  search = TRUE,
  theme = "cosmo"
)

# ============================================================================
# SPENDING PAGE
# ============================================================================

# Get the main currency for this country
main_currency_code <- if(nrow(election_dat30) > 0 && "main_currency" %in% names(election_dat30)) {
  unique_currencies <- unique(election_dat30$main_currency[!is.na(election_dat30$main_currency)])
  # Use the most common currency, or fall back to first non-NA
  if(length(unique_currencies) > 0) {
    currency_counts <- table(election_dat30$main_currency[!is.na(election_dat30$main_currency)])
    names(currency_counts)[which.max(currency_counts)]
  } else {
    NULL
  }
} else {
  NULL
}

# Prepare spending data by advertiser - limit to top 20
# Filter by main currency and convert total_spend_formatted to numeric
if(nrow(election_dat30) > 0 && "page_name" %in% names(election_dat30)){
  spending_data_30 <- election_dat30 %>%
    filter(!is.na(page_name))
  
  # Filter by main currency if available
  if(!is.null(main_currency_code) && "main_currency" %in% names(spending_data_30)) {
    spending_data_30 <- spending_data_30 %>%
      filter(main_currency == main_currency_code)
  }
  
  spending_data_30 <- spending_data_30 %>%
    # Convert to numeric and take one row per advertiser
    mutate(total_spend_formatted = as.numeric(total_spend_formatted)) %>%
    distinct(page_name, .keep_all = TRUE) %>%
    mutate(
      total_num_ads_num = as.numeric(total_num_ads)
    ) %>%
    filter(!is.na(total_spend_formatted), total_spend_formatted > 0) %>%
    select(page_name, total_spend = total_spend_formatted, total_ads = total_num_ads_num) %>%
    arrange(desc(total_spend)) %>%
    slice_head(n = 20) %>%  # Top 20 advertisers
    # Convert to factor with proper ordering for visualization
    mutate(
      advertiser = page_name,
      advertiser = fct_reorder(advertiser, total_spend, .desc = FALSE)  # Bottom to top = highest to lowest
    )
} else {
  spending_data_30 <- tibble(advertiser = character(), total_spend = numeric(), total_ads = numeric())
}

if(nrow(election_dat7) > 0 && "page_name" %in% names(election_dat7)){
  spending_data_7 <- election_dat7 %>%
    filter(!is.na(page_name))
  
  # Filter by main currency if available
  if(!is.null(main_currency_code) && "main_currency" %in% names(spending_data_7)) {
    spending_data_7 <- spending_data_7 %>%
      filter(main_currency == main_currency_code)
  }
  
  spending_data_7 <- spending_data_7 %>%
    # Convert to numeric and take one row per advertiser
    mutate(total_spend_formatted = as.numeric(total_spend_formatted)) %>%
    distinct(page_name, .keep_all = TRUE) %>%
    mutate(
      total_num_ads_num = as.numeric(total_num_ads)
    ) %>%
    filter(!is.na(total_spend_formatted), total_spend_formatted > 0) %>%
    select(page_name, total_spend = total_spend_formatted, total_ads = total_num_ads_num) %>%
    arrange(desc(total_spend)) %>%
    slice_head(n = 20) %>%  # Top 20 advertisers
    # Convert to factor with proper ordering for visualization
    mutate(
      advertiser = page_name,
      advertiser = fct_reorder(advertiser, total_spend, .desc = FALSE)  # Bottom to top = highest to lowest
    )
} else {
  spending_data_7 <- tibble(advertiser = character(), total_spend = numeric(), total_ads = numeric())
}

# Create spending visualizations using create_viz + add_viz pattern
# Use a better color palette
n_spending <- length(unique(c(spending_data_30$advertiser, spending_data_7$advertiser)))
spending_colors <- RColorBrewer::brewer.pal(min(11, max(3, n_spending)), "Spectral")
if(length(spending_colors) < 20){
  spending_colors <- rep(spending_colors, length.out = 20)
}

spending_viz <- create_viz(
  type = "bar",
  horizontal = TRUE,
  bar_type = "count",
  color_palette = spending_colors
)

if(nrow(spending_data_30) > 0){
  spending_viz <- spending_viz %>%
    add_viz(
      title = paste("Spending by Advertiser -", last30days_string),
      x_var = "advertiser",
      weight_var = "total_spend",
      y_label = "Total Spend",
      subtitle = paste0("Total: ", currency_symbol, scales::comma(sum(spending_data_30$total_spend, na.rm = TRUE)), " | ", scales::comma(sum(spending_data_30$total_ads, na.rm = TRUE)), " ads (Top 20)"),
      tabgroup = "spending/30days"
    )
}

if(nrow(spending_data_7) > 0){
  spending_viz <- spending_viz %>%
    add_viz(
      title = paste("Spending by Advertiser -", last7days_string),
      x_var = "advertiser",
      weight_var = "total_spend",
      y_label = "Total Spend",
      subtitle = paste0("Total: ", currency_symbol, scales::comma(sum(spending_data_7$total_spend, na.rm = TRUE)), " | ", scales::comma(sum(spending_data_7$total_ads, na.rm = TRUE)), " ads (Top 20)"),
      tabgroup = "spending/7days"
    )
}

# Set tabgroup labels on the visualization
spending_viz <- spending_viz %>%
  set_tabgroup_labels(
    spending = "Spending",
    `30days` = last30days_string,
    `7days` = last7days_string
  )

# DEBUG: Print spending data
cat("\n=== SPENDING DATA DEBUG ===\n")
cat("Main currency:", main_currency_code, "\n")
cat("30-day data rows:", nrow(spending_data_30), "\n")
if(nrow(spending_data_30) > 0) {
  cat("Top 5 (30-day):\n")
  print(head(spending_data_30, 5))
  cat("Total spend (30-day, top 20):", sum(spending_data_30$total_spend, na.rm = TRUE), "\n")
  cat("Total ads (30-day, top 20):", sum(spending_data_30$total_ads, na.rm = TRUE), "\n")
}
cat("7-day data rows:", nrow(spending_data_7), "\n")
if(nrow(spending_data_7) > 0) {
  cat("Top 5 (7-day):\n")
  print(head(spending_data_7, 5))
  cat("Total spend (7-day, top 20):", sum(spending_data_7$total_spend, na.rm = TRUE), "\n")
  cat("Total ads (7-day, top 20):", sum(spending_data_7$total_ads, na.rm = TRUE), "\n")
}

# Combine spending data for the page
spending_data_combined <- bind_rows(
  spending_data_30 %>% mutate(timeframe = "30days"),
  spending_data_7 %>% mutate(timeframe = "7days")
)

# Add spending page
dashboard <- dashboard %>%
  add_page(
    "Spending",
    text = md_text(paste0(
      "# Spending\n\n",
      "Here you can see the expenditures of the top 20 advertisers according to the latest data.\n\n",
      "## ", last30days_string, "\n\n",
      "During this period, the top 20 advertisers spent a total of ", 
      currency_symbol, scales::comma(if(nrow(spending_data_30) > 0) sum(spending_data_30$total_spend, na.rm = TRUE) else 0), 
      " and displayed ", scales::comma(if(nrow(spending_data_30) > 0) sum(spending_data_30$total_ads, na.rm = TRUE) else 0), 
      " advertisements on Meta.\n\n",
      "## ", last7days_string, "\n\n",
      "During this period, the top 20 advertisers spent a total of ", 
      currency_symbol, scales::comma(if(nrow(spending_data_7) > 0) sum(spending_data_7$total_spend, na.rm = TRUE) else 0), 
      " and displayed ", scales::comma(if(nrow(spending_data_7) > 0) sum(spending_data_7$total_ads, na.rm = TRUE) else 0), 
      " advertisements on Meta.\n\n"
    )),
    data = if(nrow(spending_data_combined) > 0) spending_data_combined else spending_data_30,
    visualizations = spending_viz,
    is_landing_page = TRUE
  )

# ============================================================================
# TARGETING PAGE
# ============================================================================

# Calculate targeting breakdown - only for top 20 advertisers
top_20_advertisers_30 <- if(nrow(spending_data_30) > 0) spending_data_30$advertiser else character()
top_20_advertisers_7 <- if(nrow(spending_data_7) > 0) spending_data_7$advertiser else character()

if(nrow(election_dat30) > 0 && "page_name" %in% names(election_dat30) && length(top_20_advertisers_30) > 0){
  col_each30 <- election_dat30 %>%
    filter(!is.na(page_name), page_name %in% top_20_advertisers_30) %>%
    mutate(cntry_advertiser = paste0(cntry, "XXXXXXX", page_name)) %>%
    pull(cntry_advertiser) %>%
    unique() %>%
    map_dfr(~{
      important <- str_split(.x, "XXXXXXX") %>% unlist()
      the_dat <- election_dat30 %>%
        filter(cntry == important[1]) %>%
        filter(page_name == important[2])
      
      # Filter by main currency if available
      # Filter by main currency if available
      if(!is.null(main_currency_code) && "main_currency" %in% names(the_dat) && nrow(the_dat) > 0) {
        the_dat <- the_dat %>% filter(main_currency == main_currency_code)
      }
      
      if(nrow(the_dat) > 0){
        tryCatch({
          result <- calc_targeting(the_dat)
          if(nrow(result) > 0 && "target" %in% names(result)){
            result %>%
              mutate(
                advertiser = the_dat$page_name[1],
                internal_id = the_dat$internal_id[1],
                cntry = the_dat$cntry[1]
              ) %>%
              filter(target != "Gender: All", target != "Unknown")
          } else {
            tibble()
          }
        }, error = function(e) tibble())
      } else {
        tibble()
      }
    }, .progress = TRUE) %>%
    {if("target" %in% names(.)) filter(., target != "Gender: All", target != "Unknown") else .}
} else {
  col_each30 <- tibble()
}

if(nrow(election_dat7) > 0 && "page_name" %in% names(election_dat7) && length(top_20_advertisers_7) > 0){
  col_each7 <- election_dat7 %>%
    filter(!is.na(page_name), page_name %in% top_20_advertisers_7) %>%
    mutate(cntry_advertiser = paste0(cntry, "XXXXXXX", page_name)) %>%
    pull(cntry_advertiser) %>%
    unique() %>%
    map_dfr(~{
      important <- str_split(.x, "XXXXXXX") %>% unlist()
      the_dat <- election_dat7 %>%
        filter(cntry == important[1]) %>%
        filter(page_name == important[2])
      
      if(nrow(the_dat) > 0){
        tryCatch({
          result <- calc_targeting(the_dat)
          if(nrow(result) > 0 && "target" %in% names(result)){
            result %>%
              mutate(
                advertiser = the_dat$page_name[1],
                internal_id = the_dat$internal_id[1],
                cntry = the_dat$cntry[1]
              ) %>%
              filter(target != "Gender: All", target != "Unknown")
          } else {
            tibble()
          }
        }, error = function(e) tibble())
      } else {
        tibble()
      }
    }, .progress = TRUE) %>%
    {if("target" %in% names(.)) filter(., target != "Gender: All", target != "Unknown") else .}
} else {
  col_each7 <- tibble()
}

# Create targeting visualizations with nice colors
targeting_colors <- RColorBrewer::brewer.pal(8, "Set2")
targeting_viz <- create_viz(
  type = "bar",
  horizontal = TRUE,
  bar_type = "percent",
  color_palette = targeting_colors
)

# Initialize targeting_summary variables
targeting_summary_30 <- tibble()
targeting_summary_7 <- tibble()

if(nrow(col_each30) > 0){
  all_spend_30 <- col_each30 %>%
    distinct(internal_id, .keep_all = TRUE) %>%
    summarize(
      total = sum(total, na.rm = TRUE),
      total_ads = sum(total_ads, na.rm = TRUE)
    )
  
  if(all_spend_30$total > 0){
    targeting_summary_30 <- col_each30 %>%
      filter(target != "countries") %>%
      mutate(target = case_when(
        target == "custom_audience" ~ "Custom Audiences",
        target == "countries" ~ "GEOGRAPHY: Entire Country",
        target == "regions" ~ "GEOGRAPHY: Regions",
        target == "lookalike_audience" ~ "Lookalike Audiences",
        target == "interest" ~ "Detailed",
        target == "age" ~ "Age",
        target == "zips" ~ "GEOGRAPHY: Postal Code",
        target == "CITY" ~ "GEOGRAPHY: City",
        target == "language" ~ "Language",
        target == "gender" ~ "Gender",
        target == "COMUNE" ~ "GEOGRAPHY: Municipality",
        target == "electoral_districts" ~ "GEOGRAPHY: Electoral Districts",
        target == "COUNTY" ~ "GEOGRAPHY: Counties",
        str_detect(target, "NEIGHBOR") ~ "GEOGRAPHY: Neighborhood",
        TRUE ~ target
      )) %>%
      filter(target != "Unknown") %>%
      group_by(target) %>%
      summarize(spend_per = sum(spend_per, na.rm = TRUE)) %>%
      mutate(perc = spend_per / all_spend_30$total * 100) %>%
      arrange(desc(perc)) %>%
      filter(perc >= 0.01) %>%
      # Convert to factor with proper ordering - highest at TOP for horizontal bars
      mutate(target = fct_reorder(target, perc, .desc = TRUE))  # Top = highest
    
    if(nrow(targeting_summary_30) > 0){
      # DEBUG: Print targeting data
      cat("\n=== TARGETING DATA (30-day) DEBUG ===\n")
      cat("Rows:", nrow(targeting_summary_30), "\n")
      cat("Total spend across all targeting methods:", all_spend_30$total, "\n")
      cat("Sample calculation check:\n")
      sample_targeting <- col_each30 %>%
        filter(target == first(targeting_summary_30$target)) %>%
        head(3) %>%
        select(advertiser, target, spend_per, total)
      if(nrow(sample_targeting) > 0) print(sample_targeting)
      cat("\nTop 10 targeting methods:\n")
      print(head(targeting_summary_30, 10))
      cat("Sum of percentages:", sum(targeting_summary_30$perc, na.rm = TRUE), "%\n")
      
      targeting_viz <- targeting_viz %>%
        add_viz(
          title = paste("Spending per Targeting Method -", last30days_string),
          x_var = "target",
          weight_var = "perc",
          y_label = "Budget spent on targeting method (% of total spend)",
          tabgroup = "targeting/30days"
        )
    }
  }
}

if(nrow(col_each7) > 0){
  all_spend_7 <- col_each7 %>%
    distinct(internal_id, .keep_all = TRUE) %>%
    summarize(
      total = sum(total, na.rm = TRUE),
      total_ads = sum(total_ads, na.rm = TRUE)
    )
  
  if(all_spend_7$total > 0){
      targeting_summary_7 <- col_each7 %>%
      filter(target != "countries") %>%
      mutate(target = case_when(
        target == "custom_audience" ~ "Custom Audiences",
        target == "countries" ~ "GEOGRAPHY: Entire Country",
        target == "regions" ~ "GEOGRAPHY: Regions",
        target == "lookalike_audience" ~ "Lookalike Audiences",
        target == "interest" ~ "Detailed",
        target == "age" ~ "Age",
        target == "zips" ~ "GEOGRAPHY: Postal Code",
        target == "CITY" ~ "GEOGRAPHY: City",
        target == "language" ~ "Language",
        target == "gender" ~ "Gender",
        target == "COMUNE" ~ "GEOGRAPHY: Municipality",
        target == "electoral_districts" ~ "GEOGRAPHY: Electoral Districts",
        target == "COUNTY" ~ "GEOGRAPHY: Counties",
        str_detect(target, "NEIGHBOR") ~ "GEOGRAPHY: Neighborhood",
        TRUE ~ target
      )) %>%
      filter(target != "Unknown") %>%
      group_by(target) %>%
      summarize(spend_per = sum(spend_per, na.rm = TRUE)) %>%
      mutate(perc = spend_per / all_spend_7$total * 100) %>%
      arrange(desc(perc)) %>%
      filter(perc >= 0.01) %>%
      # Convert to factor with proper ordering - highest at TOP for horizontal bars
      mutate(target = fct_reorder(target, perc, .desc = TRUE))  # Top = highest
    
    if(nrow(targeting_summary_7) > 0){
      # DEBUG: Print targeting data
      cat("\n=== TARGETING DATA (7-day) DEBUG ===\n")
      cat("Rows:", nrow(targeting_summary_7), "\n")
      cat("Total spend across all targeting methods:", all_spend_7$total, "\n")
      cat("\nTop 10 targeting methods:\n")
      print(head(targeting_summary_7, 10))
      cat("Sum of percentages:", sum(targeting_summary_7$perc, na.rm = TRUE), "%\n")
      
      targeting_viz <- targeting_viz %>%
        add_viz(
          title = paste("Spending per Targeting Method -", last7days_string),
          x_var = "target",
          weight_var = "perc",
          y_label = "Budget spent on targeting method (% of total spend)",
          tabgroup = "targeting/7days"
        )
    }
  }
}

# Set tabgroup labels on targeting visualization
if(exists("targeting_summary_7") && nrow(targeting_summary_7) > 0){
  targeting_viz <- targeting_viz %>%
    set_tabgroup_labels(
      targeting = "Targeting Methods",
      `30days` = last30days_string,
      `7days` = last7days_string
    )
} else {
  targeting_viz <- targeting_viz %>%
    set_tabgroup_labels(
      targeting = "Targeting Methods",
      `30days` = last30days_string
    )
}

# Combine targeting data for the page
targeting_data_combined <- bind_rows(
  if(nrow(targeting_summary_30) > 0) targeting_summary_30 %>% mutate(timeframe = "30days") else tibble(),
  if(exists("targeting_summary_7") && nrow(targeting_summary_7) > 0) targeting_summary_7 %>% mutate(timeframe = "7days") else tibble()
)

# Add targeting page
dashboard <- dashboard %>%
  add_page(
    "Targeting",
    text = md_text(paste0(
      "# Spending per Targeting\n\n",
      "These charts show the groups that advertisers target with their advertisements, ",
      "according to the latest data. The focus is on Meta's platforms (Facebook and Instagram) ",
      "because they offer the most detailed targeting options.\n\n",
      "> Note: Different targeting criteria may overlap, so the outcome may not necessarily add up to 100%.\n\n"
    )),
    data = if(nrow(targeting_data_combined) > 0) targeting_data_combined else if(nrow(targeting_summary_30) > 0) targeting_summary_30 else tibble(),
    visualizations = targeting_viz
  )

# ============================================================================
# DEMOGRAPHICS PAGE
# ============================================================================

# Prepare demographics data (age/gender) - only top 20 advertisers, filter by currency
if(nrow(election_dat30) > 0 && "type" %in% names(election_dat30) && "page_name" %in% names(election_dat30) && length(top_20_advertisers_30) > 0){
  
  # Prepare age data
  age_data_30 <- election_dat30 %>%
    filter(type == "age", !is.na(value), !is.na(page_name), page_name %in% top_20_advertisers_30)
  
  # Filter by main currency if available
  if(!is.null(main_currency_code) && "main_currency" %in% names(age_data_30)) {
    age_data_30 <- age_data_30 %>%
      filter(main_currency == main_currency_code)
  }
  
  age_data_30 <- age_data_30 %>%
    mutate(
      total_spend_formatted = as.numeric(total_spend_formatted),
      total_spend_pct_num = as.numeric(total_spend_pct)
    ) %>%
    # Aggregate by advertiser and age value
    group_by(page_name, value) %>%
    summarize(
      total_spend = sum(total_spend_formatted * total_spend_pct_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(advertiser = page_name) %>%
    # Calculate total spend per age value for sorting
    group_by(value) %>%
    mutate(age_total = sum(total_spend, na.rm = TRUE)) %>%
    ungroup() %>%
    # Format age values as age ranges
    mutate(
      age_num = suppressWarnings(as.numeric(value)),
      value = case_when(
        !is.na(age_num) & age_num %in% 13:17 ~ "13-17",
        !is.na(age_num) & age_num %in% 18:24 ~ "18-24",
        !is.na(age_num) & age_num %in% 25:34 ~ "25-34",
        !is.na(age_num) & age_num %in% 35:44 ~ "35-44",
        !is.na(age_num) & age_num %in% 45:54 ~ "45-54",
        !is.na(age_num) & age_num %in% 55:64 ~ "55-64",
        !is.na(age_num) & age_num >= 65 ~ "65+",
        TRUE ~ as.character(value)
      )
    ) %>%
    # Convert to factors with proper ordering - highest at TOP for horizontal bars
    mutate(
      value = fct_reorder(value, age_total, .desc = TRUE),  # Top = highest
      advertiser = fct_reorder(advertiser, total_spend, .desc = TRUE)  # Top spenders first
    ) %>%
    # Sort by age total (most targeted ages first), then by advertiser spend
    arrange(desc(age_total), desc(total_spend)) %>%
    select(-age_num)  # Remove helper column
  
  # Prepare gender data
  gender_data_30 <- election_dat30 %>%
    filter(type == "gender", !is.na(value), !is.na(page_name), value != "All", page_name %in% top_20_advertisers_30)
  
  # Filter by main currency if available
  if(!is.null(main_currency_code) && "main_currency" %in% names(gender_data_30)) {
    gender_data_30 <- gender_data_30 %>%
      filter(main_currency == main_currency_code)
  }
  
  gender_data_30 <- gender_data_30 %>%
    mutate(
      total_spend_formatted = as.numeric(total_spend_formatted),
      total_spend_pct_num = as.numeric(total_spend_pct)
    ) %>%
    # Aggregate by advertiser and gender value
    group_by(page_name, value) %>%
    summarize(
      total_spend = sum(total_spend_formatted * total_spend_pct_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(advertiser = page_name) %>%
    # Calculate total spend per gender value for sorting
    group_by(value) %>%
    mutate(gender_total = sum(total_spend, na.rm = TRUE)) %>%
    ungroup() %>%
    # Convert to factors with proper ordering - highest at TOP for horizontal bars
    mutate(
      value = fct_reorder(value, gender_total, .desc = TRUE),  # Top = highest
      advertiser = fct_reorder(advertiser, total_spend, .desc = TRUE)  # Top spenders first
    ) %>%
    # Sort by gender total (most targeted genders first), then by advertiser spend
    arrange(desc(gender_total), desc(total_spend))
  
  # DEBUG: Print demographics data
  cat("\n=== DEMOGRAPHICS DATA DEBUG ===\n")
  cat("Age data rows:", nrow(age_data_30), "\n")
  if(nrow(age_data_30) > 0) {
    cat("Top 10 age groups by spend:\n")
    print(head(age_data_30, 10))
    cat("\nUnique age groups:", length(unique(age_data_30$value)), "\n")
  }
  cat("Gender data rows:", nrow(gender_data_30), "\n")
  if(nrow(gender_data_30) > 0) {
    cat("Top 10 gender targeting by spend:\n")
    print(head(gender_data_30, 10))
    cat("\nUnique genders:", length(unique(gender_data_30$value)), "\n")
  }
  
  # Create demographics visualizations with better colors
  n_advertisers_demo <- max(
    if(nrow(age_data_30) > 0) length(unique(age_data_30$advertiser)) else 0,
    if(nrow(gender_data_30) > 0) length(unique(gender_data_30$advertiser)) else 0
  )
  # Use a more vibrant, professional color palette
  demo_colors <- RColorBrewer::brewer.pal(min(11, max(3, n_advertisers_demo)), "Spectral")
  if(length(demo_colors) < n_advertisers_demo){
    demo_colors <- rep(demo_colors, length.out = n_advertisers_demo)
  }
  
  demographics_viz <- create_viz(
    type = "bar",
    horizontal = TRUE,
    bar_type = "count",
    color_palette = demo_colors
  )
  
  if(nrow(age_data_30) > 0){
    demographics_viz <- demographics_viz %>%
      add_viz(
        title = paste("Age Targeting -", last30days_string),
        x_var = "value",
        group_var = "advertiser",
        weight_var = "total_spend",
        y_label = "Total Spend",
        tabgroup = "demographics/age"
      )
  }
  
  if(nrow(gender_data_30) > 0){
    demographics_viz <- demographics_viz %>%
      add_viz(
        title = paste("Gender Targeting -", last30days_string),
        x_var = "value",
        group_var = "advertiser",
        weight_var = "total_spend",
        y_label = "Total Spend",
        tabgroup = "demographics/gender"
      )
  }
  
  # Set tabgroup labels
  if(nrow(age_data_30) > 0 && nrow(gender_data_30) > 0){
    demographics_viz <- demographics_viz %>%
      set_tabgroup_labels(
        demographics = "Demographics",
        age = "Age Targeting",
        gender = "Gender Targeting"
      )
  } else if(nrow(age_data_30) > 0){
    demographics_viz <- demographics_viz %>%
      set_tabgroup_labels(
        demographics = "Demographics",
        age = "Age Targeting"
      )
  } else if(nrow(gender_data_30) > 0){
    demographics_viz <- demographics_viz %>%
      set_tabgroup_labels(
        demographics = "Demographics",
        gender = "Gender Targeting"
      )
  }
  
  # Combine demographics data for the page
  demographics_data_combined <- bind_rows(
    if(nrow(age_data_30) > 0) age_data_30 %>% mutate(demographic_type = "age") else tibble(),
    if(nrow(gender_data_30) > 0) gender_data_30 %>% mutate(demographic_type = "gender") else tibble()
  )
  
  if(nrow(demographics_data_combined) > 0 || nrow(age_data_30) > 0 || nrow(gender_data_30) > 0){
    dashboard <- dashboard %>%
      add_page(
        "Demographics",
        text = md_text(paste0(
          "# Demographics\n\n",
          "Demographic targeting breakdown by advertiser, sorted by top spenders per demographic category.\n\n"
        )),
        data = if(nrow(demographics_data_combined) > 0) demographics_data_combined else if(nrow(age_data_30) > 0) age_data_30 else gender_data_30,
        visualizations = demographics_viz
      )
  }
}

# ============================================================================
# LOCATION PAGE
# ============================================================================

# Prepare location data - only top 20 advertisers, filter by currency
if(nrow(election_dat30) > 0 && "type" %in% names(election_dat30) && "page_name" %in% names(election_dat30) && length(top_20_advertisers_30) > 0){
  location_data_30 <- election_dat30 %>%
    filter(type == "location", !is.na(value), !is.na(page_name), page_name %in% top_20_advertisers_30)
  
  # Filter by main currency if available
  if(!is.null(main_currency_code) && "main_currency" %in% names(location_data_30)) {
    location_data_30 <- location_data_30 %>%
      filter(main_currency == main_currency_code)
  }
  
  location_data_30 <- location_data_30 %>%
    mutate(
      total_spend_formatted = as.numeric(total_spend_formatted),
      total_spend_pct_num = as.numeric(total_spend_pct)
    ) %>%
    # Group by advertiser and location value only (not location_type)
    group_by(page_name, value) %>%
    summarize(
      total_spend = sum(total_spend_formatted * total_spend_pct_num, na.rm = TRUE),
      location_type = first(location_type),  # Keep as metadata
      .groups = "drop"
    ) %>%
    mutate(advertiser = page_name) %>%
    # Calculate total spend per location value for sorting
    group_by(value) %>%
    mutate(location_total = sum(total_spend, na.rm = TRUE)) %>%
    ungroup() %>%
    # Convert to factors with proper ordering - highest at TOP for horizontal bars
    mutate(
      value = fct_reorder(value, location_total, .desc = TRUE),  # Top = highest
      advertiser = fct_reorder(advertiser, total_spend, .desc = TRUE)  # Top spenders first
    ) %>%
    # Sort by location total (most targeted locations first), then by advertiser spend
    arrange(desc(location_total), desc(total_spend))
  
  # DEBUG: Print location data
  cat("\n=== LOCATION DATA DEBUG ===\n")
  cat("Rows:", nrow(location_data_30), "\n")
  if(nrow(location_data_30) > 0) {
    cat("Sample calculation check (first location):\n")
    sample_location <- election_dat30 %>%
      filter(type == "location", page_name == first(location_data_30$advertiser), 
             value == first(location_data_30$value)) %>%
      head(3) %>%
      select(page_name, value, total_spend_formatted, total_spend_pct) %>%
      mutate(
        total_spend_num = as.numeric(total_spend_formatted),
        pct_num = as.numeric(total_spend_pct),
        calculated_spend = total_spend_num * pct_num
      )
    print(sample_location)
    cat("\nTop 10 locations by spend:\n")
    print(head(location_data_30, 10))
    cat("\nUnique locations:", length(unique(location_data_30$value)), "\n")
    cat("Unique advertisers:", length(unique(location_data_30$advertiser)), "\n")
    cat("Total spend on location targeting:", sum(location_data_30$total_spend, na.rm = TRUE), "\n")
  }
  
  if(nrow(location_data_30) > 0){
    # Get unique advertisers for color palette - use better colors
    n_advertisers <- length(unique(location_data_30$advertiser))
    location_colors <- RColorBrewer::brewer.pal(min(11, max(3, n_advertisers)), "Spectral")
    if(length(location_colors) < n_advertisers){
      location_colors <- rep(location_colors, length.out = n_advertisers)
    }
    
    location_viz <- create_viz(
      type = "bar",
      horizontal = TRUE,
      bar_type = "count",
      color_palette = location_colors
    ) %>%
      add_viz(
        title = paste("Location Targeting -", last30days_string),
        x_var = "value",
        group_var = "advertiser",
        weight_var = "total_spend",
        y_label = "Total Spend",
        tabgroup = "location/30days"
      ) %>%
      set_tabgroup_labels(
        location = "Location Targeting",
        `30days` = last30days_string
      )
    
    dashboard <- dashboard %>%
      add_page(
        "Location",
        text = md_text(paste0(
          "# Location Targeting\n\n",
          "Geographic targeting by advertiser, sorted by top spenders per location.\n\n"
        )),
        data = location_data_30,
        visualizations = location_viz
      )
  }
}

# ============================================================================
# DETAILED PAGE (Interest-based targeting)
# ============================================================================

# Prepare detailed targeting data - only top 20 advertisers, filter by currency
if(nrow(election_dat30) > 0 && "type" %in% names(election_dat30) && "page_name" %in% names(election_dat30) && length(top_20_advertisers_30) > 0){
  detailed_data_30 <- election_dat30 %>%
    filter(type == "detailed", !is.na(value), !is.na(page_name), page_name %in% top_20_advertisers_30)
  
  # Filter by main currency if available
  if(!is.null(main_currency_code) && "main_currency" %in% names(detailed_data_30)) {
    detailed_data_30 <- detailed_data_30 %>%
      filter(main_currency == main_currency_code)
  }
  
  detailed_data_30 <- detailed_data_30 %>%
    mutate(
      total_spend_formatted = as.numeric(total_spend_formatted),
      total_spend_pct_num = as.numeric(total_spend_pct)
    ) %>%
    # Aggregate by advertiser and interest value (sum all spend for same advertiser+value)
    group_by(page_name, value) %>%
    summarize(
      total_spend = sum(total_spend_formatted * total_spend_pct_num, na.rm = TRUE),
      detailed_type = first(detailed_type),  # Take first type if multiple
      .groups = "drop"
    ) %>%
    mutate(advertiser = page_name) %>%
    # Calculate total spend per interest value across all advertisers
    group_by(value) %>%
    mutate(interest_total = sum(total_spend, na.rm = TRUE)) %>%
    ungroup()
  
  # Get top 50 interest values by total spend
  top_interests <- detailed_data_30 %>%
    group_by(value) %>%
    summarize(interest_total = first(interest_total), .groups = "drop") %>%
    arrange(desc(interest_total)) %>%
    slice_head(n = 50) %>%
    pull(value)
  
  # Filter to top interests and show all advertisers for those interests, sorted properly
  detailed_data_30 <- detailed_data_30 %>%
    filter(value %in% top_interests) %>%
    # Recalculate interest_total after filtering
    group_by(value) %>%
    mutate(interest_total = sum(total_spend, na.rm = TRUE)) %>%
    ungroup() %>%
    # Convert to factors with proper ordering - highest at TOP for horizontal bars
    mutate(
      value = fct_reorder(value, interest_total, .desc = TRUE),  # Top = highest
      advertiser = fct_reorder(advertiser, total_spend, .desc = TRUE)  # Top spenders first
    ) %>%
    # Sort by interest total (most targeted interests first), then within each interest by advertiser spend
    arrange(desc(interest_total), desc(total_spend))
  
  # DEBUG: Print detailed data
  cat("\n=== DETAILED TARGETING DATA DEBUG ===\n")
  cat("Rows:", nrow(detailed_data_30), "\n")
  if(nrow(detailed_data_30) > 0) {
    cat("Sample calculation check (first interest):\n")
    sample_interest <- election_dat30 %>%
      filter(type == "detailed", page_name == first(detailed_data_30$advertiser), 
             value == first(detailed_data_30$value)) %>%
      head(3) %>%
      select(page_name, value, total_spend_formatted, total_spend_pct) %>%
      mutate(
        total_spend_num = as.numeric(total_spend_formatted),
        pct_num = as.numeric(total_spend_pct),
        calculated_spend = total_spend_num * pct_num
      )
    print(sample_interest)
    cat("\nTop 10 interests by spend:\n")
    print(head(detailed_data_30, 10))
    cat("\nUnique interests:", length(unique(detailed_data_30$value)), "\n")
    cat("Unique advertisers:", length(unique(detailed_data_30$advertiser)), "\n")
    cat("Total spend on interest targeting:", sum(detailed_data_30$total_spend, na.rm = TRUE), "\n")
  }
  
  if(nrow(detailed_data_30) > 0){
    # Get unique advertisers for color palette - use better colors
    n_advertisers <- length(unique(detailed_data_30$advertiser))
    detailed_colors <- RColorBrewer::brewer.pal(min(11, max(3, n_advertisers)), "Spectral")
    if(length(detailed_colors) < n_advertisers){
      detailed_colors <- rep(detailed_colors, length.out = n_advertisers)
    }
    
    detailed_viz <- create_viz(
      type = "bar",
      horizontal = TRUE,
      bar_type = "count",
      color_palette = detailed_colors
    ) %>%
      add_viz(
        title = paste("Interest-based Targeting -", last30days_string),
        x_var = "value",
        group_var = "advertiser",
        weight_var = "total_spend",
        y_label = "Total Spend",
        tabgroup = "detailed/30days"
      ) %>%
      set_tabgroup_labels(
        detailed = "Interest-based Targeting",
        `30days` = last30days_string
      )
    
    dashboard <- dashboard %>%
      add_page(
        "Detailed",
        text = md_text(paste0(
          "# Detailed Targeting\n\n",
          "Interest-based targeting by advertiser, sorted by top spenders per interest.\n\n"
        )),
        data = detailed_data_30,
        visualizations = detailed_viz
      )
  }
}

# ============================================================================
# ABOUT PAGE
# ============================================================================

dashboard <- dashboard %>%
  add_page(
    "About",
    text = md_text(paste0(
      "# About\n\n",
      "This dashboard visualizes ad targeting data from Meta's Ad Library.\n\n",
      "## Data Source\n\n",
      "Data is retrieved using the `metatargetr` R package.\n\n",
      "## Methodology\n\n",
      "The dashboard shows which advertisers are targeting what audiences, ",
      "without relying on party identification data.\n\n"
    ))
  )

# Generate dashboard
cat("\n\nGenerating dashboard...\n\n")
generate_dashboard(dashboard, render = TRUE)

cat("\n\nDashboard generation complete!\n\n")
