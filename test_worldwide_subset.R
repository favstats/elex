# =================================================================
# Test: Worldwide Dashboard with Subset of Countries
# =================================================================
# Quick test with just 5 countries to verify the implementation
# =================================================================

library(dashboardr)
library(tidyverse)
library(arrow)
library(countrycode)
library(here)
library(glue)

OUTPUT_DIR <- here("worldwide_test")
DATA_DIR <- here("data/30")

# Test with just 5 countries
TEST_COUNTRIES <- c("US", "DE", "FR", "GB", "BR")

# ============================================================
# Helper: Load page info from GitHub releases
# ============================================================
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
    
    cols_to_select <- intersect(c("page_id", "page_name", "page_category"), names(info))
    if (length(cols_to_select) == 0) return(NULL)
    
    info %>%
      select(all_of(cols_to_select)) %>%
      mutate(page_id = as.character(page_id)) %>%
      distinct(page_id, .keep_all = TRUE)
  }, error = function(e) {
    cli::cli_alert_warning("Could not load page info for {iso2c}: {e$message}")
    NULL
  })
}

# ============================================================
# Helper: Process country data with page info
# ============================================================
process_country_data <- function(iso2c, dat) {
  page_info <- load_page_info(iso2c)
  if (is.null(page_info)) return(dat)
  
  if (!"page_id" %in% names(dat) && "internal_id" %in% names(dat)) {
    dat <- dat %>% mutate(page_id = as.character(internal_id))
  } else if ("page_id" %in% names(dat)) {
    dat <- dat %>% mutate(page_id = as.character(page_id))
  }
  
  dat %>%
    left_join(page_info, by = "page_id", suffix = c("", "_info")) %>%
    mutate(page_group = case_when(
      !is.na(page_category) & str_detect(page_category, "Party") ~ "Political Parties",
      !is.na(page_category) & str_detect(page_category, "Politician|Candidate") ~ "Politicians",
      !is.na(page_category) & str_detect(page_category, "Government") ~ "Government",
      TRUE ~ "Other"
    ))
}

# ============================================================
# 1. Build country summary for map (test subset only)
# ============================================================
cat("Loading country data for test subset...\n")

country_summary <- map_dfr(TEST_COUNTRIES, function(iso2c) {
  file_path <- file.path(DATA_DIR, paste0(iso2c, ".rds"))
  if (!file.exists(file_path)) {
    cat("  File not found:", file_path, "\n")
    return(NULL)
  }
  
  tryCatch({
    dat <- readRDS(file_path)
    if (nrow(dat) == 0) return(NULL)
    
    id_col <- if ("internal_id" %in% names(dat)) "internal_id" else "page_id"
    
    dat %>%
      distinct(across(all_of(id_col)), .keep_all = TRUE) %>%
      summarize(
        iso2c = iso2c,
        country = countrycode(iso2c, "iso2c", "country.name", warn = FALSE) %||% iso2c,
        total_spend = sum(as.numeric(total_spend_formatted), na.rm = TRUE),
        total_ads = sum(as.numeric(total_num_ads), na.rm = TRUE),
        n_advertisers = n_distinct(page_name, na.rm = TRUE)
      )
  }, error = function(e) {
    cat("  Error loading", iso2c, ":", e$message, "\n")
    NULL
  })
}) %>%
  filter(total_ads > 0) %>%
  arrange(desc(total_spend))

cat("\nTest countries loaded:", nrow(country_summary), "\n")
print(country_summary)

# ============================================================
# 2. Create main map dashboard
# ============================================================
cat("\nCreating main map dashboard...\n")

main_viz <- create_viz() %>%
  add_viz(
    type = "map",
    value_var = "total_spend",
    join_var = "iso2c",
    click_url_template = "{iso2c}/index.html",
    title = "Election Ad Spending by Country (TEST)",
    subtitle = "Click a country to see detailed targeting",
    color_palette = c("#f7fbff", "#2171b5", "#08306b"),
    tooltip_vars = c("country", "total_spend", "total_ads", "n_advertisers"),
    legend_title = "Total Spend"
  )

main_dashboard <- create_dashboard(
  title = "Who Targets Me - Worldwide (TEST)",
  output_dir = OUTPUT_DIR,
  allow_inside_pkg = TRUE
) %>%
  add_page(
    name = "World Map",
    data = country_summary,
    visualizations = main_viz,
    is_landing_page = TRUE,
    text = "# Worldwide Ad Targeting (TEST)\n\nClick on any country to explore."
  )

# ============================================================
# 3. Create country dashboards
# ============================================================
cat("\nCreating country dashboards...\n")

country_dashboards <- country_summary$iso2c %>%
  set_names() %>%
  map(~{
    iso2c <- .x
    country_name <- country_summary$country[country_summary$iso2c == iso2c]
    
    cat("  Processing", iso2c, "-", country_name, "\n")
    
    tryCatch({
      dat <- readRDS(file.path(DATA_DIR, paste0(iso2c, ".rds")))
      dat <- process_country_data(iso2c, dat)
      
      spending_data <- dat %>%
        filter(!is.na(page_name)) %>%
        distinct(page_name, .keep_all = TRUE) %>%
        mutate(total_spend = as.numeric(total_spend_formatted)) %>%
        filter(!is.na(total_spend), total_spend > 0) %>%
        arrange(desc(total_spend)) %>%
        slice_head(n = 20) %>%
        mutate(advertiser = fct_reorder(page_name, total_spend))
      
      if (nrow(spending_data) == 0) return(NULL)
      
      # Check page_group distribution
      if ("page_group" %in% names(spending_data)) {
        cat("    Page groups:", paste(unique(spending_data$page_group), collapse = ", "), "\n")
      }
      
      if ("page_group" %in% names(spending_data) && 
          n_distinct(spending_data$page_group) > 1) {
        viz <- create_viz(type = "bar", horizontal = TRUE) %>%
          add_viz(
            x_var = "advertiser",
            weight_var = "total_spend",
            filter = ~ page_group == "Political Parties",
            tabgroup = "Category/Parties",
            title = "Political Parties"
          ) %>%
          add_viz(
            x_var = "advertiser", 
            weight_var = "total_spend",
            filter = ~ page_group == "Politicians",
            tabgroup = "Category/Politicians",
            title = "Politicians"
          ) %>%
          add_viz(
            x_var = "advertiser",
            weight_var = "total_spend", 
            filter = ~ page_group == "Other",
            tabgroup = "Category/Other",
            title = "Other Advertisers"
          )
      } else {
        viz <- create_viz(type = "bar", horizontal = TRUE) %>%
          add_viz(
            x_var = "advertiser",
            weight_var = "total_spend",
            title = paste("Top Advertisers in", country_name)
          )
      }
      
      create_dashboard(
        title = paste("Ad Targeting:", country_name),
        output_dir = file.path(OUTPUT_DIR, iso2c),
        allow_inside_pkg = TRUE
      ) %>%
        add_page(
          name = "Spending",
          data = spending_data,
          visualizations = viz,
          is_landing_page = TRUE,
          text = paste0(
            "[← Back to World Map](../index.html#)\n\n",
            "# ", country_name, "\n\n",
            "Top 20 advertisers by spending."
          )
        )
    }, error = function(e) {
      cat("    Error:", e$message, "\n")
      NULL
    })
  }) %>%
  compact()

cat("\nCreated", length(country_dashboards), "country dashboards\n")

# ============================================================
# 4. Generate all dashboards
# ============================================================
cat("\nGenerating all dashboards...\n\n")

all_dashboards <- c(list(main = main_dashboard), country_dashboards)

results <- generate_dashboards(
  all_dashboards,
  render = TRUE,
  continue_on_error = TRUE,
  linked = TRUE,
  open = TRUE
)

# ============================================================
# Summary
# ============================================================
successful <- sum(sapply(results, function(r) isTRUE(r$success)))
cat("\n=== TEST Complete ===\n")
cat("Successfully generated:", successful, "/", length(results), "dashboards\n")
cat("Output directory:", OUTPUT_DIR, "\n")
