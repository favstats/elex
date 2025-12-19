# Main workflow script
# Updated to use metatargetr and dashboardr

pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown, rvest, metatargetr, dashboardr)

here::i_am("elex.Rproj")

source("utils.R")

t1 <- Sys.time()

sets <- jsonlite::fromJSON("settings.json")

eu_countries <- c("NL", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                  "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                  "PL", "PT", "RO", "SE", "SI", "SK", "US", "MX", "NZ", 
                  "CA", "AU")

full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) %>% 
  sample_n(n()) %>% 
  mutate(iso2c = fct_relevel(iso2c, eu_countries)) %>% 
  arrange(iso2c)

the_cntries <- full_cntry_list$iso2c

the_cntries <- "CA"  # Test with Canada

# Main loop through countries
for (cntryy in the_cntries) {
  
  t2 <- Sys.time()
  
  print(paste0(cntryy, ": ", t2))
  
  try({
    
    time_difference_seconds <- difftime(t2, t1, units = "hours")
    
    if (as.numeric(time_difference_seconds) > 4) {
      break
    }
    
    # Update settings for current country
    sets$the_country <- full_cntry_list$country[which(the_cntries == cntryy)]
    sets$cntry <- cntryy
    
    jsonlite::write_json(sets, "settings.json", simplifyVector = TRUE)
    
    # Step 1: Get data using metatargetr (for both 30 and 7 day timeframes)
    cat("\n\n=== Retrieving data for", cntryy, "===\n\n")
    
    # Determine date (must be at least 3 days old per metatargetr requirement)
    target_date <- Sys.Date() - lubridate::days(3)
    target_date_str <- as.character(target_date)
    
    # Get 30-day data
    cat("Getting 30-day data...\n")
    try({
      
      election_dat30 <- get_targeting_db(
        the_cntry = cntryy,
        tf = "30",
        ds = target_date_str,
        remove_nas = TRUE,
        verbose = TRUE
      )
      
      if(nrow(election_dat30) > 0){
        if(!"internal_id" %in% names(election_dat30) && "page_id" %in% names(election_dat30)){
          election_dat30 <- election_dat30 %>% rename(internal_id = page_id)
        }
        if(!"page_id" %in% names(election_dat30) && "internal_id" %in% names(election_dat30)){
          election_dat30 <- election_dat30 %>% rename(page_id = internal_id)
        }
        
        saveRDS(election_dat30, "data/election_dat30.rds")
        cat("30-day data: ", nrow(election_dat30), " rows\n")
      }
    }, silent = FALSE)
    
    # Get 7-day data
    cat("Getting 7-day data...\n")
    try({
      election_dat7 <- get_targeting_db(
        the_cntry = cntryy,
        tf = "7",
        ds = target_date_str,
        remove_nas = TRUE,
        verbose = TRUE
      )
      
      if(nrow(election_dat7) > 0){
        if(!"internal_id" %in% names(election_dat7) && "page_id" %in% names(election_dat7)){
          election_dat7 <- election_dat7 %>% rename(internal_id = page_id)
        }
        if(!"page_id" %in% names(election_dat7) && "internal_id" %in% names(election_dat7)){
          election_dat7 <- election_dat7 %>% rename(page_id = internal_id)
        }
        
        saveRDS(election_dat7, "data/election_dat7.rds")
        cat("7-day data: ", nrow(election_dat7), " rows\n")
      }
    }, silent = FALSE)
    
    # Step 2: Process data using actor_utils
    cat("\n=== Processing data ===\n\n")
    source("actor_utils.R")
    
    # Step 3: Generate dashboard using dashboardr
    cat("\n=== Generating dashboard ===\n\n")
    source("create_dashboard.R")
    
    # Step 4: Organize output files
    cat("\n=== Organizing output files ===\n\n")
    
    # Create country-specific directory
    country_dir <- paste0("docs/", cntryy)
    if(!dir.exists(country_dir)){
      dir.create(country_dir, recursive = TRUE)
    }
    
    # Copy dashboard files to country directory
    if(dir.exists("docs")){
      # Copy HTML files
      dir("docs", full.names = TRUE, pattern = "\\.html$") %>%
        walk(~file.copy(.x, file.path(country_dir, basename(.x)), overwrite = TRUE))
      
      # Copy site_libs if it exists
      if(dir.exists("docs/site_libs")){
        if(dir.exists(file.path(country_dir, "site_libs"))){
          unlink(file.path(country_dir, "site_libs"), recursive = TRUE)
        }
        fs::dir_copy("docs/site_libs", file.path(country_dir, "site_libs"), overwrite = TRUE)
      }
    }
    
    # Clean up temporary files
    unlink("node_modules", recursive = TRUE, force = TRUE)
    unlink("out", recursive = TRUE, force = TRUE)
    
    cat("\n=== Completed", cntryy, "===\n\n")
    
  }, silent = FALSE)
  
}

# Generate index page
cat("\n=== Generating index page ===\n\n")
try({
  rmarkdown::render("index.Rmd")
  if(file.exists("index.html")){
    file.copy(from = "index.html", to = "docs/index.html", overwrite = TRUE)
  }
}, silent = TRUE)

# Copy log file if it exists
if(file.exists("logs/log.html")){
  file.copy(from = "logs/log.html", to = "docs/log.html", overwrite = TRUE)
}

# Git operations (if user is fabio)
# if (Sys.info()[["effective_user"]] == "fabio") {
#   try({
#     system("git pull")
#     system("git add -A")
#     system('git commit -m "update"')
#     system("git push")
#   }, silent = TRUE)
# }

cat("\n\nWorkflow complete!\n\n")
