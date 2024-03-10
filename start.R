
pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown, rvest)
# setwd("C:/Users/fabio/Dropbox/postdoc/microdashboards/wtm_iq/")
# setwd("..")
# getwd()

source("cntry.R")

sets <- jsonlite::fromJSON("settings.json")

full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) %>% 
  sample_n(n()) %>% 
  filter(iso2c == "NL")

# cntryy <- "NL"

for (cntryy in full_cntry_list$iso2c) {
  # print(cntryy)
  
  try({
    
    sets$the_country <- full_cntry_list$country[which(full_cntry_list$iso2c==cntryy)]
    sets$cntry <- cntryy
    
    jsonlite::write_json(sets, "settings.json",  simplifyVector = TRUE)
    
    # Sys.sleep(5)
    
    
    title_txt <- read_lines("_quarto.yml")
    title_txt[which(str_detect(title_txt, "title"))[1]] <- glue::glue("  title: \"Targeting Dashboard - {sets$the_country}\"")
    # title_txt[which(str_detect(title_txt, "output-dir"))[1]] <- glue::glue("  output-dir: ../docs/{sets$cntry}")  
    # Sys.sleep(1)
    write_lines(title_txt, "_site/_quarto.yml")
    
    
    # all_dat <- readRDS("data/all_dat.rds")
    # color_dat <- readRDS("data/color_dat.rds")
    
    color_dat <- tibble()
    
    if(read_lines("cntry.R") %>% length() > 5){
      election_dat30 <- readRDS("data/election_dat30.rds")  %>% 
        select(-contains("party")) %>%
        left_join(all_dat %>% select(page_id, party))
    }
    
    if(!exists("election_dat30")){
      out <- sets$cntry %>% 
        map(~{
          .x %>% 
            paste0(c("-last_30_days"))
        }) %>% 
        unlist() %>% 
        # keep(~str_detect(.x, tf)) %>% 
        # .[100:120] %>% 
        map_dfr_progress(~{
          the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_targeting/releases/expanded_assets/", .x))
          
          the_assets %>% httr::content() %>% 
            rvest::html_elements(".Box-row") %>% 
            rvest::html_text()  %>%
            tibble(raw = .)   %>%
            # Split the raw column into separate lines
            mutate(raw = strsplit(as.character(raw), "\n")) %>%
            # Extract the relevant lines for filename, file size, and timestamp
            transmute(
              filename = sapply(raw, function(x) trimws(x[3])),
              file_size = sapply(raw, function(x) trimws(x[6])),
              timestamp = sapply(raw, function(x) trimws(x[7]))
            ) %>% 
            filter(filename != "Source code") %>% 
            mutate(release = .x) %>% 
            mutate_all(as.character)
        })
      
      thosearethere <- out %>% 
        rename(tag = release,
               file_name = filename)  %>% 
        arrange(desc(tag)) %>% 
        separate(tag, into = c("cntry", "tframe"), remove = F, sep = "-") %>%
        filter(cntry == sets$cntry) %>% 
        mutate(ds  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet")) %>% 
        distinct(cntry, ds, tframe) %>% 
        drop_na(ds) %>% 
        arrange(desc(ds))
      
      print(thosearethere)
      
      if(is.na(thosearethere$ds[1])){
        print("go next")
        next
      } 
      # try({
      election_dat30 <- arrow::read_parquet(paste0("https://github.com/favstats/meta_ad_targeting/releases/download/", sets$cntry, "-last_", 30,"_days/", thosearethere$ds[1], ".parquet"))
      # })
      
    }
    
    
    raw <- election_dat30 %>%
      rename(internal_id = contains("page_id"))# %>%
      # filter(is.na(no_data)) %>% 
      # filter(sources == "wtm")
    
    if(nrow(raw)==0){
      
      if(read_lines("cntry.R") %>% length() > 5){
        election_dat30 <- election_dat30 %>%
          rename(internal_id = contains("page_id")) %>%
          # filter(is.na(no_data)) %>% 
          drop_na(party) #%>% 
          # filter(party %in% color_dat$party) 
      } else {
        election_dat30 <- tibble()
      }
      
      
    } else {
      election_dat30 <- raw %>% 
        drop_na(party) #%>% 
        # filter(party %in% color_dat$party) 
    }
    
    
    
    # rstudioapi::jobRunScript("fbadlibrary.R")
    try({
      
      
      
      
      if(nrow(election_dat30)!=0){
        
        # Sys.sleep(60*7)
        # all_dat <- readRDS("data/all_dat.rds")
        
        write_lines(nrow(distinct(election_dat30, internal_id)), file = "n_advertisers.txt")
        render_it <- possibly(quarto::quarto_render, otherwise = NULL, quiet = F)
        dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(render_it)
        
        dir("docs", full.names = T) %>% 
          keep(~str_detect(.x, "site|files")) %>% 
          walk(~fs::dir_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{sets$cntry}/")), overwrite = T))
        
        dir("docs", full.names = T) %>% 
          keep(~str_detect(.x, "html|json|logo")) %>% 
          walk(~fs::file_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{sets$cntry}/")), overwrite = T))
        
        knitr::knit("README.Rmd")
        
        rmarkdown::render("logs/overview.Rmd")
        
        file.copy(from = "logs/overview.html", to = glue::glue("docs/{sets$cntry}/overview.html"), overwrite = T)
        
        unlink("node_modules", recursive = T, force = T)
        unlink("out", recursive = T, force = T)
        
        dir("docs", full.names = T) %>% 
          keep(~str_detect(.x, "site|files")) %>% 
          walk(fs::dir_delete)
        
        dir("docs", full.names = T) %>% 
          keep(~str_detect(.x, "html|json")) %>% 
          walk(fs::file_delete)
        
      } else {
        
        rmarkdown::render("logs/index.Rmd")
        dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
        file.copy(from = "logs/index.Rmd", to = glue::glue("docs/{sets$cntry}/index.html"), overwrite = T, recursive = T)
        
        unlink("node_modules", recursive = T, force = T)
        unlink("out", recursive = T, force = T)
        
      }
      
      
      
      
      
    })
    
  })
  
  dir("docs", full.names = T) %>% 
    keep(~str_detect(.x, "site|files")) %>% 
    walk(fs::dir_delete)
  
  dir("docs", full.names = T) %>% 
    keep(~str_detect(.x, "html|json")) %>% 
    walk(fs::file_delete)
  
  # file.remove("_site/_quarto.yml")
  
  
}
