
pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown, rvest)
# setwd("C:/Users/fabio/Dropbox/postdoc/microdashboards/wtm_iq/")
# setwd("C:/Users/fabio/Dropbox/postdoc/backup/elex")
# getwd()
here::i_am("elex.Rproj")

# getwd()
# source("cntry.R")
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
  arrange(iso2c) #%>% 
  # filter(iso2c %in% c("NL", "MX", "DR", "US", "ZA"))

render_it <- function(...) {
  print(...)
  thefile <- str_remove(..., "_site/") %>% str_replace("qmd", "html")
  if(any(str_detect(..., "map"))){
    if(cntryy == "NL"){
      quarto::quarto_render(..., quiet = T)
    }
  } else {
    # print(thefile)
    quarto::quarto_render(..., quiet = T)
  }
  
}
render_it <- possibly(render_it, otherwise = NULL, quiet = F)


# cntryy <- "NL"
for (cntryy in full_cntry_list$iso2c) {
  # print(cntryy)
  
  t2 <- Sys.time()
  
  print(paste0(cntryy,": ", t2))
  
  
  try({
    
    
    time_difference_seconds <- difftime(t2, t1, units = "hours")

    
    if (as.numeric(time_difference_seconds) > 4) {
      if (Sys.info()[["sysname"]] != "Windows") {
        break
      }
    }
    
 
    
    sets$the_country <- full_cntry_list$country[which(full_cntry_list$iso2c==cntryy)]
    sets$cntry <- cntryy
    
    jsonlite::write_json(sets, "settings.json",  simplifyVector = TRUE)
    
    # Sys.sleep(5)
    
    if(cntryy != "NL"){
      
    title_txt <- read_lines("_quarto.yml")
    # title_txt[which(str_detect(title_txt, "title"))[1]] <- glue::glue("  title: \"Targeting Dashboard - {sets$the_country}\"")
    # title_txt[which(str_detect(title_txt, "output-dir"))[1]] <- glue::glue("  output-dir: ../docs/{sets$cntry}")  
    # Sys.sleep(1)
    write_lines(title_txt, "_site/_quarto.yml"
                )
    }
    
    
    # all_dat <- readRDS("data/all_dat.rds")
    # color_dat <- readRDS("data/color_dat.rds")
    
    # color_dat <- tibble()
    
    # if(read_lines("cntry.R") %>% length() > 5){
    #   election_dat30 <- readRDS("data/election_dat30.rds")  %>% 
    #     select(-contains("party")) %>%
    #     left_join(all_dat %>% select(page_id, party))
    # }
    
 
        # Sys.sleep(60*7)
        # all_dat <- readRDS("data/all_dat.rds")
        
        # render_it <- possibly(quarto::quarto_render, otherwise = NULL, quiet = F)
    dir("_site", full.names = T) %>%
      keep(~str_detect(.x, "qmd")) %>%
      # discard( ~ str_detect(.x, "map")) %>%
      fct_relevel("_site/map.qmd") %>%
      sort() %>%
      as.character() %>%
      # .[2] %>% 
      purrr::walk(render_it, .progress = T)
        
        dir("docs", full.names = T) %>% 
          keep(~str_detect(.x, "site|files")) %>% 
          walk(~fs::dir_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{sets$cntry}/")), overwrite = T))
        
        dir("docs", full.names = T) %>% 
          keep(~str_detect(.x, "html|json|logo")) %>% 
          walk(~fs::file_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{sets$cntry}/")), overwrite = T))
        
        # knitr::knit("README.Rmd")
        
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
        
      
      
      
      
      
      
    })
    
    dir("docs", full.names = T) %>% 
      keep(~str_detect(.x, "site|files")) %>% 
      walk(fs::dir_delete)
    
    dir("docs", full.names = T) %>% 
      keep(~str_detect(.x, "html|json")) %>% 
      walk(fs::file_delete)
    
  # })
  

  
  # file.remove("_site/_quarto.yml")
  
  # rm(election_dat30)
  
}


rmarkdown::render("index.Rmd")
# dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
file.copy(from = "index.html", to = glue::glue("docs/index.html"), overwrite = T, recursive = T)
dir(full.names = F) %>%
  keep(~str_detect(.x, "_libs")) %>%
  walk(~fs::dir_copy(.x, "docs/site_libs", overwrite = T))

file.copy(from = "logs/log.html", to = glue::glue("docs/log.html"), overwrite = T, recursive = T)

file.copy(from = "docs/NL/map.html", to = glue::glue("docs/map.html"), overwrite = T, recursive = T)

# fs::dir_copy("docs/NL/site_libs", "docs/site_libs", overwrite = T)

# setwd("C:/Users/favoo/Documents/ep2024")
full_cntry_list$iso2c %>%
  # .[1] %>% 
  walk_progress( ~ {
    try({
      
      city_name <- .x
      dir("docs/NL", full.names = T) %>%
        keep( ~ str_detect(.x, "map")) %>%
        walk( ~ fs::file_copy(.x, str_replace(
          .x, "docs/ <- /", glue::glue("docs/{city_name}/")
        ), overwrite = T))
      
      dir("docs/NL", full.names = T) %>%
        keep(~str_detect(.x, "site|files"))  %>%
        walk( ~ fs::dir_copy(.x, str_replace(
          .x, "docs/NL/", glue::glue("docs/{city_name}/")
        ), overwrite = T))
      
    })
  })

if (Sys.info()[["effective_user"]] == "fabio") {
  system("git pull")
  system("git add -A")
  system('git commit -m "update"')
  system("git push")
}
