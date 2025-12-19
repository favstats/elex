# =====================================================================
# Worldwide Election Ad Targeting Dashboards (WORKING SINGLE SCRIPT)
# - One world map dashboard + one dashboard per country
# - Deduplicated spend for overlapping targeting clusters
# - Percentages are within-group budgets (actor_type/party) + overall %
# - Demographics: age, gender, language, education, employment, relationship
# - Targeting + excluded + contested audiences
# - Landing page includes summary stats + top spenders (as HTML cards)
# =====================================================================

suppressPackageStartupMessages({
  library(dashboardr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(glue)
  library(arrow)
  library(countrycode)
  library(httr)
  library(jsonlite)
  library(here)
  library(cli)
  library(forcats)
})

# -------------------------------
# CONFIG
# -------------------------------
OUTPUT_DIR <- here("worldwide")
DATA_DIR   <- here("data/30")

TEST_MODE        <- TRUE
TEST_COUNTRIES   <- c("NL")  # set NULL for all
TIMEFRAME_DAYS   <- 30
FORCE_REFRESH    <- FALSE
SAVE_REFRESHED   <- TRUE
USE_DOLLAR_SPEND <- TRUE

GITHUB_OWNER <- "favstats"
GITHUB_REPO  <- "meta_ad_targeting"

# -------------------------------
# Small helpers
# -------------------------------
`%||%` <- function(x, y) if (!is.null(x) && length(x) && !all(is.na(x))) x else y

has_fun <- function(pkg, fun) {
  isTRUE(requireNamespace(pkg, quietly = TRUE)) &&
    exists(fun, envir = asNamespace(pkg), inherits = FALSE)
}

fmt_currency <- function(x, currency = "$") {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return(paste0(currency, "0"))
  if (abs(x) >= 1e9) return(paste0(currency, round(x / 1e9, 1), "B"))
  if (abs(x) >= 1e6) return(paste0(currency, round(x / 1e6, 1), "M"))
  if (abs(x) >= 1e3) return(paste0(currency, round(x / 1e3, 1), "K"))
  paste0(currency, round(x, 0))
}

fmt_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("0")
  if (abs(x) >= 1e9) return(paste0(round(x / 1e9, 1), "B"))
  if (abs(x) >= 1e6) return(paste0(round(x / 1e6, 1), "M"))
  if (abs(x) >= 1e3) return(paste0(round(x / 1e3, 1), "K"))
  as.character(round(x, 0))
}

assert_cols <- function(dat, cols, ctx = "") {
  miss <- setdiff(cols, names(dat))
  if (length(miss)) {
    stop(glue("Missing required columns{if (nzchar(ctx)) paste0(' for ', ctx) else ''}: {paste(miss, collapse=', ')}"), call. = FALSE)
  }
  invisible(TRUE)
}

# -------------------------------
# GitHub release + asset discovery (API, stable)
# -------------------------------
gh_release_by_tag <- function(tag) {
  url <- glue("https://api.github.com/repos/{GITHUB_OWNER}/{GITHUB_REPO}/releases/tags/{tag}")
  resp <- httr::GET(url, httr::timeout(20))
  if (httr::status_code(resp) != 200) return(NULL)
  httr::content(resp, as = "text", encoding = "UTF-8") |> jsonlite::fromJSON()
}

extract_ds_from_filename <- function(filename) {
  # expects YYYY-MM-DD somewhere in the filename
  m <- str_match(filename, "(\\d{4}-\\d{2}-\\d{2})")
  if (is.na(m[1,2])) return(NA_character_)
  m[1,2]
}

get_latest_country_asset <- function(iso2c, timeframe_days = 30) {
  tag <- glue("{iso2c}-last_{timeframe_days}_days")
  rel <- gh_release_by_tag(tag)
  if (is.null(rel) || is.null(rel$assets) || nrow(rel$assets) == 0) return(NULL)
  
  assets <- rel$assets %>%
    as_tibble() %>%
    dplyr::filter(str_detect(name, "\\.parquet$")) %>%
    mutate(ds = extract_ds_from_filename(name)) %>%
    dplyr::filter(!is.na(ds)) %>%
    arrange(desc(ds))
  
  if (nrow(assets) == 0) return(NULL)
  
  list(
    iso2c = iso2c,
    tag = tag,
    ds = assets$ds[1],
    url = assets$browser_download_url[1],
    filename = assets$name[1]
  )
}

get_available_countries <- function(countries = NULL, timeframe_days = 30) {
  cn <- countries %||%
    countrycode::codelist %>%
    dplyr::filter(!is.na(iso2c)) %>%
    distinct(iso2c) %>%
    pull(iso2c)
  
  cli_h2("Checking GitHub releases (API)")
  out <- map(cn, ~{
    a <- tryCatch(get_latest_country_asset(.x, timeframe_days), error = function(e) NULL)
    a
  }) %>% compact()
  
  if (!length(out)) return(NULL)
  
  bind_rows(lapply(out, as_tibble)) %>%
    arrange(desc(ds))
}

# -------------------------------
# Currency conversion rates (USD base)
# -------------------------------
get_conversion_rates <- function(date = Sys.Date()) {
  date <- as.Date(date)
  for (k in 0:5) {
    d <- date - k
    url <- glue("https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@{d}/v1/currencies/usd.json")
    resp <- tryCatch(httr::GET(url, httr::timeout(15)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) next
    
    js <- httr::content(resp, as = "text", encoding = "UTF-8") |> jsonlite::fromJSON()
    tibble(main_currency = toupper(names(js$usd)), conversion_rate = unname(js$usd)) %>%
      dplyr::filter(nchar(main_currency) == 3) %>%
      distinct(main_currency, .keep_all = TRUE)
  }
  NULL
}

# -------------------------------
# Data fetch + caching
# -------------------------------
read_parquet_url <- function(url) {
  arrow::read_parquet(url)
}

fetch_country_data <- function(asset_row, conversion_rates) {
  iso2c <- asset_row$iso2c
  ds    <- asset_row$ds
  url   <- asset_row$url
  
  dat <- tryCatch(read_parquet_url(url), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  
  dat <- dat %>%
    mutate(cntry = iso2c, ds = ds) %>%
    dplyr::filter(is.na(.data$no_data) | !"no_data" %in% names(dat))
  
  if (USE_DOLLAR_SPEND) {
    if (!("main_currency" %in% names(dat))) {
      dat <- dat %>% mutate(main_currency = NA_character_)
    }
    
    dat <- dat %>%
      left_join(conversion_rates, by = "main_currency") %>%
      mutate(
        total_spend_formatted = suppressWarnings(as.numeric(total_spend_formatted)),
        total_spend_formatted = if_else(total_spend_formatted == 100, 1, total_spend_formatted),
        conversion_rate = suppressWarnings(as.numeric(conversion_rate)),
        dollar_spend = if_else(!is.na(conversion_rate) & conversion_rate > 0,
                               total_spend_formatted / conversion_rate,
                               NA_real_)
      )
  }
  
  dat
}

cache_path <- function(iso2c) file.path(DATA_DIR, paste0(iso2c, ".rds"))

check_cached_data <- function(iso2c, latest_ds) {
  f <- cache_path(iso2c)
  if (!file.exists(f)) return(list(up_to_date = FALSE, reason = "no cache"))
  cached <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(cached) || !"ds" %in% names(cached)) return(list(up_to_date = FALSE, reason = "bad cache"))
  cached_ds <- unique(cached$ds)[1]
  if (!is.na(cached_ds) && cached_ds >= latest_ds) return(list(up_to_date = TRUE, data = cached, cached_ds = cached_ds))
  list(up_to_date = FALSE, reason = glue("outdated ({cached_ds} < {latest_ds})"))
}

refresh_all_data <- function(countries = NULL, timeframe_days = 30, force = FALSE) {
  rels <- get_available_countries(countries, timeframe_days)
  if (is.null(rels) || nrow(rels) == 0) return(NULL)
  
  latest_date <- suppressWarnings(max(as.Date(rels$ds), na.rm = TRUE))
  cli_alert_info("Latest ds in releases: {latest_date}")
  
  rates <- get_conversion_rates(latest_date)
  if (USE_DOLLAR_SPEND && is.null(rates)) {
    cli_alert_warning("Could not fetch conversion rates → will keep local spend only.")
    rates <- tibble(main_currency = character(), conversion_rate = numeric())
  }
  
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
  
  n_cached <- 0; n_fetched <- 0; n_failed <- 0
  
  data_list <- rels %>%
    split(.$iso2c) %>%
    map(function(x) {
      iso2c <- x$iso2c[1]
      ds    <- x$ds[1]
      
      if (!force) {
        cc <- check_cached_data(iso2c, ds)
        if (isTRUE(cc$up_to_date)) {
          n_cached <<- n_cached + 1
          cli_alert_success("{iso2c}: cached ({cc$cached_ds})")
          return(cc$data)
        }
      }
      
      cli_alert_info("{iso2c}: fetching {ds} …")
      dat <- fetch_country_data(x, rates)
      if (is.null(dat)) { n_failed <<- n_failed + 1; return(NULL) }
      
      n_fetched <<- n_fetched + 1
      if (SAVE_REFRESHED) saveRDS(dat, cache_path(iso2c))
      dat
    }) %>%
    compact()
  
  list(
    data = data_list,
    releases = rels,
    date = latest_date,
    stats = list(cached = n_cached, fetched = n_fetched, failed = n_failed)
  )
}

# -------------------------------
# Standardization: make columns consistent across countries
# -------------------------------
choose_internal_id <- function(dat) {
  candidates <- c("ad_archive_id", "ad_id", "archive_id", "creative_id", "adset_id", "ad_snapshot_url")
  found <- candidates[candidates %in% names(dat)]
  if (length(found)) return(found[1])
  "page_id"
}

standardize_data <- function(dat, iso2c) {
  assert_cols(dat, c("page_id"), "standardize_data")
  
  dat <- dat %>%
    mutate(page_id = as.character(page_id))
  
  # internal_id: MUST be per-ad ideally; fallback to page_id if nothing else exists
  id_col <- choose_internal_id(dat)
  if (id_col != "page_id") {
    dat <- dat %>% mutate(internal_id = as.character(.data[[id_col]]))
  } else {
    cli_alert_warning("{iso2c}: no ad-level id found; using page_id as internal_id (fallback)")
    dat <- dat %>% mutate(internal_id = page_id)
  }
  
  # Spend: pick USD if requested and available, else local
  if (USE_DOLLAR_SPEND && "dollar_spend" %in% names(dat) && any(!is.na(dat$dollar_spend))) {
    dat <- dat %>% mutate(total_spend = suppressWarnings(as.numeric(dollar_spend)))
  } else if ("total_spend_formatted" %in% names(dat)) {
    dat <- dat %>% mutate(total_spend = suppressWarnings(as.numeric(total_spend_formatted)))
  } else if ("total_spend" %in% names(dat)) {
    dat <- dat %>% mutate(total_spend = suppressWarnings(as.numeric(total_spend)))
  } else {
    stop(glue("{iso2c}: no spend column found"), call. = FALSE)
  }
  
  # Spend pct: accept 0-1 or 0-100
  if (!"total_spend_pct" %in% names(dat)) {
    dat <- dat %>% mutate(total_spend_pct = NA_real_)
  } else {
    dat <- dat %>% mutate(total_spend_pct = suppressWarnings(as.numeric(total_spend_pct)))
  }
  
  # Ads count: normalize to num_ads
  if ("num_ads" %in% names(dat)) {
    dat <- dat %>% mutate(num_ads = suppressWarnings(as.numeric(num_ads)))
  } else if ("total_num_ads" %in% names(dat)) {
    dat <- dat %>% mutate(num_ads = suppressWarnings(as.numeric(total_num_ads)))
  } else {
    dat <- dat %>% mutate(num_ads = 1)
  }
  
  # Ensure grouping columns exist
  if (!"actor_type" %in% names(dat)) dat <- dat %>% mutate(actor_type = "Unknown")
  if (!"party" %in% names(dat))      dat <- dat %>% mutate(party = "unknown")
  
  # Basic cleanup
  dat %>%
    mutate(
      actor_type = as.character(actor_type),
      party = as.character(party)
    )
}

# -------------------------------
# Spend deduplication (robust)
# -------------------------------
normalize_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_real_, ifelse(x > 1, x / 100, x))
}

deduplicate_spend <- function(dat,
                              spend_col = "total_spend",
                              spend_pct_col = "total_spend_pct",
                              ads_col = "num_ads",
                              id_col = "internal_id") {
  assert_cols(dat, c(spend_col, spend_pct_col, ads_col, id_col), "deduplicate_spend")
  
  dat %>%
    mutate(
      spend_total = suppressWarnings(as.numeric(.data[[spend_col]])),
      spend_pct   = normalize_pct(.data[[spend_pct_col]]),
      ads_n       = suppressWarnings(as.numeric(.data[[ads_col]])),
      likely_together = paste0(round(spend_pct %||% NA_real_, 6), "_", ads_n %||% NA_real_) %>%
        as.factor() %>% as.integer()
    ) %>%
    group_by(.data[[id_col]]) %>%
    add_count(likely_together, name = "n_clusters") %>%
    ungroup() %>%
    mutate(
      n_clusters = if_else(is.na(n_clusters) | n_clusters < 1, 1L, as.integer(n_clusters)),
      spend = if_else(!is.na(spend_total) & !is.na(spend_pct),
                      (spend_total * spend_pct) / n_clusters,
                      NA_real_)
    )
}

calculate_group_budgets <- function(dat, group_col = "actor_type",
                                    spend_col = "total_spend", id_col = "internal_id") {
  assert_cols(dat, c(group_col, spend_col, id_col), "calculate_group_budgets")
  dat %>%
    distinct(.data[[id_col]], .keep_all = TRUE) %>%
    group_by(.data[[group_col]]) %>%
    summarise(total_budget = sum(suppressWarnings(as.numeric(.data[[spend_col]])), na.rm = TRUE),
              .groups = "drop")
}

calculate_total_budget <- function(dat, spend_col = "total_spend", id_col = "internal_id") {
  assert_cols(dat, c(spend_col, id_col), "calculate_total_budget")
  dat %>%
    distinct(.data[[id_col]], .keep_all = TRUE) %>%
    summarise(total = sum(suppressWarnings(as.numeric(.data[[spend_col]])), na.rm = TRUE)) %>%
    pull(total)
}

# -------------------------------
# Generic “dimension prep” (one function for all demographics)
# -------------------------------
prepare_dimension <- function(dat,
                              dim_col,
                              group_col = "actor_type",
                              by_group = TRUE,
                              spend_col = "total_spend",
                              spend_pct_col = "total_spend_pct",
                              ads_col = "num_ads",
                              id_col = "internal_id",
                              top_n = NULL) {
  
  assert_cols(dat, c(dim_col, spend_col, spend_pct_col, ads_col, id_col), glue("prepare_dimension({dim_col})"))
  
  dplyr::filtered <- dat %>% dplyr::filter(!is.na(.data[[dim_col]]), .data[[dim_col]] != "")
  if (nrow(dplyr::filtered) == 0) return(tibble())
  
  dd <- deduplicate_spend(dplyr::filtered, spend_col, spend_pct_col, ads_col, id_col)
  total_all <- calculate_total_budget(dplyr::filtered, spend_col, id_col)
  
  if (by_group && group_col %in% names(dplyr::filtered)) {
    budgets <- calculate_group_budgets(dplyr::filtered, group_col, spend_col, id_col)
    
    out <- dd %>%
      group_by(.data[[group_col]], .data[[dim_col]]) %>%
      summarise(
        spend = sum(spend, na.rm = TRUE),
        n_ads = sum(suppressWarnings(as.numeric(.data[[ads_col]])), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(budgets, by = group_col) %>%
      mutate(
        perc = if_else(total_budget > 0, spend / total_budget * 100, 0),
        overall_perc = if_else(total_all > 0, spend / total_all * 100, 0),
        avg_spend_per_ad = if_else(n_ads > 0, spend / n_ads, 0)
      ) %>%
      group_by(.data[[group_col]]) %>%
      mutate(
        rank_within_group = dense_rank(desc(spend)),
        n_in_group = n(),
        percentile_within_group = round((rank_within_group - 1) / pmax(n_in_group, 1) * 100, 0)
      ) %>%
      ungroup() %>%
      mutate(
        rank_overall = dense_rank(desc(spend)),
        n_total = n(),
        percentile_overall = round((rank_overall - 1) / pmax(n_total, 1) * 100, 0)
      )
    
    if (!is.null(top_n)) {
      out <- out %>%
        group_by(.data[[group_col]]) %>%
        slice_max(order_by = spend, n = top_n, with_ties = FALSE) %>%
        ungroup()
    }
    
  } else {
    out <- dd %>%
      group_by(.data[[dim_col]]) %>%
      summarise(
        spend = sum(spend, na.rm = TRUE),
        n_ads = sum(suppressWarnings(as.numeric(.data[[ads_col]])), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        total_budget = total_all,
        perc = if_else(total_all > 0, spend / total_all * 100, 0),
        overall_perc = perc,
        avg_spend_per_ad = if_else(n_ads > 0, spend / n_ads, 0),
        rank_overall = dense_rank(desc(spend)),
        n_total = n(),
        percentile_overall = round((rank_overall - 1) / pmax(n_total, 1) * 100, 0)
      )
    
    if (!is.null(top_n)) {
      out <- out %>% slice_max(order_by = spend, n = top_n, with_ties = FALSE)
    }
  }
  
  # Standardize output column name so dashboard code stays simple
  out %>% rename(dimension = !!dim_col)
}

prepare_targeting_like <- function(dat,
                                   item_col,
                                   group_col = "actor_type",
                                   by_group = TRUE,
                                   top_n = 30) {
  prepare_dimension(dat,
                    dim_col = item_col,
                    group_col = group_col,
                    by_group = by_group,
                    top_n = top_n)
}

prepare_contested_audiences <- function(dat,
                                        target_col = "targeting",
                                        group_col = "actor_type",
                                        min_groups = 2,
                                        top_n = 30) {
  if (!all(c(target_col, group_col, "total_spend", "total_spend_pct", "num_ads", "internal_id") %in% names(dat))) return(tibble())
  
  dplyr::filtered <- dat %>% dplyr::filter(!is.na(.data[[target_col]]), .data[[target_col]] != "")
  if (nrow(dplyr::filtered) == 0) return(tibble())
  
  dd <- deduplicate_spend(dplyr::filtered)
  
  out <- dd %>%
    group_by(.data[[target_col]]) %>%
    summarise(
      n_groups = n_distinct(.data[[group_col]]),
      groups = paste(sort(unique(.data[[group_col]])), collapse = ", "),
      total_spend = sum(spend, na.rm = TRUE),
      total_ads = sum(suppressWarnings(as.numeric(num_ads)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_groups >= min_groups) %>%
    mutate(
      avg_spend_per_ad = if_else(total_ads > 0, total_spend / total_ads, 0),
      competitive_intensity = n_groups * total_spend,
      rank_by_spend = dense_rank(desc(total_spend)),
      rank_by_competition = dense_rank(desc(n_groups)),
      rank_by_intensity = dense_rank(desc(competitive_intensity))
    ) %>%
    arrange(desc(total_spend)) %>%
    slice_head(n = top_n) %>%
    rename(targeting = !!target_col)
  
  out
}

# -------------------------------
# Landing page HTML cards (no dependence on dashboardr content builders)
# -------------------------------
landing_cards_html <- function(stats, top_tbl, currency = "$", group_label = "Actor Types") {
  # bootstrap-ish simple cards; works inside markdown/html
  cards <- glue('
<div style="display:flex;flex-wrap:wrap;gap:12px;margin:12px 0 8px 0;">
  <div style="flex:1;min-width:220px;border:1px solid #e5e7eb;border-radius:12px;padding:12px;">
    <div style="font-size:12px;color:#6b7280;">Total spend</div>
    <div style="font-size:22px;font-weight:700;">{fmt_currency(stats$total_spend, currency)}</div>
  </div>
  <div style="flex:1;min-width:220px;border:1px solid #e5e7eb;border-radius:12px;padding:12px;">
    <div style="font-size:12px;color:#6b7280;">Total ads</div>
    <div style="font-size:22px;font-weight:700;">{fmt_num(stats$total_ads)}</div>
  </div>
  <div style="flex:1;min-width:220px;border:1px solid #e5e7eb;border-radius:12px;padding:12px;">
    <div style="font-size:12px;color:#6b7280;">{group_label} active</div>
    <div style="font-size:22px;font-weight:700;">{stats$n_groups}</div>
  </div>
  <div style="flex:1;min-width:220px;border:1px solid #e5e7eb;border-radius:12px;padding:12px;">
    <div style="font-size:12px;color:#6b7280;">Avg spend / ad</div>
    <div style="font-size:22px;font-weight:700;">{fmt_currency(stats$avg_spend_per_ad, currency)}</div>
  </div>
</div>
')
  
  top_html <- ""
  if (!is.null(top_tbl) && nrow(top_tbl) > 0) {
    rows <- apply(top_tbl, 1, function(r) {
      glue("<tr><td style='padding:6px 8px;'><b>{r[['group']]}</b></td><td style='padding:6px 8px;text-align:right;'>{fmt_currency(r[['spend']], currency)}</td><td style='padding:6px 8px;text-align:right;'>{fmt_num(r[['ads']])}</td></tr>")
    }) |> paste(collapse = "\n")
    
    top_html <- glue('
<h3 style="margin-top:16px;">Top spenders</h3>
<table style="border-collapse:collapse;min-width:320px;">
  <thead>
    <tr>
      <th style="text-align:left;padding:6px 8px;border-bottom:1px solid #e5e7eb;">{group_label}</th>
      <th style="text-align:right;padding:6px 8px;border-bottom:1px solid #e5e7eb;">Spend</th>
      <th style="text-align:right;padding:6px 8px;border-bottom:1px solid #e5e7eb;">Ads</th>
    </tr>
  </thead>
  <tbody>{rows}</tbody>
</table>
')
  }
  
  paste0(cards, top_html)
}

calculate_dashboard_stats <- function(dat,
                                      date_col = "ds",
                                      spend_col = "total_spend",
                                      ads_col = "num_ads",
                                      group_col = "actor_type",
                                      id_col = "internal_id") {
  assert_cols(dat, c(spend_col, ads_col, id_col), "calculate_dashboard_stats")
  
  # IMPORTANT FIX: totals must be on DISTINCT internal_id (not the targeting rows)
  distinct_ads <- dat %>% distinct(.data[[id_col]], .keep_all = TRUE)
  
  total_spend <- sum(suppressWarnings(as.numeric(distinct_ads[[spend_col]])), na.rm = TRUE)
  total_ads   <- sum(suppressWarnings(as.numeric(distinct_ads[[ads_col]])), na.rm = TRUE)
  
  n_groups <- if (group_col %in% names(dat)) n_distinct(dat[[group_col]]) else NA_integer_
  
  date_range <- "Date range not available"
  latest_date <- NULL
  
  if (date_col %in% names(dat)) {
    dd <- suppressWarnings(as.Date(dat[[date_col]]))
    if (any(!is.na(dd))) {
      latest_date <- max(dd, na.rm = TRUE)
      earliest_date <- min(dd, na.rm = TRUE)
      date_range <- paste0(format(earliest_date, "%b %d, %Y"), " – ", format(latest_date, "%b %d, %Y"))
    }
  }
  
  avg_spend_per_ad <- if (total_ads > 0) total_spend / total_ads else 0
  
  top_group <- NA_character_
  top_group_spend <- 0
  
  if (group_col %in% names(dat)) {
    top <- distinct_ads %>%
      group_by(.data[[group_col]]) %>%
      summarise(spend = sum(suppressWarnings(as.numeric(.data[[spend_col]])), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(spend)) %>%
      slice_head(n = 1)
    
    if (nrow(top) == 1) {
      top_group <- top[[group_col]][1]
      top_group_spend <- top$spend[1]
    }
  }
  
  list(
    total_spend = total_spend,
    total_ads = total_ads,
    n_groups = n_groups,
    date_range = date_range,
    avg_spend_per_ad = avg_spend_per_ad,
    top_group = top_group,
    top_group_spend = top_group_spend,
    latest_date = latest_date
  )
}

top_spenders_table <- function(dat, group_col = "actor_type", spend_col = "total_spend",
                               ads_col = "num_ads", id_col = "internal_id", n_top = 5) {
  if (!group_col %in% names(dat)) return(NULL)
  distinct_ads <- dat %>% distinct(.data[[id_col]], .keep_all = TRUE)
  
  distinct_ads %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      spend = sum(suppressWarnings(as.numeric(.data[[spend_col]])), na.rm = TRUE),
      ads = sum(suppressWarnings(as.numeric(.data[[ads_col]])), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(spend)) %>%
    slice_head(n = n_top) %>%
    rename(group = !!group_col)
}

# -------------------------------
# Build vizzes (minimal, robust)
# -------------------------------
make_stackedbar_viz <- function(title, subtitle, x_var, stack_var = NULL,
                                weight_var = "perc", horizontal = FALSE,
                                icon = NULL, height = 480) {
  v <- dashboardr::create_viz()
  v %>%
    dashboardr::add_viz(
      type = "stackedbar",
      x_var = x_var,
      stack_var = stack_var,
      weight_var = weight_var,
      title = title,
      subtitle = subtitle,
      y_label = "% of budget",
      stacked_type = if (!is.null(stack_var)) "grouped" else "normal",
      horizontal = horizontal,
      height = height,
      icon = icon
    )
}

make_bar_viz <- function(title, subtitle, x_var, weight_var, horizontal = TRUE, icon = NULL, height = 520) {
  dashboardr::create_viz() %>%
    dashboardr::add_viz(
      type = "bar",
      x_var = x_var,
      weight_var = weight_var,
      title = title,
      subtitle = subtitle,
      horizontal = horizontal,
      height = height,
      icon = icon
    )
}

# -------------------------------
# Country dashboard creation
# -------------------------------
create_country_dashboard <- function(iso2c, dat, country_name,
                                     group_col = "actor_type",
                                     currency = "$") {
  
  group_label <- tools::toTitleCase(gsub("_", " ", group_col))
  
  stats <- calculate_dashboard_stats(dat, group_col = group_col)
  top_tbl <- top_spenders_table(dat, group_col = group_col)
  
  landing_text <- paste0(
    "# Political Advertising Dashboard: ", country_name, "\n\n",
    "**Coverage:** ", stats$date_range, "\n\n",
    landing_cards_html(stats, top_tbl, currency = currency, group_label = group_label), "\n\n",
    "## What you can explore\n\n",
    "- **Demographics:** age, gender, language, education, employment, relationship status\n",
    "- **Geographic:** where spending concentrates (location targeting)\n",
    "- **Targeting:** interests/behaviors/custom audience signals (if available)\n",
    "- **Competitive:** contested segments targeted by multiple actors\n",
    "- **Excluded:** who advertisers explicitly avoid (if available)\n"
  )
  
  dash <- dashboardr::create_dashboard(
    output_dir = file.path(OUTPUT_DIR, iso2c),
    title = paste0(country_name, " Election Advertising Dashboard"),
    description = paste0("Political advertising targeting analysis for ", country_name),
    allow_inside_pkg = TRUE
  ) %>%
    dashboardr::add_page(
      name = "Overview",
      is_landing_page = TRUE,
      text = paste0("[← Back to World Map](../index.html)\n\n", landing_text)
    )
  
  # ----------------
  # Demographics page
  # ----------------
  demo_dims <- list(
    age = "age",
    gender = "gender",
    language = "language",
    education = "education",
    employment = "employment",
    relationship = "relationship_status"
  )
  
  demo_viz <- dashboardr::create_viz()
  demo_any <- FALSE
  
  for (nm in names(demo_dims)) {
    col <- demo_dims[[nm]]
    if (!col %in% names(dat)) next
    
    prep <- prepare_dimension(dat, dim_col = col, group_col = group_col, by_group = TRUE, top_n = if (nm %in% c("language","education","employment")) 25 else NULL)
    if (nrow(prep) == 0) next
    
    # Use a consistent x-axis column name expected by dashboardr:
    # We'll copy dimension → actual x variable name
    prep <- prep %>% mutate(x = .data$dimension)
    
    this <- make_stackedbar_viz(
      title = paste0(tools::toTitleCase(nm), " targeting"),
      subtitle = paste0("Share of each ", group_label, "'s budget allocated to ", nm, " segments (deduplicated spend)."),
      x_var = "x",
      stack_var = group_col,
      horizontal = nm %in% c("language","education","employment"),
      icon = "ph:users-three",
      height = if (nm %in% c("language","education","employment")) 560 else 460
    )
    
    dash <- dash %>% dashboardr::add_page(
      name = paste0("Demographics: ", tools::toTitleCase(nm)),
      data = prep,
      visualizations = this,
      text = paste0(
        "# ", tools::toTitleCase(nm), "\n\n",
        "Hover bars for spend, ads, within-", group_col, " %, overall %, ranks.\n"
      )
    )
    demo_any <- TRUE
  }
  
  # ----------------
  # Geographic page
  # ----------------
  if ("location" %in% names(dat)) {
    loc <- prepare_dimension(dat, dim_col = "location", group_col = group_col, by_group = TRUE, top_n = 40)
    if (nrow(loc) > 0) {
      loc <- loc %>% mutate(x = .data$dimension)
      
      loc_viz <- make_bar_viz(
        title = "Geographic targeting (top locations)",
        subtitle = paste0("Top locations by deduplicated spend; stacked by ", group_label, "."),
        x_var = "x",
        weight_var = "spend",
        horizontal = TRUE,
        icon = "ph:map-trifold",
        height = 620
      )
      
      dash <- dash %>% dashboardr::add_page(
        name = "Geographic",
        data = loc,
        visualizations = loc_viz,
        text = "# Geographic targeting\n\nTop locations by spend (deduplicated).\n"
      )
    }
  }
  
  # ----------------
  # Targeting page
  # ----------------
  if ("targeting" %in% names(dat)) {
    tgt <- prepare_targeting_like(dat, item_col = "targeting", group_col = group_col, by_group = TRUE, top_n = 35)
    if (nrow(tgt) > 0) {
      tgt <- tgt %>% mutate(x = .data$dimension)
      
      tgt_viz <- make_stackedbar_viz(
        title = "Targeting: interests & behaviors (top)",
        subtitle = paste0("Top targeting items by deduplicated spend; within-", group_label, " budget shares."),
        x_var = "x",
        stack_var = group_col,
        horizontal = TRUE,
        icon = "ph:target",
        height = 680
      )
      
      dash <- dash %>% dashboardr::add_page(
        name = "Targeting",
        data = tgt,
        visualizations = tgt_viz,
        text = "# Targeting\n\nTop interests/behaviors/custom segments.\n"
      )
    }
  }
  
  # ----------------
  # Competitive (contested)
  # ----------------
  if ("targeting" %in% names(dat) && group_col %in% names(dat)) {
    con <- prepare_contested_audiences(dat, target_col = "targeting", group_col = group_col, min_groups = 2, top_n = 35)
    if (nrow(con) > 0) {
      con_viz <- make_bar_viz(
        title = "Contested audience segments",
        subtitle = "Segments targeted by multiple actors; higher spend indicates stronger competition.",
        x_var = "targeting",
        weight_var = "total_spend",
        horizontal = TRUE,
        icon = "ph:hand-fist",
        height = 650
      )
      
      dash <- dash %>% dashboardr::add_page(
        name = "Competitive",
        data = con,
        visualizations = con_viz,
        text = "# Competitive targeting\n\nContested segments are targeted by multiple groups.\n"
      )
    }
  }
  
  # ----------------
  # Excluded audiences
  # ----------------
  if ("excluded" %in% names(dat)) {
    exc <- prepare_targeting_like(dat, item_col = "excluded", group_col = group_col, by_group = TRUE, top_n = 30)
    if (nrow(exc) > 0) {
      exc <- exc %>% mutate(x = .data$dimension)
      
      exc_viz <- make_stackedbar_viz(
        title = "Excluded audiences (top)",
        subtitle = paste0("Audiences explicitly excluded; stacked by ", group_label, "."),
        x_var = "x",
        stack_var = group_col,
        horizontal = TRUE,
        icon = "ph:prohibit",
        height = 620
      )
      
      dash <- dash %>% dashboardr::add_page(
        name = "Excluded",
        data = exc,
        visualizations = exc_viz,
        text = "# Excluded audiences\n\nAudiences advertisers explicitly avoid.\n"
      )
    }
  }
  
  # About
  dash <- dash %>% dashboardr::add_page(
    name = "About",
    text = paste0(
      "# About\n\n",
      "- Spend is **deduplicated** for overlapping targeting clusters.\n",
      "- Percentages are **within ", group_col, " budgets**, plus overall %.\n",
      "- Totals use **distinct internal_id** (ad-level id if available; fallback otherwise).\n\n",
      "*Generated: ", format(Sys.Date(), "%Y-%m-%d"), "*\n"
    )
  ) %>%
    dashboardr::add_powered_by_dashboardr(size = "large")
  
  dash
}

# -------------------------------
# MAIN SCRIPT
# -------------------------------
cli_h1("Worldwide Dashboard Generator (fixed rewrite)")

# 1) Refresh or load cached
refresh <- refresh_all_data(
  countries = if (TEST_MODE) TEST_COUNTRIES else NULL,
  timeframe_days = TIMEFRAME_DAYS,
  force = FORCE_REFRESH
)

if (is.null(refresh)) {
  cli_alert_warning("Refresh failed; falling back to cached .rds files in {DATA_DIR}")
}

# Determine available countries from cache
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
country_files <- list.files(DATA_DIR, "\\.rds$", full.names = FALSE)
available_countries <- gsub("\\.rds$", "", country_files)

if (TEST_MODE) {
  available_countries <- intersect(available_countries, TEST_COUNTRIES %||% available_countries)
  cli_alert_warning("TEST MODE: {paste(available_countries, collapse=', ')}")
}

cli_alert_info("Countries in cache: {length(available_countries)}")

# 2) Build country summary for world map
country_summary <- map_dfr(available_countries, function(iso2c) {
  dat <- tryCatch(readRDS(cache_path(iso2c)), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  
  dat <- standardize_data(dat, iso2c)
  
  distinct_ads <- dat %>% distinct(internal_id, .keep_all = TRUE)
  
  tibble(
    iso2c = iso2c,
    country = countrycode(iso2c, "iso2c", "country.name", warn = FALSE) %||% iso2c,
    total_spend = sum(distinct_ads$total_spend, na.rm = TRUE),
    total_ads = sum(distinct_ads$num_ads, na.rm = TRUE),
    n_advertisers = n_distinct(distinct_ads$page_id)
  )
}) %>%
  dplyr::filter(total_spend > 0, total_ads > 0) %>%
  arrange(desc(total_spend))

cli_alert_success("Countries with usable data: {nrow(country_summary)}")

# 3) Main world map dashboard
cli_h1("Creating main world map dashboard")

data_date_display <- if (!is.null(refresh) && !is.na(refresh$date)) {
  format(as.Date(refresh$date), "%B %d, %Y")
} else {
  "Unknown"
}

main_viz <- dashboardr::create_viz() %>%
  dashboardr::add_viz(
    type = "map",
    value_var = "total_spend",
    join_var = "iso2c",
    click_url_template = "{iso2c}/index.html",
    title = "Election Ad Spending by Country",
    subtitle = "Click a country to explore detailed targeting",
    tooltip_vars = c("country", "total_spend", "total_ads", "n_advertisers"),
    legend_title = "Total Spend (USD)"
  )

main_dashboard <- dashboardr::create_dashboard(
  title = "Who Targets Me - Worldwide",
  output_dir = OUTPUT_DIR,
  description = "Worldwide political ad targeting dashboard",
  allow_inside_pkg = TRUE
) %>%
  dashboardr::add_page(
    name = "World Map",
    data = country_summary,
    visualizations = main_viz,
    is_landing_page = TRUE,
    text = paste0(
      "# Worldwide Ad Targeting\n\n",
      "Click on any country to explore detailed targeting analysis.\n\n",
      "- **Countries:** ", nrow(country_summary), "\n",
      "- **Data:** ", data_date_display, "\n\n",
      "*Spend shown in USD if conversion was available; otherwise local.*\n"
    )
  ) %>%
  dashboardr::add_powered_by_dashboardr(size = "large")

# 4) Country dashboards
cli_h1("Creating country dashboards")

country_dashboards <- country_summary$iso2c %>%
  set_names() %>%
  map(function(iso2c) {
    country_name <- country_summary$country[country_summary$iso2c == iso2c][1] %||% iso2c
    dat <- tryCatch(readRDS(cache_path(iso2c)), error = function(e) NULL)
    if (is.null(dat) || nrow(dat) == 0) return(NULL)
    
    dat <- standardize_data(dat, iso2c)
    
    # Default grouping: actor_type (your intended default)
    create_country_dashboard(
      iso2c = iso2c,
      dat = dat,
      country_name = country_name,
      group_col = "actor_type",
      currency = "$"
    )
  }) %>% compact()

cli_alert_success("Built {length(country_dashboards)} country dashboards")

# 5) Generate everything
cli_h1("Generating dashboards")

all_dashboards <- c(list(main = main_dashboard), country_dashboards)

# dashboardr sometimes provides generate_dashboards(); sometimes generate_dashboard()
if (has_fun("dashboardr", "generate_dashboards")) {
  results <- dashboardr::generate_dashboards(
    all_dashboards,
    render = TRUE,
    continue_on_error = TRUE,
    linked = TRUE,
    open = TRUE
  )
} else {
  results <- lapply(all_dashboards, function(d) {
    dashboardr::generate_dashboard(d, render = TRUE)
  })
}

cli_h1("Done")
cli_alert_success("Output directory: {OUTPUT_DIR}")
cli_alert_info("Main dashboard entry: {file.path(OUTPUT_DIR, 'docs', 'index.html')}")