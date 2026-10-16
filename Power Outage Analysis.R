# ===================== outage_analysis.R =====================
# Power outage analysis and visualization: EAGLE-I data
# Utility functions + event analysis in one script

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tmap)
library(tigris)
library(stringr)

options(tigris_use_cache = TRUE)

`%notin%` <- Negate(`%in%`)

# ===================== UTILITY FUNCTIONS =====================

#' Load EAGLE-I outage data from CSV file
load_outage_data <- function(filename, datetime_col = "run_start_time") {
  filepath <- file.path("data", filename)
  
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath))
  }
  
  df <- readr::read_csv(filepath, show_col_types = FALSE) %>%
    dplyr::rename(datetime = !!sym(datetime_col))
  
  return(df)
}

#' Filter outage data by state and date range
filter_by_location_and_date <- function(df, state_name, start_date, end_date) {
  start <- lubridate::dmy(start_date)
  end <- lubridate::dmy(end_date)
  
  df %>%
    dplyr::filter(state == state_name,
                  datetime >= start,
                  datetime <= end)
}

#' Aggregate outages by date (sum across all counties per day)
aggregate_daily_outages <- function(df, customers_col = "customers_out") {
  df %>%
    dplyr::mutate(date = lubridate::as_date(datetime)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(total_customers_out = sum(!!sym(customers_col), na.rm = TRUE),
                     .groups = "drop")
}

#' Calculate maximum customers out per county during the period
aggregate_county_max_outages <- function(df, customers_col = "customers_out") {
  df %>%
    dplyr::group_by(fips_code, county) %>%
    dplyr::summarise(max_customers_out = max(!!sym(customers_col), na.rm = TRUE),
                     .groups = "drop")
}

#' Create time series line plot of daily outages
plot_time_series <- function(daily_df, state_name, event_name,
                             line_color = "red", save_path = NULL) {
  
  if (!dir.exists("outputs")) dir.create("outputs")
  
  p <- ggplot(daily_df, aes(x = date, y = total_customers_out)) +
    geom_line(color = line_color, size = 1.2) +
    geom_point(color = line_color, size = 3, shape = 19) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Power Outages in", state_name, "During", event_name),
      x = "Date",
      y = "Customers Out"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  if (!is.null(save_path)) {
    full_path <- file.path("outputs", save_path)
    ggsave(full_path, plot = p, width = 12, height = 6, dpi = 300)
    cat(paste0("   Saved: ", full_path, "\n"))
  }
  
  print(p)
  return(p)
}

#' Create county-level choropleth map of maximum outages
create_choropleth_map <- function(county_data, state_code, state_name,
                                  event_name, color_palette = "YlOrRd",
                                  save_path = NULL) {
  
  if (!dir.exists("outputs")) dir.create("outputs")
  
  cat("   Downloading county boundaries from TIGER/Line...\n")
  
  tryCatch({
    counties_sf <- tigris::counties(state = state_code,
                                    year = 2019,
                                    resolution = "500k",
                                    cb = TRUE) %>%
      dplyr::select(GEOID, NAME)
    
    cat(paste0("   Successfully loaded ", nrow(counties_sf), " counties\n"))
    
    counties_sf <- counties_sf %>%
      dplyr::mutate(GEOID = stringr::str_pad(as.character(GEOID),
                                             width = 5, pad = "0", side = "left"))
    
    county_data <- county_data %>%
      dplyr::mutate(fips_code = stringr::str_pad(as.character(fips_code),
                                                 width = 5, pad = "0", side = "left"))
    
    map_data <- counties_sf %>%
      dplyr::left_join(county_data, by = c("GEOID" = "fips_code")) %>%
      dplyr::mutate(max_customers_out = ifelse(is.na(max_customers_out),
                                               0, max_customers_out))
    
    tmap::tmap_mode("plot")
    
    tm <- tmap::tm_shape(map_data) +
      tmap::tm_fill("max_customers_out",
                    style = "jenks",
                    n = 7,
                    palette = color_palette,
                    title = "Max Customers Out") +
      tmap::tm_borders(col = "gray40", lwd = 0.3) +
      tmap::tm_layout(
        main.title = paste0("Maximum Customers Without Power by County\n",
                            state_name, " ", event_name),
        main.title.position = "center",
        main.title.size = 1.2,
        legend.outside = TRUE,
        legend.title.size = 1,
        legend.text.size = 0.7,
        frame = FALSE
      )
    
    if (!is.null(save_path)) {
      full_path <- file.path("outputs", save_path)
      tmap::tmap_save(tm, filename = full_path, dpi = 300, width = 12, height = 10)
      cat(paste0("   Saved: ", full_path, "\n"))
    }
    
    print(tm)
    return(tm)
    
  }, error = function(e) {
    cat(paste0("   Error creating map: ", e$message, "\n"))
    return(NULL)
  })
}

#' Generate summary statistics for the outage event
generate_summary_statistics <- function(daily_df, county_df) {
  peak_idx <- which.max(daily_df$total_customers_out)
  max_county_idx <- which.max(county_df$max_customers_out)
  
  list(
    peak_outages = max(daily_df$total_customers_out, na.rm = TRUE),
    peak_date = as.character(daily_df$date[peak_idx]),
    total_counties_affected = sum(county_df$max_customers_out > 0, na.rm = TRUE),
    most_affected_county = county_df$county[max_county_idx],
    most_affected_county_outages = max(county_df$max_customers_out, na.rm = TRUE),
    average_daily_outages = mean(daily_df$total_customers_out, na.rm = TRUE)
  )
}

# ===================== EVENT DEFINITIONS =====================
events <- list(
  list(
    name        = "Texas Winter Storm Uri",
    data_file   = "eaglei_outages_2021.csv",
    state_name  = "Texas",
    state_abbr  = "TX",
    start_date  = "13-2-2021",
    end_date    = "23-2-2021"
  ),
  list(
    name        = "Hurricane Ida",
    data_file   = "eaglei_outages_2021.csv",
    state_name  = "Louisiana",
    state_abbr  = "LA",
    start_date  = "27-8-2021",
    end_date    = "25-9-2021"
  ),
  list(
    name        = "Hurricane Irma",
    data_file   = "eaglei_outages_2017.csv",
    state_name  = "Florida",
    state_abbr  = "FL",
    start_date  = "8-9-2017",
    end_date    = "20-9-2017"
  )
)

# ===================== DATA CACHE =====================
# Avoid re-reading the same CSV for events that share a file (both 2021 events)
.data_cache <- new.env()

get_outage_data <- function(filename) {
  if (!exists(filename, envir = .data_cache)) {
    assign(filename, load_outage_data(filename), envir = .data_cache)
  }
  get(filename, envir = .data_cache)
}

# ===================== RUN ALL EVENTS =====================
run_event_analysis <- function(event) {
  cat("\n=== ", event$name, " (", event$state_name, ") ===\n", sep = "")
  
  df <- get_outage_data(event$data_file)
  
  filtered <- filter_by_location_and_date(
    df, event$state_name, event$start_date, event$end_date
  )
  
  daily <- aggregate_daily_outages(filtered)
  county_max <- aggregate_county_max_outages(filtered)
  
  ts_filename  <- paste0(gsub(" ", "_", event$name), "_timeseries.png")
  map_filename <- paste0(gsub(" ", "_", event$name), "_map.png")
  
  plot_time_series(
    daily, event$state_name, event$name,
    save_path = ts_filename
  )
  
  create_choropleth_map(
    county_max, event$state_abbr, event$state_name, event$name,
    save_path = map_filename
  )
  
  generate_summary_statistics(daily, county_max)
}

results <- lapply(events, run_event_analysis)
names(results) <- sapply(events, function(e) e$name)

# Inspect summary stats for any event, e.g.:
# results$`Hurricane Ida`