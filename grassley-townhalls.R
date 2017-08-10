library(tidyverse)

library(leaflet)
library(lubridate)
library(RColorBrewer)
library(sf)
library(stringr)
library(tidycensus)
library(USAboundaries)

# setwd("~/work/data/grassley-townhalls")

iowa_census_file <- "data/iowa_census.RData"

if (!dir.exists("data")) {
    dir.create("data")
}

# Get Iowa census data, including county shapefiles. (Requires API key.)

if (!file.exists(iowa_census_file)) {
    source("census-api-key.R")
    census_api_key(getOption("api_key"))

    # B01003_001 is Total Population
    iowa_census_data <- get_acs(geography = "county",
                                variables = "B01003_001",
                                state = "IA",
                                geometry = TRUE)

    save(iowa_census_data, file = iowa_census_file)
} else {
    load(iowa_census_file)
}

# Get 2016 election results from Data for Democracy

election_results_file <- "data/PresidentialElectionResults2016.RData"

if (!file.exists(election_results_file)) {
    # Source: https://data.world/data4democracy/election-transparency
    election_results <- read_csv("https://query.data.world/s/2udr4m30w08wdcu1y8vffo4aa")
    save(election_results, file = election_results_file)
} else {
    load(election_results_file)
}

iowa_results <- election_results %>%
    filter(StateAbbr == "IA")

# Combine census data and Iowa election result data.

census_df <- iowa_census_data %>%
    mutate(county = gsub(" County, Iowa", "", NAME), population = estimate) %>%
    select(county, population, geometry) %>%
    inner_join(iowa_results, by = c("county" = "CountyName"))

# Chuck Grassley’s August 2017 town halls. Data from https://townhallproject.com/.
# They don’t appear to have an API.
# We geocode the town hall locations using the ggmap package.

grassley_townhalls_file <- "data/grassley_townhalls.RData"

if (!file.exists(grassley_townhalls_file)) {
    library(ggmap)

    # Source: https://townhallproject.com/
    grassley_townhalls <- data.frame(
        location = c("Mt Ayr, IA",
                     "Bedford, IA",
                     "Primghar, IA",
                     "Sibley, IA",
                     "Mapleton, IA",
                     "Harlan, IA"),
        date = c("2017-08-23",
                 "2017-08-24",
                 "2017-08-29",
                 "2017-08-30",
                 "2017-08-31",
                 "2017-08-31"),
        time = c("3:45 PM, CDT",
                 "11:30 AM, CDT",
                 "3:45 PM, CDT",
                 "08:00 AM, CDT",
                 "10:00 AM, CDT",
                 "2:45 PM, CDT "),
        stringsAsFactors = FALSE)

    # Geocode locations
    grassley_townhalls <- grassley_townhalls %>%
        mutate_geocode(location)

    save(grassley_townhalls, file = grassley_townhalls_file)
} else {
    load(grassley_townhalls_file)
}

# grassley_townhalls

# Use congressional district boundaries from USAboundaries package.

iowa_congressional_boundaries <- us_congressional(states = "Iowa", resolution = "low")

# Functions for town hall and county popup labels for the maps.

town_hall_create_label <- function(df) {
    paste(sep = "<br/>",
          paste0("<b>", df$location, "</b>"),
          paste("<b>Date:</b>", format(as_date(df$date), "%A, %B %d, %Y")),
          paste("<b>Time:</b>", df$time)
    )
}

county_create_label <- function(df) {
    paste(sep = "<br/>",
          paste0("<b>", df$county, "</b>"),
          paste("<b>Population:</b>", df$population),
          paste("<b>Trump vote:</b>", sprintf("%1.2f%%", 100 * df$rDRPct))
    )
}

grassley_townhalls <- grassley_townhalls %>%
    mutate(townhall_popup = town_hall_create_label(.))

# Population map

# Color palette based on log(population)
pop_pal <- colorNumeric(brewer.pal(9, "Blues"), c(log(3000), log(500000)))

legend_pops <- seq(50000, 450000, by = 100000)
legend_colors <- lapply(log(legend_pops), pop_pal)

pop_map <- census_df %>%
    mutate(county_popup = county_create_label(.)) %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(data = iowa_congressional_boundaries,
                color = "black") %>%
    addPolygons(popup = ~ county_popup,
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pop_pal(log(population))) %>%
    addLegend("bottomright",
              colors = legend_colors,
              labels = legend_pops,
              values = ~ population,
              title = "Population",
              opacity = 1) %>%
    addMarkers(data = grassley_townhalls, lng = ~ lon, lat = ~ lat, popup = ~ townhall_popup)

# Trump vote map

vote_pal <- colorNumeric(colorRamp(c("#0000ff", "#ffffff", "#ff0000")), 0.0:1.0)

vote_map <- census_df %>%
    mutate(county_popup = county_create_label(.)) %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(data = iowa_congressional_boundaries,
                color = "black") %>%
    addPolygons(popup = ~ county_popup,
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ vote_pal(rDRPct)) %>%
    addLegend("bottomright",
              pal = vote_pal,
              values = ~ dDRPct,
              title = "Trump Vote %",
              opacity = 1) %>%
    addMarkers(data = grassley_townhalls, lng = ~ lon, lat = ~ lat, popup = ~ townhall_popup)
