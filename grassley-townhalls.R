library(tidyverse)

library(leaflet)
library(lubridate)
library(RColorBrewer)
library(scales)
library(sf)
library(stringr)
library(tidycensus)
library(readxl)

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
    select(GEOID, county, population, geometry) %>%
    inner_join(iowa_results, by = c("county" = "CountyName"))

# Chuck Grassley’s August 2017 town halls. Data from https://townhallproject.com/.
# They don’t appear to have an API.
# We geocode the town hall locations using the ggmap package.
# Updated with subsequent town halls from https://www.grassley.senate.gov/news

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
                     "Harlan, IA",
                     "Boone, IA",
                     "Logan, IA",
                     "Sac City, IA",
                     "Bloomfield, IA",
                     "Keosaqua, IA",
                     "Wapello, IA",
                     "Manchester, IA",
                     "Bellevue, IA",
                     "New Hampton, IA",
                     "Eagle Grove, IA",
                     "Orange City, IA"),
        county = c("Ringgold",
                   "Taylor",
                   "O'Brien",
                   "Osceola",
                   "Monona",
                   "Shelby",
                   "Boone",
                   "Harrison",
                   "Sac",
                   "Davis",
                   "Van Buren",
                   "Louisa",
                   "Delaware",
                   "Jackson",
                   "Chickasaw",
                   "Wright",
                   "Sioux"),
        date = c("2017-08-23",
                 "2017-08-24",
                 "2017-08-29",
                 "2017-08-30",
                 "2017-08-31",
                 "2017-08-31",
                 "2017-10-9",
                 "2018-01-12",
                 "2018-01-13",
                 "2018-02-21",
                 "2018-02-22",
                 "2018-02-23",
                 "2018-02-23",
                 "2018-05-1",
                 "2018-05-4",
                 "2018-05-30",
                 "2018-05-31"),
        time = c("3:45 PM, CDT",
                 "11:30 AM, CDT",
                 "3:45 PM, CDT",
                 "08:00 AM, CDT",
                 "10:00 AM, CDT",
                 "2:45 PM, CDT",
                 "2:15 PM, CDT",
                 "10:45 AM, CST",
                 "10:30 AM, CST",
                 "3:45 PM, CST",
                 "8:00 AM, CST",
                 "10:15 AM, CST",
                 "4:00 PM, CST",
                 "2:00 PM, CDT",
                 "8:00 AM, CDT",
                 "8:30 AM, CDT",
                 "2:00 PM, CDT"),
        stringsAsFactors = FALSE)

    # Geocode locations
    grassley_townhalls <- grassley_townhalls %>%
        mutate_geocode(location)

    save(grassley_townhalls, file = grassley_townhalls_file)
} else {
    load(grassley_townhalls_file)
}

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
          paste("<b>Trump vote:</b>", sprintf("%1.2f%%", 100 * df$rPct))
    )
}

grassley_townhalls <- grassley_townhalls %>%
    mutate(townhall_popup = town_hall_create_label(.)) %>%
    inner_join(census_df, by = "county") %>%
    mutate(county_population = population) %>%
    select(location, county, county_population, rPct, date, time, lon, lat, townhall_popup)

# Population map

# Color palette based on log(population)
pop_pal <- colorNumeric(brewer.pal(9, "Blues"), c(log(3000), log(500000)))

legend_pops <- c(10000, seq(50000, 450000, by = 100000))
legend_colors <- lapply(log(legend_pops), pop_pal)

pop_map <- census_df %>%
    mutate(county_popup = county_create_label(.)) %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ county_popup,
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pop_pal(log(population))) %>%
    addLegend("bottomright",
              colors = legend_colors,
              labels = format(legend_pops, big.mark = ","),
              values = ~ population,
              title = "County Population",
              opacity = 1) %>%
    addMarkers(data = grassley_townhalls, lng = ~ lon, lat = ~ lat, popup = ~ townhall_popup)

# Trump vote map

vote_pal <- colorNumeric(colorRamp(c("#0000ff", "#ffffff", "#ff0000")), 0.0:1.0)

vote_map <- census_df %>%
    mutate(county_popup = county_create_label(.)) %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ county_popup,
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ vote_pal(rPct)) %>%
    addLegend("bottomright",
              pal = vote_pal,
              values = ~ dPct,
              title = "Trump Vote %",
              opacity = 1) %>%
    addMarkers(data = grassley_townhalls, lng = ~ lon, lat = ~ lat, popup = ~ townhall_popup)

# Town hall count map

# town halls from 2011 - 2017
townhall_counts <- read_excel("data/Chuck Grassley Public Town Meetings 2011-2017.xlsx", skip = 1)

townhall_counts <- townhall_counts %>%
  select(-starts_with("TOTAL")) %>%
  filter(County != "TOTALS") %>%
  gather(year, count, -County)

townhall_counts_after_2017 <- grassley_townhalls %>%
  filter(year(date) > 2017) %>%
  mutate(County = county, year = as.character(year(date))) %>%
  select(County, year) %>%
  group_by(County, year) %>%
  summarize(count = n()) %>%
  ungroup()

townhall_counts <- rbind(townhall_counts, townhall_counts_after_2017)

iowa_county_data_url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2017_Gazetteer/2017_gaz_counties_19.txt"

iowa_county_data_file <- "data/iowa_county_data.RData"

# we need latitudes and longitudes to center labels in each county

if (!file.exists(iowa_county_data_file)) {
  iowa_county_data <- read_tsv(iowa_county_data_url, col_types = cols(
    USPS = col_character(),
    GEOID = col_character(),
    ANSICODE = col_character(),
    NAME = col_character(),
    ALAND = col_double(),
    AWATER = col_integer(),
    ALAND_SQMI = col_double(),
    AWATER_SQMI = col_double(),
    INTPTLAT = col_double(),
    INTPTLONG = col_double()
  ))
  save(iowa_county_data, file = iowa_county_data_file)
} else {
  load(iowa_county_data_file)
}

iowa_county_data <- iowa_county_data %>%
  select(GEOID, NAME, INTPTLAT, INTPTLONG) %>%
  mutate(lat = INTPTLAT, lon = INTPTLONG)

townhall_count_summary <- townhall_counts %>%
  group_by(County) %>%
  summarize(meetings = sum(count))

census_df <- census_df %>%
  inner_join(townhall_count_summary, by = c("county" = "County")) %>%
  inner_join(iowa_county_data, by = c("GEOID" = "GEOID")) %>%
  select(county, population, geometry, lat, lon, meetings, rPct)

townhall_count_map <- census_df %>%
  mutate(county_popup = county_create_label(.)) %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ county_popup,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pop_pal(log(population))) %>%
  addLegend("bottomright",
            colors = legend_colors,
            labels = format(legend_pops, big.mark = ","),
            values = ~ population,
            title = "County Population",
            opacity = 1) %>%
  addLabelOnlyMarkers(lng = ~ lon, lat = ~ lat,
                      label = ~ as.character(meetings),
                      labelOptions = labelOptions(noHide = T,
                                                  direction = 'top',
                                                  textOnly = T,
                                                  style = list(
                                                    "font-size" = "20px"
                                                  )))

# County population bar chart

zero_town_hall_counties <- census_df[census_df$meetings == 0,]$county

pop_bars <- census_df %>%
    mutate(zero_town_hall_county = county %in% zero_town_hall_counties) %>%
    mutate(county = reorder(county, population)) %>%
    ggplot(aes(county, population, fill = zero_town_hall_county)) +
    theme_minimal() +
    xlab(NULL) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(county, population, label = as.character(meetings)), position = position_nudge(y = 7000)) +
    coord_flip() +
    scale_y_continuous(label = comma) +
    scale_fill_manual(values = c("slategray3", "navyblue")) +
    labs(title = "Iowa counties and Grassley public town hall counts by population",
         subtitle = "Counties with zero public town halls since 2011 highlighted",
         y = "Population")

# Total town halls by year plot

townhalls_by_year <- townhall_counts %>%
  group_by(year) %>%
  summarize(meetings = sum(count))

townhalls_by_year_plot <- townhalls_by_year %>%
  ggplot(aes(year, meetings)) +
  geom_col(fill = "slategray3") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  labs(title = "Total Grassley public town hall meetings by year 2011-2018",
       subtitle = "2018 data is year-to-date",
       x = "Year",
       y = "Town halls")
