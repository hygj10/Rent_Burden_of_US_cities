library(tidyverse)

create_cities_with_rent <- function() {
  library(googleway)
  cities = c("New York", "San Francisco", "San Jose", "Los Angeles",
             "Chicago", "Detroit", "Atlanta", "Boston", "Miami", "Austin", "Dallas", "Phoenix",
             "Seattle", "Denver", "Philadelphia", "Washington", "Minneapolis", "Houston")
  states = c("NY", "CA", "CA", "CA", "IL", "MI", "GA", "MA", "FL", "TX", "TX",
             "AZ", "WA", "CO", "PA", "DC", "MN", "TX")
  cities <- data.frame(cities,states, stringsAsFactors = FALSE)
  colnames(cities) <- c("name", "state")
  locs <- data.frame(matrix(ncol=2, nrow=0))
  colnames(locs) <- c("lat", "lng")
  for (i in 1:nrow(cities)) {
    locs <- rbind(locs, google_geocode(cities[[i,1]],
                                       key = Sys.getenv("GOOGLE_MAPS_API_KEY"))$results$geometry$location)
  }
  cities <- cbind(cities, locs) %>%
    mutate(full_name = paste(name, state, sep = ", "))
  median_rent_cities <- read_csv("../data/median_rent_burden_cities.csv")
  rent_burdens <- data.frame(matrix(ncol=3, nrow=0))
  colnames(rent_burdens) <- c("metro_name", "rent_burden", "burden_error")
  metro_names = c("New York", "San Francisco", "San Jose", "Los Angeles",
                  "Chicago", "Detroit", "Atlanta", "Boston", "Miami", "Austin", "Dallas", "Phoenix",
                  "Seattle", "Denver", "Philadelphia", "Washington", "Minneapolis", "Houston")
  for (i in 1:length(metro_names)) {
    v <- filter(median_rent_cities, grepl(metro_names[[i]], `GEO.display-label`))
    rent_burdens <- rbind(rent_burdens, data.frame(as.list(c(metro_name = metro_names[[i]], rent_burden = v$HD01_VD01, burden_error = v$HD02_VD01)),
                                                   stringsAsFactors = FALSE))
  }
  rent_burdens$rent_burden <- as.numeric(rent_burdens$rent_burden)
  rent_burdens$burden_error <- as.numeric(rent_burdens$burden_error)
  city_descriptions <- cities[, c("name", "full_name")] %>%
    mutate(description = paste(full_name, "is a great place"))
  cities_with_rent <- cbind(cities, rent_burdens, description = city_descriptions$description, stringsAsFactors = FALSE)
  # TODO: remove this once all cities are in place
  cities_with_rent <- filter(cities_with_rent, name %in% c("Atlanta", "Boston", "Chicago", "Denver", "Detroit", "Houston", "Los Angeles", "Miami", "New York", "San Francisco"))
  return(cities_with_rent)
}

create_us_states <- function() {
  library(geojsonio)
  state_burden <- read_csv("../data/median_rent_burden_states.csv") %>%
    slice(2:n()) %>%
    mutate(name = `GEO.display-label`, rent_burden = as.numeric(HD01_VD01), burden_error = as.numeric(HD02_VD01)) %>%
    select(c("name", "rent_burden", "burden_error"))
  
  shp <- geojson_read("../data/us_states.json", what = "sp", string)
  shp@data <- left_join(shp@data, state_burden, by = "name")
  return(shp)
}

create_map <- function(cities_with_rent) {
  library(leaflet)
  city_popups <- paste("<b>",cities_with_rent$name, "</b><br/>",
                       "<b>Rent Burden</b>: ", cities_with_rent$rent_burden, "±", cities_with_rent$burden_error, "%<br/>")
  us_map <- leaflet(cities_with_rent, options = leafletOptions(minZoom = 4)) %>%
    addTiles(paste0("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png"),
             options = tileOptions(opacity = .7), group = "Dark") %>%
    setMaxBounds(-127.2,50.3,-66.0,24.3) %>%
    addCircles(popup = city_popups, color = "yellow", fillColor = "yellow",
               opacity = 1, fillOpacity = 1, radius = ~((rent_burden * 1) ** 3), group="Circles",
               layerId = ~full_name)
  return(us_map)
}
