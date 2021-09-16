source('map_creators.R')

cities_with_rent_filename = "www/RData/cities_with_rent.rds"
if(file.exists(cities_with_rent_filename)) {
  cities_with_rent <- readRDS(file = cities_with_rent_filename)
} else {
  cities_with_rent <- create_cities_with_rent()
  saveRDS(cities_with_rent, file = cities_with_rent_filename)
}

us_states_filename = "www/RData/us_states.rds"
if(file.exists(us_states_filename)) {
  us_states <- readRDS(file = us_states_filename)
} else {
  us_states <- create_us_states() # Now I am become George Washington, creator of the states
  saveRDS(us_states, file = us_states_filename)
}

map_filename = "www/RData/map.rds"
if(file.exists(map_filename)) {
  us_map <- readRDS(file = map_filename)
} else {
  us_map <- create_map(cities_with_rent)
  saveRDS(us_map, file = map_filename)
}
