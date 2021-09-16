library(tidyverse)

create_metro_time_series <- function() {
  all_metro <- read.csv("../data/Metro_MedianRentalPrice_AllHomes.csv", stringsAsFactors = FALSE)
  specific <- c("New York, NY", "Los Angeles, CA", "Chicago, IL", "Dallas, TX", "Philadelphia, PA", "Houston, TX",
                "Washington, DC", "Miami, FL", "Atlanta, GA", "San Francisco, CA", "Boston, MA", "Detroit, MI",
                "Phoenix, AZ", "Seattle, WA", "Minneapolis, MN", "Austin, TX", "San Jose, CA", "Denver, CO")
  metro <- all_metro[all_metro$RegionName %in% specific,]

  # transpose
  tmetro <- transpose(metro)

  # get row and colnames in order
  colnames(tmetro) <- rownames(metro)
  rownames(tmetro) <- colnames(metro)

  setDT(tmetro, keep.rownames = TRUE)[]
  colnames(tmetro)[1] <- "date"

  names(tmetro) <- as.matrix(tmetro[1, ])
  tmetro <- tmetro[-1, ]
  tmetro[] <- lapply(tmetro, function(x) type.convert(as.character(x)))

  tmetro <- tail(tmetro,-1)
  tmetro$RegionName <- substr(tmetro$RegionName,2,8)
  tmetro$RegionName <- paste(paste(substr(tmetro$RegionName,1,4), "-"),substr(tmetro$RegionName,6,7) )
  cpi <- c(218.1, 224.9, 229.6, 233.0, 236.7, 237.0, 240.0, 244.7, 249.554)
  metro_ts <- melt(tmetro, id.vars="RegionName") %>%
    dplyr::rename(full_name = variable, median_rent = value, date = RegionName) %>%
    mutate(date_dt = gsub(" ", "", date, fixed = TRUE)) %>%
    mutate(date_dt = paste(date_dt, "-01", sep = "")) %>%
    mutate(date_dt = as.Date(date_dt, "%Y-%m-%d")) %>%
    dplyr::select(-c(date)) %>%
    mutate(adjusted_rent = median_rent * cpi[1]/cpi[((year(date_dt) %% 2010) + 1)]) %>%
    dplyr::rename(date = date_dt)

  # TODO: remove this once all cities are in place
  metro_ts <- filter(metro_ts, full_name %in% c("Atlanta, GA", "Boston, MA", "Chicago, IL", "Denver, CO", "Detroit, MI",
                                                "Houston, TX", "Los Angeles, CA", "Miami, FL", "New York, NY", "San Francisco, CA"))
  return(metro_ts)
}

create_rent_burden_time_series <- function() {
  city_names <- c("Atlanta, GA", "Boston, MA", "Chicago, IL", "Denver, CO", "Detroit, MI",
                  "Houston, TX", "Los Angeles, CA", "Miami, FL", "New York, NY", "San Francisco, CA")
  city_to_folder <- c("Atlanta, GA" = "Atlanta", "Boston, MA" = "Boston", "Chicago, IL" = "Chicago", "Denver, CO" = "Denver", "Detroit, MI" = "Detroit",
                      "Houston, TX" = "Houston", "Los Angeles, CA" = "LA", "Miami, FL" = "Miami", "New York, NY" = "NewYork", "San Francisco, CA" = "SF")
  thfive <- function(x) {
    e<-as.data.frame(x)
    total <- (e["HD01_VD08",] + e["HD01_VD09",] + e["HD01_VD10",] + e["HD01_VD11",])/e[1,]
    return(total)
  }
  fhfive <- function(x) {
    e<-as.data.frame(x)
    total <- (e["HD01_VD10",]+ e["HD01_VD11",])/e[1,]
    return(total)
  }
  rent_burden_ts <- data.frame(matrix(ncol = 4, nrow = 0), stringsAsFactors = F)
  colnames(rent_burden_ts) <- c("year", "rent_burden", "severe_rent_burden", "full_name")
  for (city in city_names) {
    for (year in c("09", "10", "11", "12", "13", "14", "15", "16")) {
      d <- read.csv(paste("../data/", city_to_folder[[city]], "/", "ACS_", year, "_5YR_B25070_with_ann.csv", sep = ""))[, c(4:25)]
      rent_burden_ts <- rbind(
        rent_burden_ts,
        data.frame(
          year = paste("20", year, sep = ""),
          rent_burden = thfive(colSums(d)),
          severe_rent_burden = fhfive(colSums(d)),
          full_name = city
        )
      )
    }
  }
  rent_burden_ts$year <- as.numeric(as.character(rent_burden_ts$year))
  rent_burden_ts$full_name <- as.character(rent_burden_ts$full_name)
  return(rent_burden_ts)
}
