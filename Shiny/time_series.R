source('time_series_creators.R')
library(ggplot2)
library(plotly)

metro_time_series_filename = "www/RData/metro_ts.rds"
if(file.exists(metro_time_series_filename)) {
  metro_ts <- readRDS(file = metro_time_series_filename)
} else {
  metro_ts <- create_metro_time_series()
  saveRDS(metro_ts, file = metro_time_series_filename)
}

rent_burden_time_series_filename = "www/RData/rent_burden_ts.rds"
if(file.exists(rent_burden_time_series_filename)) {
  rent_burden_ts <- readRDS(file = rent_burden_time_series_filename)
} else {
  rent_burden_ts <- create_rent_burden_time_series()
  saveRDS(rent_burden_ts, file = rent_burden_time_series_filename)
}


plot_median_rent <- function(city) {
  d <- filter(metro_ts, full_name == city)
  tooltip <- paste("<b>Date:</b>", format(d$date, "%Y-%m"), "\n",
                   paste("<b>Median Rent</b>: $", d$median_rent, sep = ""))
  tooltip2 <- paste("<b>Date:</b>", format(d$date, "%Y-%m"), "\n",
                   sprintf("<b>Adjusted Rent</b>: $%.0f", d$adjusted_rent))
  p <- ggplot(data = d, aes(x = date, y = median_rent, group = 1)) +
    geom_line(aes(x = date, y = median_rent, color = "Median Rent", text = tooltip), linetype = 3) +
    geom_line(aes(x = date, y = adjusted_rent, color = "Inflation Adjusted Rent", text = tooltip2)) +
    scale_color_manual(values=c("#2d2d2d", "#919191")) +
    theme_minimal() +
    xlab("Date") +
    ylab("Median Rent") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme(legend.title = element_blank())
  return(ggplotly(p, tooltip = c("text")) %>% layout(legend = list(orientation = "h")))
}

plot_rent_growth <- function(city) {
  dat <- metro_ts %>%
    filter(full_name == city)
  dat$growth <- with(dat, ave(median_rent,
                              FUN=function(val) c(NA, diff(val)/val[-length(val)]) ))
  dat <- dat %>% mutate(did_grow = growth > 0)

  tooltip <- paste("<b>Date:</b>", format(dat$date, "%Y-%m"), "\n",
                   sprintf("<b>Growth</b>: %0.2f%%", dat$growth * 100))

  p <- ggplot(dat, aes(x=date, y=growth, text = tooltip)) +
    geom_segment(aes(x=date, xend=date, y=0, yend=growth, color=did_grow), size=1.3, alpha=0.9) +
    theme_light() +
    theme(
      legend.position = "none",
      panel.border = element_blank()
    ) +
    xlab("Date") +
    ylab("Percent Change in Median Rent") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  return(ggplotly(p, tooltip = c("text")))
}

plot_rent_burden <- function(city) {
  d <- filter(rent_burden_ts, full_name == city)
  tooltip <- paste("<b>Date:</b> ", d$year, "\n",
                   "<b>City:</b> ", city, "\n",
                   sprintf("<b>Rent Burden:</b> %4.2f%%", d$rent_burden * 100), sep = "")
  tooltip2 <- paste("<b>Date:</b> ", d$year, "\n",
                   "<b>City:</b> ", city, "\n",
                   sprintf("<b>Severe Rent Burden:</b> %4.2f%%", d$severe_rent_burden * 100), sep = "")
  p <- ggplot(d, aes(x = year, y = rent_burden, group = 1)) +
    geom_line(aes(x = year, y = rent_burden, color = "Burdened", text = tooltip)) +
    geom_line(aes(x = year, y = severe_rent_burden, color = "Severe", text = tooltip2)) +
    theme_minimal() +
    scale_color_manual(values=c("black", "red")) +
    xlab("Year") +
    ylab("") +
    scale_x_continuous(breaks = 2009:2016) +
    ggtitle("Proportion of rent burdened households") +
    theme(legend.title = element_blank())
  return(ggplotly(p, tooltip = c("text"))  %>% layout(legend = list(x = 0.8, y = 0.5)))
}

plot_all_medians <- function(type) {
  type_names = list("median_rent" = "Median Rent", "adjusted_rent" = "Median Rent (adjusted for inflation)")
  tooltip <- paste("<b>Date:</b>", format(metro_ts$date, "%Y-%m"), "\n",
                   sprintf("<b>%s:</b> $%d", type_names[[type]], metro_ts[[type]]), "\n",
                   "<b>City:</b>", metro_ts$full_name)
  p <- ggplot(metro_ts, aes(x = date, y = metro_ts[[type]], col = full_name, group = 1, text = tooltip)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
    theme_minimal() +
    xlab("Date") +
    ylab("Rent Price in USD") +
    theme(legend.title = element_blank())
  return(ggplotly(p, tooltip = c("text")))
}

plot_all_rent_burdens <- function(type) {
  type_names = list("rent_burden" = "Rent Burden", "severe_rent_burden" = "Severe Rent Burden")
  tooltip <- paste("<b>Date:</b> ", rent_burden_ts$year, "\n",
                   "<b>City:</b> ", rent_burden_ts$full_name, "\n",
                   sprintf("<b>%s:</b> %4.2f%%", type_names[[type]], rent_burden_ts[[type]] * 100), sep = "")
  p <- ggplot(rent_burden_ts, aes(x = year, y = rent_burden_ts[[type]], col = full_name, group = 1, text = tooltip)) +
    geom_line() +
    theme_minimal() +
    xlab("Year") +
    ylab("") +
    scale_x_continuous(breaks = 2009:2016) +
    ggtitle("Proportion of rent burdened households") +
    theme(legend.title = element_blank())
  return(ggplotly(p, tooltip = c("text")))
}
