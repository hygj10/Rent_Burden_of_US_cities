my_packages = c("leaflet", "plotly", "tidyverse")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

# upgrade the default shiny
install.packages("shiny")

invisible(sapply(my_packages, install_if_missing))
