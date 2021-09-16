library(leaflet)
library(shiny)
library(plotly)

source('map.R')
source('time_series.R')

gif_names <- list("New York, NY" = "nyc", "Los Angeles, CA" = "la", "Chicago, IL" = "chicago", "Houston, TX" = "houston",
                  "Miami, FL" = "miami", "Atlanta, GA" = "atlanta", "Boston, MA" = "boston", "San Francisco, CA" = "sf",
                  "Detroit, MI" = "detroit", "Denver, CO" = "denver")
default_city = "New York, NY"

server <- function(input, output, session) {
  observeEvent(input$jumpToMap, {
    updateTabsetPanel(session, "mainPage", selected = "mapPage")
  })
  output$map <- renderLeaflet({
    us_map
  })
  city <- eventReactive(input$map_shape_click,  {
    x <- input$map_shape_click
    y <- x$id
    y
  }, ignoreNULL = FALSE)
  output$tract_plot <- renderImage({
    c <- city()
    c <- if(is.null(c)) default_city else c
    path <- paste("www/gifs/", gif_names[[c]], "_op.gif", sep = "")
    list(
      src = path,
      contentType = "image/gif",
      width = session$clientData[["output_tract_plot_width"]],
      height = session$clientData[["output_tract_plot_height"]],
      alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  output$median_rent_plot <- renderPlotly({
    c <- city()
    c <- if(is.null(c)) default_city else c
    plot_median_rent(c)
  })
  output$median_rent_growth <- renderPlotly({
    c <- city()
    c <- if(is.null(c)) default_city else c
    plot_rent_growth(c)
  })
  output$rent_burden_city <- renderPlotly({
    c <- city()
    c <- if(is.null(c)) default_city else c
    plot_rent_burden(c)
  })
  output$city_description <- renderText({
    c <- city()
    c <- if(is.null(c)) default_city else c
    path <- paste("www/story/", gif_names[[c]], ".md", sep = "")
    includeMarkdown(path)
  })
  output$comparison_plot <- renderPlotly({
    typ <- input$comparison_plot_type
    if(typ == "rent_burden" || typ == "severe_rent_burden") plot_all_rent_burdens(typ) else plot_all_medians(typ)
  })
  output$download_tract <- downloadHandler(
    filename = function() {
      c <- city()
      c <- if(is.null(c)) default_city else c
      paste(gif_names[[c]], "_op.gif", sep = "")
    },
    content = function(file) {
      c <- city()
      c <- if(is.null(c)) default_city else c
      path <- paste("www/gifs/", gif_names[[c]], "_op.gif", sep = "")
      file.copy(path, file)
    },
    contentType = "image/gif"
  )
}