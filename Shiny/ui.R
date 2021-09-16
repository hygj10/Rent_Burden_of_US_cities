library(leaflet)
library(shiny)
library(plotly)

navbarPage("Rent Burden", id = "mainPage", theme = "styles.css",
  tabPanel("Story", value = "storyPage",
    div(class = "centered-div", align = "center",
      verticalLayout(
        div(class = "guppy",
          h1("Exploring Rent Burden in US Metropolitan Areas")
        ),
        div(class = "aveny",
          includeMarkdown("www/story/description.md")
        ),
        actionButton('jumpToMap', 'Show me')
      )
    )
  ),
  tabPanel("Map", value = "mapPage",
    fluidPage(
      fluidRow(
        column(8,
          div(class = "leaflet-wrapper",
            leafletOutput("map", height="100%", width = "100%")
          )
        ),
        column(4,
          fluidPage(
            fluidRow(
              column(12,
                div(class = "aveny",
                  uiOutput("city_description")
                )
              )
            ),
            fluidRow(
              column(12,
                tabsetPanel(
                  tabPanel("Median Rent",
                    plotlyOutput("median_rent_plot")
                  ),
                  tabPanel("Median Rent Growth",
                    plotlyOutput("median_rent_growth")
                  ),
                  tabPanel("Rent Burden (Census)",
                    imageOutput("tract_plot"),
                    downloadButton("download_tract", label = "Download")
                  ),
                  tabPanel("Rent Burden (City)",
                    plotlyOutput("rent_burden_city")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  tabPanel("Comparisons", value = "comparisonsPage",
    tags$head(
      includeCSS("www/styles.css")
    ),
    verticalLayout(
      div(class = "centered-radio",
        radioButtons("comparison_plot_type", NULL, inline = T,
          c("Median Rent" = "median_rent",
            "Median Rent (adjusted for inflation)" = "adjusted_rent",
            "Rent Burden" = "rent_burden",
            "Severe Rent Burden" = "severe_rent_burden"
          )
        )
      ),
      plotlyOutput("comparison_plot")
    )
  ),
  tabPanel("About", value = "aboutPage",
    tags$head(
      includeCSS("www/styles.css")
    ),
    div(class = "centered-div", align = "center",
      verticalLayout(
        div(class = "guppy",
          h1("About")
        ),
        div(class = "aveny",
          includeMarkdown("www/story/about.md")
        )
      )
    )
  )
)
