#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplot", tabName = "boxplot", icon = icon("th")),
      menuItem("Histogram", tabName = "histogram", icon = icon("bar-chart")),
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barchart", tabName = "barchart", icon = icon("th")),
      menuItem("ScatterPlot", tabName = "ScatterPlot", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "boxplot",
              actionButton(inputId = "clicks0",  label = "Click me"),
              plotOutput("distPlot0")
      ),
      
      # Second tab content
      tabItem(tabName = "histogram",
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1")
      ),
      
      # Fourth tab content
      tabItem(tabName = "crosstab",
              sliderInput("KPI4", "KPI_value:", 
                          min = .1, max = .9,  value = .5),
              actionButton(inputId = "clicks4",  label = "Click me"),
              plotOutput("distPlot4")
      ),
      
      # Second tab content
      tabItem(tabName = "barchart",
              actionButton(inputId = "clicks2",  label = "Click me"),
              plotOutput("distPlot2")
      ),
      
      # Third tab content
      tabItem(tabName = "ScatterPlot",
              actionButton(inputId = "clicks3",  label = "Click me"),
              plotOutput("distPlot3")
      )
    )
  )
)
