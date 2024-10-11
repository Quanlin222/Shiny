#' @title Interactive Data Analysis Shiny App
#' @description
#' This is an interactive Shiny application for downloading and displaying KPI (Key Performance Indicator) data.
#' The user can select a year, filter data values based on a threshold, and visualize the filtered data in a table format.
#' Data is retrieved from the Kolada API based on the selected year.
#' 
#' @section Inputs:
#' - `dataset`: A dropdown menu for selecting the year of data (options: 2023, 2022, 2021).
#' - `filterValue`: A dropdown menu for selecting a filter threshold to display values greater than the chosen value.
#' 
#' @section Outputs:
#' - `dataTable`: A table displaying the filtered KPI data, including the KPI code, municipality, period, and value.
#' 
#' @import downloader shiny
#' @importFrom Five getdata_api
#' @examples
#' if (interactive()) {
#'   shiny::runApp()
#' }
#' @export
#' @name shinyApp

library(downloader)
library(shiny)
library(devtools)

# Install and load the Five package before starting the application
if (!require(Five)) {
  devtools::install_github("Quanlin222/Group3_lab5/Five")
}

# Load the Five package
library(Five)

#' @title Shiny UI for the Interactive Data Analysis Application
#' @description
#' This is the UI component of the Shiny app. It contains two inputs: one for selecting the year and another for filtering values greater than a selected threshold. The filtered data is displayed in a table format.
#' 
#' @return Returns the user interface for the Shiny application.
#' @export
#' @name ui  
ui <- fluidPage(
  # Title 
  titlePanel("Interactive Data Analysis"),
  
  # Layout with two sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # menu for selecting year and filter value
      selectInput(inputId = "dataset",
                  label = "Year:",
                  choices = list("2023" = "https://api.kolada.se/v2/data/kpi/N00945/year/2023", 
                                 "2022" = "https://api.kolada.se/v2/data/kpi/N00945/year/2022",
                                 "2021" = "https://api.kolada.se/v2/data/kpi/N00945/year/2021")),
      
      selectInput(inputId = "filterValue",
                  label = "Filter values greater than:",
                  choices = c(100, 130, 140))
    ),
    mainPanel(
      # display the data table
      tableOutput("dataTable")
    )
  )
)

#' @title Shiny Server for the Interactive Data Analysis Application
#' @description
#' This is the server component of the Shiny app. It handles data downloading, filtering, and displaying the data in a table format.
#' 
#' @return A reactive Shiny server function that filters and displays data based on user input.
#' @export
#' @name server  
server <- function(input, output) {
  
  # Create a reactive expression to download data based on the selected year
  dataset <- reactive({
    url <- input$dataset
    filename <- tempfile(fileext = ".json")
    data <- getdata_api(url, filename)
    
    data_frame <- data$values
    
    # removing commas and other non-numeric characters
    data_frame$values <- gsub("[^0-9.]", "", data_frame$values)
    
    # Convert to numeric
    data_frame$values <- suppressWarnings(as.numeric(data_frame$values))
    
    return(data_frame)
  })
  
  # Create a reactive expression to filter the data
  filteredData <- reactive({
    data <- dataset()
    filterValue <- as.numeric(input$filterValue)
    
    filtered <- data[data$values > filterValue, c("kpi", "municipality", "period", "values"), drop = FALSE]
    return(filtered)
  })
  
  # Render the table output
  output$dataTable <- renderTable({
    filteredData()
  })
}

#' @title Complete Shiny Application
#' @description
#' The complete Shiny application combining the `ui` and `server` components.
#' 
#' @export
#' @name shinyApp  
shinyApp(ui = ui, server = server)