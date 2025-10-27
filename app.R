#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load necessary libraries


library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(shinythemes)


# Load and combine the data once globally
tryCatch({
  folder_path <- "Tag_Metadata_Files/"
  AC_tag_sheets <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE)
  
  tag_list <- lapply(AC_tag_sheets, function(file) {
    tryCatch({
      data <- read_excel(file, sheet = 2)
      mutate_all(data, as.character)
    }, error = function(e) {
      warning(paste("Error reading file:", file, "-", e$message))
      NULL
    })
  })
  
  AC_tags <- bind_rows(tag_list, .id = "source_file") |> 
    dplyr::select(
      `Sales Order`, Researcher, `Serial No.`, `VUE Tag ID`, 
      `Tag Family`, `Est tag life (days)`, `Ship Date`
    ) |> 
    dplyr::distinct()
  
  if (nrow(AC_tags) == 0) {
    stop("No data was loaded. Please check the 'TagSheets' folder.")
  }
}, error = function(e) {
  stop("Failed to initialize app: ", e$message)
})

str(AC_tags)


# Define the UI
ui <- fluidPage(
  titlePanel("Acoustic Tag ID Metadata Search"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "tag_id",
        label = "Enter Tag ID:",
        placeholder = "Type VUE Tag ID here..."
      ),
      actionButton(
        inputId = "search_button",
        label = "Search"
      )
    ),
    mainPanel(
      tableOutput(outputId = "search_results")
    )
  )
)


# Define the server logic
server <- function(input, output) {
  
  # Observe event to filter data based on input
  observeEvent(input$search_button, {
    req(input$tag_id) # Ensure input is not empty
    
    # Debugging: print the input value
    print(paste("Searching for ID:", input$tag_id))
    
    # Filter results
    search_results <- AC_tags |> 
      dplyr::filter(str_detect(`VUE Tag ID`, input$tag_id)) 
    
    # Debugging: print number of results
    print(paste("Number of results found:", nrow(search_results)))
    
    # Render results
    output$search_results <- renderTable({
      search_results
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server)





