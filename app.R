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
library(writexl)
library(gridExtra)


# Load and combine the data once globally
tryCatch({
  folder_path <- "Tag_Metadata_Files/"
  AC_tag_sheets <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE)
  
  tag_list <- lapply(AC_tag_sheets, function(file) {
    tryCatch({
      data <- read_excel(file, sheet = 2)
      dplyr::mutate_all(data, as.character)
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
    dplyr::mutate(
      Researcher = Researcher |>
        stringr::str_squish() |>
        stringr::str_to_title(),
      tag_suffix = sub("^.*-", "", `VUE Tag ID`)
    ) |> 
    dplyr::distinct()
  
  if (nrow(AC_tags) == 0) {
    stop("No data was loaded. Please check the 'TagSheets' folder.")
  }
}, error = function(e) {
  stop("Failed to initialize app: ", e$message)
})




ui <- fluidPage(
  tags$div(
    style = "text-align: left; margin-top: 20px;",
    tags$img(src = "logo.png", height = "75px")
  ),
  
  titlePanel("Acoustic Tag ID Metadata Search"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "tag_id",
        label = "Enter Tag ID:",
        placeholder = "Type tag ID here..."
      ),
      helpText(
        "Enter the last numeric part of the VUE Tag ID only.",
        tags$br(),
        "That is the part after the second hyphen.",
        tags$br(),
        "You can enter one or several IDs separated by ';'.",
        tags$br(),
        "Example: 615; 721; 900",
        shiny::tags$br(),
        "You can also search by researcher only."
      ),
      selectizeInput(
        inputId = "researcher",
        label = "Select Researcher:",
        choices = c("All", sort(unique(AC_tags$Researcher))),
        selected = "All",
        options = list(placeholder = "Choose a researcher")
      ), 
      actionButton("search_button", "Search"),
      br(), br(),
      downloadButton("download_csv", "Download CSV"),
      br(), br()
    ),
    mainPanel(
      tableOutput("search_results")
    )
  )
)



server <- function(input, output) {
  
  filtered_results <- reactive({
    
    results <- AC_tags
    
    if (!is.null(input$researcher) && input$researcher != "All") {
      results <- results |>
        dplyr::filter(Researcher == input$researcher)
    }
    
    if (!is.null(input$tag_id) && stringr::str_trim(input$tag_id) != "") {
      
      search_terms <- input$tag_id |>
        as.character() |>
        stringr::str_split(";") |>
        purrr::pluck(1) |>
        stringr::str_trim()
      
      search_terms <- search_terms[search_terms != ""]
      
      if (length(search_terms) > 0) {
        results <- results |>
          dplyr::filter(
            purrr::map_lgl(tag_suffix, ~ any(startsWith(.x, search_terms)))
          )
      }
    }
    
    results |>
      dplyr::select(-tag_suffix)
    
  }) |>
    shiny::bindEvent(input$search_button, input$tag_id, input$researcher)
  
  output$search_results <- shiny::renderTable({
    filtered_results()
  })
  
  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("tag_search_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_results(), file)
    }
  )
}

  
  
# Run the app
shinyApp(ui = ui, server = server)





