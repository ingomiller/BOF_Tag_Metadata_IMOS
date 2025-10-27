

library(tidyverse)
library(readxl)
library(stringr)

# Folder with all Tag metadata sheets
folder_path <- "data/Tag_Metadata_Files"

# List all .xls files in the folder
AC_tag_sheets <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE)
print(AC_tag_sheets)


# Initialize an empty list to store data frames
tag_list <- list()

# Loop through each file, read it, and store in the list
for (file in AC_tag_sheets) {
  tryCatch({
    data <- read_excel(file, sheet = 2)
    
    # Convert all columns to character for consistent type
    data <- mutate_all(data, as.character)
   
     tag_list[[file]] <- data
  }, error = function(e) {
    warning(paste("Error reading file:", file, "-", e$message))
  })
}


# Combine all data frames into one
AC_tags <- bind_rows(tag_list, .id = "source_file") 


glimpse(AC_tags)


search_tag <- AC_tags |> 
  dplyr::filter(stringr::str_detect(`VUE Tag ID`, "51407" )) |>
  dplyr::select(`Sales Order`, Researcher, `Serial No.`, `VUE Tag ID`, `Tag Family`, `Est tag life (days)`, `Ship Date`) |> 
  dplyr::distinct()


print(search_tag)

