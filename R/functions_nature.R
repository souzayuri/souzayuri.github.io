if (!require("htmltools")) install.packages("htmltools")
if (!require("gh")) install.packages("gh")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("magick")) install.packages("magick")
if (!require("pak")) install.packages("pak")
if (!require("tidytuesdayR")) pak::pak("dslc-io/tidytuesdayR")
##########
## Create the layout for the "Gallery" section ##
##########

resize_image <- function(image) {
  imFile <- image_read(here::here(paste0("gallery/nature/", image)))
  imFile_resized <- magick::image_resize(imFile, "10%")
  magick::image_write(imFile_resized, here::here(paste0("gallery/nature/thumb-", image)))
}

list_png <- list.files("gallery/nature")
lapply(list_png, resize_image)

format_bird_name <- function(filename) {
  # Remove file extension
  name_without_ext <- tools::file_path_sans_ext(filename)
  
  # Extract the name pattern: Name_with_underscores (Scientific_Name)
  pattern <- "^(.+) \\((.+)\\)$"
  name_with_underscores <- gsub(pattern, "\\1", name_without_ext)
  scientific_name <- gsub(pattern, "\\2", name_without_ext)
  
  # Split the name by underscores
  name_parts <- strsplit(name_with_underscores, "_")[[1]]
  
  # Remove underscores from scientific name
  scientific_name <- gsub("_", " ", scientific_name)
  
  # If there are multiple parts, join them with line breaks
  if (length(name_parts) > 1) {
    formatted_names <- paste(name_parts, collapse = "<br style='margin: 0; line-height: 1;'>")
  } else {
    formatted_names <- name_parts
  }
  
  # Return formatted name with line breaks and reduced spacing
  return(paste0(formatted_names, "<br style='margin: 0; line-height: 1;'>(", scientific_name, ")"))
}

make_gallery_layout <- function() {
  images <- list.files("gallery/nature/")
  images_full_size <- grep("thumb", images, value = TRUE, invert = TRUE)
  images_thumb <- grep("thumb", images, value = TRUE)
  
  images <- data.frame(images_thumb = images_thumb,
                       images_full_size = images_full_size)
  
  images[] <- lapply(images, rev)
  
  tagList(
    apply(images, 1, function(x) {
      # Get original filename for display
      original_name <- x[["images_full_size"]]
      
      # Format the display name
      formatted_name <- format_bird_name(original_name)
      
      tags$a(
        href = paste0("gallery/nature/", x[["images_full_size"]]),
        style = "display: inline-block; margin: 5px; text-align: center; width: 160px;", 
        tags$img(
          src = paste0("gallery/nature/", x[["images_thumb"]]),
          style = "width: 150px; height: auto; border: 2px solid #ddd; border-radius: 5px;"
        ),
        tags$div(
          HTML(formatted_name),
          style = "margin-top: 5px; font-size: 12px; max-width: 150px; line-height: 1.1; text-align: center;"
        )
      )
    })
  )
}