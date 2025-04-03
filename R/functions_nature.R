if (!require("htmltools")) install.packages("htmltools")
if (!require("gh")) install.packages("gh")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("magick")) install.packages("magick")
if (!require("pak")) install.packages("pak")
if (!require("tidytuesdayR")) pak::pak("dslc-io/tidytuesdayR")
##########
## Create the layout for the "Gallery" section ##
##########
# First, check and remove any existing thumbnail files
remove_existing_thumbnails <- function() {
  existing_files <- list.files("gallery/nature")
  thumbnails <- grep("^thumb-", existing_files, value = TRUE)
  
  if (length(thumbnails) > 0) {
    cat("Removing", length(thumbnails), "existing thumbnail files...\n")
    file.remove(file.path("gallery/nature", thumbnails))
  }
}
resize_image <- function(image) {
  # Skip if the image is already a thumbnail
  if (grepl("^thumb-", image)) {
    return(NULL)
  }
  
  imFile <- image_read(here::here(paste0("gallery/nature/", image)))
  imFile_resized <- magick::image_resize(imFile, "10%")
  magick::image_write(imFile_resized, here::here(paste0("gallery/nature/thumb-", image)))
}
# Remove existing thumbnails before creating new ones
remove_existing_thumbnails()
# Get only non-thumbnail images and create new thumbnails
list_png <- list.files("gallery/nature")
list_png_no_thumbs <- grep("^thumb-", list_png, value = TRUE, invert = TRUE)
lapply(list_png_no_thumbs, resize_image)
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
  images_thumb <- grep("^thumb-", images, value = TRUE)
  
  # Make sure we have the same number of thumbnails and full-size images
  if (length(images_thumb) != length(images_full_size)) {
    warning("Mismatch between thumbnail count and full-size image count")
  }
  
  # Create dataframe of image pairs
  image_pairs <- data.frame(
    images_thumb = images_thumb,
    images_full_size = sub("^thumb-", "", images_thumb)
  )
  
  # Sort images alphabetically by filename (A to Z)
  image_pairs <- image_pairs[order(image_pairs$images_full_size, decreasing = FALSE), ]
  
  # CSS for hover effect - will be included in the head of the document
  css <- tags$style(HTML("
    .thumb-container {
      position: relative;
      display: inline-block;
      margin: 5px;
      text-align: center;
      width: 160px;
    }
    
    .magnifier-icon {
      position: absolute;
      bottom: 25px;
      right: 5px;
      background-color: rgba(220,220,220,0.5);
      border-radius: 50%;
      padding: 3px;
      width: 24px;
      height: 24px;
      display: flex;
      justify-content: center;
      align-items: center;
      opacity: 0;
      transition: opacity 0.3s ease;
    }
    
    .thumb-container:hover .magnifier-icon {
      opacity: 1;
    }
  "))
  
  tagList(
    # Add the CSS styles
    css,
    
    # Generate the gallery items
    apply(image_pairs, 1, function(x) {
      # Get original filename for display
      original_name <- x[["images_full_size"]]
      
      # Format the display name
      formatted_name <- format_bird_name(original_name)
      
      tags$a(
        href = paste0("gallery/nature/", x[["images_full_size"]]),
        class = "thumb-container",
        tags$img(
          src = paste0("gallery/nature/", x[["images_thumb"]]),
          style = "width: 150px; height: auto; border: 2px solid #ddd; border-radius: 5px;"
        ),
        # Add magnifier icon that will only show on hover
        tags$div(
          class = "magnifier-icon",
          HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-search"><circle cx="11" cy="11" r="8"></circle><line x1="21" y1="21" x2="16.65" y2="16.65"></line></svg>')
        ),
        tags$div(
          HTML(formatted_name),
          style = "margin-top: 5px; font-size: 12px; max-width: 150px; line-height: 1.1; text-align: center;"
        )
      )
    })
  )
}