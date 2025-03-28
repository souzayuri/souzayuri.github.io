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
  existing_files <- list.files("gallery/img")
  thumbnails <- grep("^thumb-", existing_files, value = TRUE)
  
  if (length(thumbnails) > 0) {
    cat("Removing", length(thumbnails), "existing thumbnail files...\n")
    file.remove(file.path("gallery/img", thumbnails))
  }
}

resize_image <- function(image) {
  # Skip if the image is already a thumbnail
  if (grepl("^thumb-", image)) {
    return(NULL)
  }
  
  imFile <- image_read(here::here(paste0("gallery/img/", image)))
  imFile_resized <- magick::image_resize(imFile, "10%")
  magick::image_write(imFile_resized, here::here(paste0("gallery/img/thumb-", image)))
}

# Remove existing thumbnails before creating new ones
remove_existing_thumbnails()

# Get only non-thumbnail images and create new thumbnails
list_png <- list.files("gallery/img")
list_png_no_thumbs <- grep("^thumb-", list_png, value = TRUE, invert = TRUE)
lapply(list_png_no_thumbs, resize_image)

make_gallery_layout <- function() {
  images <- list.files("gallery/img/")
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

  tagList(
    apply(image_pairs, 1, function(x) {
      tags$a(
        href = paste0("gallery/img/", x[["images_full_size"]]),
        style = "display: inline-block; margin: 5px;", # Ensures side-by-side layout
        tags$img(
          src = paste0("gallery/img/", x[["images_thumb"]]),
          style = "width: 150px; height: auto; border: 2px solid #ddd; border-radius: 5px;"
        )
      )
    })
  )
}