
if (!require("htmltools")) install.packages("htmltools")
if (!require("gh")) install.packages("gh")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("magick")) install.packages("magick")
if (!require("tidytuesdayR")) pak::pak("dslc-io/tidytuesdayR")



##########
## Create the layout for the "Gallery" section ##
##########


resize_image <- function(image) {
  
  imFile <- image_read(here::here(paste0("gallery/img/", image)))
  imFile_resized <- magick::image_resize(imFile, "6%")
  magick::image_write(imFile_resized, here::here(paste0("gallery/img/thumb-", image)))
  
}

make_gallery_layout <- function() {
  
  images <- list.files("gallery/img")
  images_full_size <- grep("thumb", images, 
                           value = TRUE, invert = TRUE)
  images_thumb <- grep("thumb", images, value = TRUE)
  
  images <- data.frame(images_thumb = images_thumb,
                       images_full_size = images_full_size)
  
  images[] <- lapply(images, rev)
  
  tagList(apply(images, 1, function(x) {
      tags$a(
        href = paste0("gallery/img/", x[["images_full_size"]]),
        tags$img(src = paste0("gallery/img/", x[["images_thumb"]]))
      )
  }))
  
}






