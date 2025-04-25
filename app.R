library(shiny)
library(base64enc)
library(jsonlite)
library(googledrive)
library(googleAuthR)
library(lubridate)
library(dplyr)

source("camera_utils.R")  # Your functions

# Authenticate with Service Account using googleAuthR
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/drive")
gar_auth_service("googledrive_creds.json")
drive_auth(token = gar_token())


ui <- fluidPage(
  titlePanel("Murphy Camera Thumbnail Viewer"),
  actionButton("refresh", "Capture New Thumbnail"),
  br(), br(),
  uiOutput("image_output")
)

server <- function(input, output, session) {
  creds <- fromJSON("murphy_camera_creds.json")
  auth_info <- get_new_auth_token_and_tier(creds)
  
  # Set Google Drive folder ID
  GDRIVE_FOLDER_ID <- creds$thumbnail_googledrive_folder_id
  FILENAME_PREFIX <- creds$image_prefix
  
  
  observeEvent(input$refresh, {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Look for the latest thumbnail in Google Drive
    latest_file <- drive_ls(as_id(GDRIVE_FOLDER_ID), pattern = FILENAME_PREFIX) %>%
      drive_reveal("modified_time") %>%
      arrange(desc(modified_time)) %>%
      slice(1)
    
    # If the file is less than 60 seconds old, use it
    if (nrow(latest_file) == 1 &&
        difftime(Sys.time(), ymd_hms(latest_file$modified_time), units = "secs") < 20) {
      
      temp_file <- tempfile(fileext = ".jpg")
      drive_download(as_id(latest_file$id), path = temp_file, overwrite = TRUE)
      
      output$image_output <- renderUI({
        tagList(
          tags$p(paste("Loaded from Google Drive:", latest_file$name)),
          tags$img(src = base64enc::dataURI(file = temp_file, mime = "image/jpeg"),
                   width = "100%")
        )
      })
      
    } else {
      # Generate a new thumbnail if file is older than 60 seconds or doesn't exist
      status <- create_new_thumbnail(creds, auth_info)
      
      if (status == 200) {
        filename <- paste0("thumbnail_", ts, ".jpg")
        Sys.sleep(4)
        image_path <- get_new_thumbnail(creds, auth_info, filename)
        
        if (!is.null(image_path)) {
          # Remove any existing image from Google Drive
          old_files <- drive_ls(as_id(GDRIVE_FOLDER_ID), pattern = FILENAME_PREFIX)
          if (nrow(old_files) > 0) {
            drive_rm(old_files)
          }
          
          # Upload the new image to Google Drive
          drive_upload(media = image_path,
                       path = as_id(GDRIVE_FOLDER_ID),
                       name = basename(image_path),
                       type = "image/jpeg")
          
          # Show the new image
          output$image_output <- renderUI({
            tagList(
              tags$p(paste("Captured and uploaded at:", ts)),
              tags$img(src = base64enc::dataURI(file = image_path, mime = "image/jpeg"),
                       width = "100%")
            )
          })
        } else {
          output$image_output <- renderUI({
            tags$p("Failed to retrieve new thumbnail.")
          })
        }
      }
    }
  })
}

shinyApp(ui, server)
