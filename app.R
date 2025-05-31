library(shiny)
library(base64enc)
library(jsonlite)
library(googledrive)
library(googleAuthR)
library(lubridate)
library(dplyr)
library(magick)

source("camera_utils.R")  # Your functions

# Authenticate with Service Account using googleAuthR
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/drive")
gar_auth_service("googledrive_creds.json")
drive_auth(token = gar_token())

ui <- fluidPage(
  titlePanel("Murphy Camera Thumbnail Viewer"),
  actionButton("refresh", "Capture New Thumbnail"),
  br(), br(),
  # Add loading spinner div (hidden by default)
  div(id = "loading-spinner", style = "display: none; text-align: center;",
      tags$div(class = "spinner", style = "
        border: 4px solid #f3f3f3;
        border-top: 4px solid #3498db;
        border-radius: 50%;
        width: 40px;
        height: 40px;
        animation: spin 1s linear infinite;
        margin: auto;
      ")
  ),
  # Add CSS for spinner animation
  tags$style(HTML("
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
  ")),
  uiOutput("image_output"),
  # Add buttons for Blocked and Clear Crossing (rendered dynamically)
  uiOutput("action_buttons")
)

server <- function(input, output, session) {
  creds <- fromJSON("murphy_camera_creds.json")
  auth_info <- get_new_auth_token_and_tier(creds)
  
  # Set Google Drive folder IDs
  GDRIVE_FOLDER_ID <- creds$thumbnail_googledrive_folder_id
  BLOCKED_FOLDER_ID <- creds$blocked_folder_id
  CLEAR_FOLDER_ID <- creds$clear_folder_id
  FILENAME_PREFIX <- creds$image_prefix
  
  
  # Look for the latest thumbnail in Google Drive
  latest_file <- drive_ls(as_id(GDRIVE_FOLDER_ID), pattern = FILENAME_PREFIX) %>%
    drive_reveal("modified_time") %>%
    arrange(desc(modified_time)) %>%
    slice(1)
  
  # If the file is less than 60 seconds old, use it
  if (nrow(latest_file) == 1) {
    
    drive_download(as_id(latest_file$id), overwrite = TRUE)
    
    output$image_output <- renderUI({
      tagList(
        tags$p(paste("Loaded from Google Drive:", latest_file$name)),
        tags$img(src = base64enc::dataURI(file = cropped_image_thumbnail(latest_file$name), mime = "image/jpeg"),
                 width = "100%")
      )
    })
    }
  
  # Reactive value to track the current image and button state
  rv <- reactiveValues(
    current_image = NULL,  # Stores path or name of the current image
    buttons_enabled = FALSE  # Controls button visibility/enabled state
  )
  
  observeEvent(input$refresh, {
    # Show loading spinner
    session$sendCustomMessage(type = "showSpinner", message = TRUE)
    
    # Disable buttons during processing
    rv$buttons_enabled <- FALSE
    
    ts <- format(.POSIXct(Sys.time(), "US/Eastern"), "%Y%m%d_%H%M%S")
    
    # Look for the latest thumbnail in Google Drive
    latest_file <- drive_ls(as_id(GDRIVE_FOLDER_ID), pattern = FILENAME_PREFIX) %>%
      drive_reveal("modified_time") %>%
      arrange(desc(modified_time)) %>%
      slice(1)
    
    # If the file is less than 60 seconds old, use it
    if (nrow(latest_file) == 1 &&
        difftime(Sys.time(), ymd_hms(latest_file$modified_time), units = "secs") < 60) {
      
      
      drive_download(as_id(latest_file$id),  overwrite = TRUE)
      
      output$image_output <- renderUI({
        tagList(
          tags$p(paste("Loaded from Google Drive:", latest_file$name)),
          tags$img(src = base64enc::dataURI(file = cropped_image_thumbnail(latest_file$name), mime = "image/jpeg"),
                   width = "100%")
        )
      })
      
      # Update reactive values
      rv$current_image <- list(name = latest_file$name, path = latest_file$name)
      rv$buttons_enabled <- TRUE
      
    } else {
      # Generate a new thumbnail if file is older than 60 seconds or doesn't exist
      status <- create_new_thumbnail(creds, auth_info)
      
      if (status == 200) {
        filename <- paste0("thumbnail_", ts, ".jpg")
        Sys.sleep(4)  # Simulate processing delay
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
              tags$img(src = base64enc::dataURI(file = cropped_image_thumbnail(image_path), mime = "image/jpeg"),
                       width = "100%")
            )
          })
          
          # Update reactive values
          rv$current_image <- list(name = basename(image_path), path = image_path)
          rv$buttons_enabled <- TRUE
        } else {
          output$image_output <- renderUI({
            tags$p("Failed to retrieve new thumbnail.")
          })
          rv$buttons_enabled <- FALSE
        }
      } else {
        output$image_output <- renderUI({
          tags$p("Failed to generate new thumbnail.")
        })
        rv$buttons_enabled <- FALSE
      }
    }
    
    # Hide loading spinner
    session$sendCustomMessage(type = "hideSpinner", message = FALSE)
  })
  
  # Render action buttons (Blocked/Clear Crossing)
  output$action_buttons <- renderUI({
    if (rv$buttons_enabled) {
      tagList(
        actionButton("blocked", "Blocked Crossing", style = "margin-right: 10px;"),
        actionButton("clear", "Clear Crossing")
      )
    } else {
      NULL
    }
  })
  
  # Handle Blocked Crossing button click
  observeEvent(input$blocked, {
    if (!is.null(rv$current_image)) {
      # Check if file already exists in blocked folder
      existing_files <- drive_ls(as_id(BLOCKED_FOLDER_ID), pattern = rv$current_image$name)
      
      if (nrow(existing_files) == 0) {
        # Copy image to blocked folder
        drive_upload(media = rv$current_image$path,
                     path = as_id(BLOCKED_FOLDER_ID),
                     name = rv$current_image$name,
                     type = "image/jpeg")
        
        # Show notification
        showNotification("Image copied to Blocked Crossing folder.", type = "message")
      } else {
        showNotification("Image already exists in Blocked Crossing folder.", type = "warning")
      }
      
      # Disable buttons
      rv$buttons_enabled <- FALSE
    }
  })
  
  # Handle Clear Crossing button click
  observeEvent(input$clear, {
    if (!is.null(rv$current_image)) {
      # Check if file already exists in clear folder
      existing_files <- drive_ls(as_id(CLEAR_FOLDER_ID), pattern = rv$current_image$name)
      
      if (nrow(existing_files) == 0) {
        # Copy image to clear folder
        drive_upload(media = rv$current_image$path,
                     path = as_id(CLEAR_FOLDER_ID),
                     name = rv$current_image$name,
                     type = "image/jpeg")
        
        # Show notification
        showNotification("Image copied to Clear Crossing folder.", type = "message")
      } else {
        showNotification("Image already exists in Clear Crossing folder.", type = "warning")
      }
      
      # Disable buttons
      rv$buttons_enabled <- FALSE
    }
  })
  
  # JavaScript to show/hide spinner
  shiny::observe({
    session$onFlushed(function() {
      session$sendCustomMessage(type = "registerSpinner", message = list())
    }, once = TRUE)
  })
}

# Add custom JavaScript handler
shinyApp(ui = tagList(
  ui,
  tags$script(HTML("
    Shiny.addCustomMessageHandler('showSpinner', function(message) {
      document.getElementById('loading-spinner').style.display = 'block';
    });
    Shiny.addCustomMessageHandler('hideSpinner', function(message) {
      document.getElementById('loading-spinner').style.display = 'none';
    });
    Shiny.addCustomMessageHandler('registerSpinner', function(message) {
      // Ensure spinner is hidden on page load
      document.getElementById('loading-spinner').style.display = 'none';
    });
  "))
), server = server)