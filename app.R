library(shiny)
library(base64enc)

source("camera_utils.R")

ui <- fluidPage(
  titlePanel("Murphy Camera Thumbnail Viewer"),
  actionButton("refresh", "Capture New Thumbnail"),
  br(), br(),
  uiOutput("image_output")
)

server <- function(input, output) {
  observeEvent(input$refresh, {
    creds <- fromJSON("murphy_camera_creds.json")
    auth_info <- get_new_auth_token_and_tier(creds)
    
    # Create thumbnail
    status <- create_new_thumbnail(creds, auth_info)
    
    if (status == 200) {
      # Generate timestamped filename
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0("thumbnail_", ts, ".jpg")
      
      image_path <- get_new_thumbnail(creds, auth_info, filename)
      
      if (!is.null(image_path)) {
        output$image_output <- renderUI({
          tagList(
            tags$p(paste("Captured at:", ts)),
            tags$img(src = base64enc::dataURI(file = image_path, mime = "image/jpeg"),
                     width = "100%")
          )
        })
      } else {
        output$image_output <- renderUI({
          tags$p("Failed to retrieve thumbnail.")
        })
      }
    }
  })
}

shinyApp(ui, server)
