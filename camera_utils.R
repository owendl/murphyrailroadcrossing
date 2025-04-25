library(httr)
library(jsonlite)

# Load credentials
creds <- fromJSON("murphy_camera_creds.json")

# Get new auth token and tier
get_new_auth_token_and_tier <- function(creds) {
  url <- "https://rest-prod.immedia-semi.com/api/v5/account/login"
  body <- list(
    unique_id = creds$unique_id,
    password = creds$password,
    email = creds$email,
    reauth = "true"
  )
  response <- POST(url, body = toJSON(body, auto_unbox = TRUE),
                   add_headers(`Content-Type` = "application/json"))
  content <- content(response, as = "parsed")
  list(token = content$auth$token, tier = content$account$tier)
}

# Create new thumbnail
create_new_thumbnail <- function(creds, auth_token) {
  url <- sprintf("https://rest-%s.immedia-semi.com/network/%s/camera/%s/thumbnail",
                 auth_token$tier, creds$network_id, creds$camera_id)
  print(url)
  response <- POST(url, add_headers(`token-auth` = auth_token$token))
  status_code(response)
}

# Get new thumbnail
get_new_thumbnail <- function(creds, auth_token, filename) {
  url <- sprintf("https://rest-%s.immedia-semi.com/api/v3/media/accounts/%s/networks/%s/sedona/%s/thumbnail/thumbnail.jpg",
                 auth_token$tier, creds$account_id, creds$network_id, creds$camera_id)
  response <- GET(url, add_headers(`token-auth` = auth_token$token))
  if (status_code(response) == 200) {
    writeBin(content(response, "raw"), filename)
    return(filename)
  } else {
    cat("Failed to download image. Status code:", status_code(response), "\n")
  }
}
