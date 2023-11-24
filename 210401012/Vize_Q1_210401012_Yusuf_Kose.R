library(httr)
spotify_token <- function() {
  token_url <- "https://accounts.spotify.com/api/token"
  client_id <- Sys.getenv("SPOTIFY_ID")
  client_secret <- Sys.getenv("SPOTIFY_SECRET")
  
  if (client_id == "" || client_secret == "") {
    stop("SPOTIFY_ID ve SPOTIFY_SECRET environment variable'lar??n?? ayarlay??n.")
  }
  body <- list(
    grant_type = "client_credentials",
    client_id = client_id,
    client_secret = client_secret
  )
  response <- POST(
    url = token_url,
    body = body,
    encode = "form",
    add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )
  status_code <- status_code(response)
  token <- content(response)$access_token
  bearer_token <- paste("Bearer", token)
  result <- list(
    status_code = status_code,
    token = bearer_token
  )
  return(result)
}
spotify_token()