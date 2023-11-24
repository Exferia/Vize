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

spotify_search_artist <- function(artist_name) {
  if(!is.character(artist_name)) stop("Artist name must be character type.");
  token <- spotify_token()
  search_url <- paste0(
    "https://api.spotify.com/v1/search?q=", URLencode(artist_name),
    "&type=artist&limit=1"
  )
  
  response <- httr::GET(
    url = search_url,
    add_headers("Authorization" = token[[2]])
  )
  
  search_result <- httr::content(response, type = "application/json")
  status_code <- status_code(response)
  
  search_results <- data.frame(
    artist = search_result$artists$items[[1]]$name, 
    id = search_result$artists$items[[1]]$id
  )
  
  result <- list(
    status_code = status_code,
    search_results = search_results
  )
  
  return(result)
}
