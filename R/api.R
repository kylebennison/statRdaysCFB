#' Set key as environment variable and make call to the cfbd api
# Prereq: You need to get an API key, and then locate your .Renviron file and add a new line
# cfbd_staturdays_key = "mysecretkey"
#' @export
my_key <- Sys.getenv("cfbd_staturdays_key")

cfbd_api <- function(url, key){
  
  data <- httr::GET(url = url, 
                    httr::content_type_json(),
                    httr::add_headers('Authorization' = paste('Bearer', key)))
  
  data <- httr::content(data, as="text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE) %>% 
    dplyr::tibble()
  
}
