#' Get the current api key stored in your environment
# Prereq: You need to get an API key, and then locate your .Renviron file and add a new line
# cfbd_staturdays_key = "mysecretkey"
#' @export
my_key <- function(){
  Sys.getenv("cfbd_staturdays_key")
}


#' Make a direct call to the cfbd api
#' @description Directly call an API endpoint providing the full url and your key. Note, you
#' usually don't need to do this as the get_anything() function should be able to
#' call almost any endpoint for you.
#' @param url character - Full url endpoint you want to access.
#'
#' @param key character - Your api key
#'
#' @returns a tibble
#'
#' @export
cfbd_api <- function(url, key){

  # Error handling
  if(my_key() == ""){
    stop("Key is blank. Try setting your key first with set_cfb_api_key()")
  }

  data <- httr::GET(url = url,
                    httr::content_type_json(),
                    httr::add_headers('Authorization' = paste('Bearer', key)))

  data <- httr::content(data, as="text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    dplyr::tibble()

}

#' Set your personal collegefootballdata.com key to your environment
#' @description Takes your input and assigns it to the "cfbd_staturdays_key" key
#' in your .Renviron profile.
#' Prereq: You need to get an API key, and then locate your .Renviron file and add a new line
#' cfbd_staturdays_key = "mysecretkey"
#' @param key character. Your personal API key. If you don't have a key, you can register for one for
#' free at https://collegefootballdata.com/key
#' @details This will create a new key in your .Renviron file with the value you assign to it.
#' You will then access that key using my_key() which will be created for you when calling
#' most "get" functions. You can also access your key at any time using
#' Sys.getenv("cfbd_staturdays_key") or change it by using set_cfb_api_key() or directly using
#' Sys.setenv("cfbd_staturdays_key" = "yourapikeyhere")
#' @export
set_cfb_api_key <- function(key){

  Sys.setenv("cfbd_staturdays_key" = as.character(key))

}
# Consider using 	with_envvar(), local_envvar(), instead of Sys.setenv() below
