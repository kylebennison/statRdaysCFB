#' Set key as environment variable and make call to the cfbd api
# Prereq: You need to get an API key, and then locate your .Renviron file and add a new line
# cfbd_staturdays_key = "mysecretkey"
#' @export
my_key <- Sys.getenv("cfbd_staturdays_key")
# TODO Turn this into a function because otherwise this gets called at build
# time, not run time, and will not set the key for the user.
# TODO: Then, turn all references to my_key into my_key() and it will
# call the func and get the key when other functions that use it are called
# Consider using 	with_envvar(), local_envvar(), instead of Sys.setenv() below

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
#' @param key character. Your personal API key. If you don't have a key, you can register for one for
#' free at https://collegefootballdata.com/key
#' @details This will create a new key in your .Renviron file with the value you assign to it.
#' You will then access that key using the variable my_key which will be created for you when calling
#' most "get" functions. You can also access your key at any time using
#' Sys.getenv("cfbd_staturdays_key") or change it by using set_cfb_api_key() or directly using
#' Sys.setenv("cfbd_staturdays_key" = "yourapikeyhere")
#' @export
set_cfb_api_key <- function(key){

  Sys.setenv("cfbd_staturdays_key" = as.character(key))

}
