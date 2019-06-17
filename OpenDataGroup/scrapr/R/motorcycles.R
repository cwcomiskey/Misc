#' Retrieve motorcycle prices
#'
#' \code{zoom} works with \code{collect} to retrieve motorcycle prices
#'
#' @param type right now six types--"2018-honda-xr650l", "2014-husaberg-fe-501",
#' "2018-aprilia-rsv4-rf", "2018-beta-evo-250-factory",
#' "2018-bmw-r-1200-gs-adventure"--are hard coded into \code{scrapr::collect}
#'
#' @return A data frame of one rows (types) and 4 columns:
#' type of motocycle \code{cycle}, \code{price},
#' the url \code{site}, and \code{date}.
#' @export
zoom <- function(type){

  url <- paste0("https://www.motorcyclistonline.com/", type)

  cycle <- xml2::read_html(url) %>%
    rvest::html_nodes("h1.page-title") %>%
    html_text()

  price <- xml2::read_html(url) %>%
    rvest::html_nodes("div.buyers-guide--intro-stats-item") %>%
    html_text() %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    stringr::str_trim() %>%
    .[. != ""] %>%
    grep("\\$", ., value = TRUE)

  return(data.frame(cycle, price, site = url, date = today()))

}

#' Collect motorcycle prices with scrapr::zoom()
#'
#' \code{zoom} works with \code{collect} to retrieve motorcycle prices
#'
#' @return A data frame of five rows (types), 4 columns:
#' type of motocycle \code{cycle}, \code{price},
#' the url \code{site}, and \code{date}.
#' @export
collect <- function(){

  types <- c("2018-honda-xr650l", "2014-husaberg-fe-501",
             "2018-aprilia-rsv4-rf", "2018-beta-evo-250-factory",
             "2018-bmw-r-1200-gs-adventure")

  bikes <- data.frame()

  for(i in types) {
    bikes_i <- zoom(i)
    bikes <- rbind.data.frame(bikes, bikes_i)
  }

  return(bikes)
}
