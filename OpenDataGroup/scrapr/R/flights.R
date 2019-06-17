#' Collect price data flight for one orig-dest route
#'
#' \code{GET_one_route} retrieves flight prices for one route; one direction.
#'
#' @return A huge data frame (tens of thousands) with
#' flight \code{Origin}, destination \code{Dest},
#' \code{Price}, and \code{date}.
#' @export
GET_one_route <- function(orig = "BOS", dest = "DEN"){
  GET("https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search",
      query = list(
        apikey = "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34",
        departure_date = as.character(today()),
        origin = orig,
        destination = dest)) %>%
    content()


}

#' Collect many flight prices from Amadeus API
#'
#' \code{gather} works with \code{GET_one_route} to retrieve flight prices from Amadeus API, for routes to and from the ~40 busiest U.S. airports on the date of retrieval.
#'
#' @return A huge data frame (tens of thousands) with
#' flight \code{Origin}, destination \code{Dest},
#' \code{Price}, and \code{date}.
#' @export
gather <- function(){

  for(i in 1:1035){ # ROUTES

    if(i %% 10 == 0){
      print(i)
      print(dim(dat))
    } # progress reports

    # Create container
    if(i == 1){
      dat <- data.frame("Origin" = NA, "Dest" = NA, "Price" = NA)
    }

    # Retrieve ROUTE i FLIGHTS
    response <-
      GET_one_route(orig = orig_dest[i,"Origin"], dest = orig_dest[i,"Dest"])

    for(j in 1:length(response[["results"]])){ # route i, FLIGHTS j = 1, 2, ...


      if(j == 1){
        dat_j <- data.frame("Origin" = NA, "Dest" = NA, "Price" = NA)
      } # route i_0, flights j container

      dat_j[j,c("Origin", "Dest")] <- orig_dest[i,]

      # price of flight j, route i
      try({
        dat_j[j, "Price"] <-
          response[["results"]][[j]]$fare$price_per_adult$total_fare
      })
    } # FLIGHTS

    try(dat <- rbind.data.frame(dat, dat_j)) # Add prices to big container

    if(i == 1035){
      rm(i, j, dat_j)
      dat <- dat %>%
        drop_na() %>%
        mutate(date = today())
    } # finishing touches
  }
  dat <- dat %>% mutate(Price = as.numeric(Price))

  return(dat)
}



