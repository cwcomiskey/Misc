#' Transform data to Andersen-Gill format
#'
#' \code{ag.transform} reformats your data frame to the Andersen-Gill style, which includes the triplet (\code{time1}, \code{time2}, \code{event}); and deletes the former \code{event} variable.
#'
#' @param dataset the data frame of interest
#' @param id the variable that uniquely identifies each unit
#' @param age the variable that give the "age" of the unit
#' @param event the event indicator; 0/1 == nonevent/event
#' @return The same data frame with three variables (columns) added: \code{time1},
#'     \code{time2}, \code{AG_event}; and one variable removed: \code{event}.
#'
#' @examples
#' ag_style <- ag.transform(thermoking_sample_raw, serial, months_in_field, event)
#' all.equal(thermoking_sample, ag_style)
#' @export
ag.transform <- function(dataset, id, age, event){

  id <- enquo(id)
  age <- enquo(age)
  event <- enquo(event)

  dataset %>%
    dplyr::arrange(!! id, !! age) %>%
    dplyr::group_by(!! id) %>%
    dplyr::mutate(
      time1 = as.numeric(!! age),
      time2 = as.numeric(lead(!! age)),
      AG_event = lead(!! event)
    ) %>%
    dplyr::filter(is.na(AG_event) == FALSE) %>%
    dplyr::select(-(!! event))
}
