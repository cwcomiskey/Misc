#' Thermo King data, Anderson-Gill style
#'
#' A dataset containing the time-varying covariates and survival information for Thermo King units. See \code{data-raw/LoadAndProcess.R} for processing code.
#'
#' @format A data frame with 50310 rows and 87 variables; notable variables:
#' \describe{
#'   \item{months_in_field}{number of months in the field for the unit.}
#'   \item{serial}{unit identifier.}
#'   \item{time1}{start of time interval; first of A-G triplet.}
#'   \item{time2}{stop of time interval; second of A-G triplet.}
#'   \item{AG_event}{Indicator variable for last full month of survival; third of A-G triplet.}
#'   ...
#' }
#' @source \url{https://www.dropbox.com/sh/r42xicztooz1vkn/AACYyxfg2IGHeLYMs3oK9la1a?dl=0}
"thermoking"

