#' Thermo King data, Anderson-Gill style
#'
#' A dataset containing the time-varying covariates and survival information for Thermo King units. See \code{data-raw/LoadAndProcess.R} for processing code.
#'
#' @format A data frame with 42981 rows and 87 variables; notable variables:
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

#' Thermo King data, 100 random units, Andersen-Gill style
#'
#' A sample of 100 units from \code{thermoking}, the Andersen-Gill formatted package dataset containing the time-varying covariates and survival information for Thermo King units. See \code{data-raw/LoadAndProcess.R} for processing code.
#' #'
#' @format A data frame with 400 rows and 87 variables; notable variables:
#' \describe{
#'   \item{months_in_field}{number of months in the field for the unit.}
#'   \item{serial}{unit identifier.}
#'   \item{time1}{start of time interval; first of A-G triplet.}
#'   \item{time2}{stop of time interval; second of A-G triplet.}
#'   \item{AG_event}{Indicator variable for last full month of survival; third of A-G triplet.}
#'   ...
#' }
#' @source See source code \code{data-raw/LoadAndProcess.R}
"thermoking_sample"

#' Thermo King data, 100 random units
#'
#' A sample of 100 units from the Thermo King data BEFORE modification to Andersen-Gill format; instead of (\code{time1}, \code{time2}, \code{AG_event}) triplet, it contains \code{months_in_field} and \code{event}. See \code{ag.transform()} example for transformation.
#' #'
#' @format A data frame with 500 rows and 85 variables; notable variables:
#' \describe{
#'   \item{months_in_field}{number of months in the field for the unit.}
#'   \item{serial}{unit identifier.}
#'   \item{event}{event indicator for month of event (unit failure); one month later than \code{AG_event}}
#'   ...
#' }
#' @source See source code \code{data-raw/LoadAndProcess.R}
"thermoking_sample_raw"

