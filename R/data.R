#' Climate Indices Dataset
#' 
#' @title Climate Indices Dataset
#' @description A comprehensive time series dataset containing various climate indices
#' used for pattern causality analysis. This dataset includes multiple climate 
#' indicators measured over time.
#'
#' @source \url{https://www.cpc.ncep.noaa.gov/}
#' @format A data frame with 100 rows and 5 columns:
#' \describe{
#'   \item{Date}{Date; Date of the measurement}
#'   \item{AO}{Numeric; Arctic Oscillation index}
#'   \item{AAO}{Numeric; Antarctic Oscillation index}
#'   \item{NAO}{Numeric; North Atlantic Oscillation index}
#'   \item{PNA}{Numeric; Pacific/North American index}
#' }
#'
#' @examples
#' data(climate_indices)
#' head(climate_indices)
#' summary(climate_indices)
"climate_indices"

#' Dow Jones Stock Price Dataset
#' 
#' @title Dow Jones Stock Price Dataset
#' @description A comprehensive dataset containing daily stock prices for 29 companies
#' listed in the Dow Jones Industrial Average (DJIA). The dataset includes opening,
#' closing, high, and low prices for each stock.
#'
#' @source Yahoo Finance
#' @format A data frame with daily stock prices for 29 companies.
#'
#' @examples
#' data(DJS)
#' head(DJS)
#' summary(DJS)
"DJS"

#' Illapel Ecological Dataset
#' 
#' @title Illapel Ecological Dataset
#' @description Raw rodent and rainfall data collected from the Las Chinchillas 
#' National Reserve near Illapel, Coquimbo Region of Chile. This dataset provides
#' ecological time series for studying species interactions and environmental effects.
#'
#' @source Las Chinchillas National Reserve Research Station
#' @format A data frame with rodent and rainfall data.
#'
#' @examples
#' data(AUCO)
#' head(AUCO)
#' summary(AUCO)
"AUCO"
