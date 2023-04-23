# The following packages are used for data transformation and manipulation
# Install them
reload_source <- function(){
  if (!require('dplyr')) install.packages('dplyr'); library('dplyr') # data manipulation
  if (!require('zoo')) install.packages('zoo'); library('zoo') # Calculate rollmean
  if (!require('EpiEstim')) install.packages('EpiEstim'); library('EpiEstim') # Estimate R
  if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2') # Graph
  if (!require('imputeTS')) install.packages('imputeTS'); library('imputeTS') # Time serie data interpolation
  if (!require('lubridate')) install.packages('lubridate'); library('lubridate') # Dates
  if (!require('data.table')) install.packages('data.table'); library('data.table') # frollmean and data structure
}
