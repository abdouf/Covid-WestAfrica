# All the small pieces
# create the server functions for the dashboard 
# Get all the data
library(dplyr) # data manipulation
library(zoo) # Calculate rollmean
library(EpiEstim) # Estimate R
library(imputeTS) # Time serie data interpolation
library(data.table) # frollmean
library(lubridate)
library(ggplot2)
data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data_gh <- data[data$Country.Region=="Ghana",]
total_positive = as.numeric(data_gh[5:length(data_gh)])
data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data_gh <- data[data$Country.Region=="Ghana",]
total_death = as.numeric(data_gh[5:length(data_gh)])
data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
data_gh <- data[data$Country.Region=="Ghana",]
total_recovery = as.numeric(data_gh[5:length(data_gh)])
#Incidence_positive <- c(positive[1],positive[2:length(positive)]
#                        -positive[1:length(positive)-1])
#Incidence_death <- c(death[1],death[2:length(death)]
#                     -death[1:length(death)-1])
#Incidence_recovery <- c(recovery[1],recovery[2:length(recovery)]
#                        -recovery[1:length(recovery)-1])
Dates = seq(as.Date("2020-01-22"),(as.Date("2020-01-22")+length(total_positive)-1),"day")
df_gh = data.frame(Dates,total_positive,total_death,total_recovery)
date_case_reported_gh = df_gh$Dates[nrow(df_gh)] # Get the date of the last reported data
# We are going to extend the data to today
l1 = as.numeric(as.Date(Sys.Date()) - as.Date(df_gh$Dates[nrow(df_gh)]))
l2 = df_gh$Dates[nrow(df_gh)]+1:l1
more_df = data.frame(matrix(nrow = l1, ncol = ncol(df_gh)))
names(more_df) <- names(df_gh)
if(l1>0){
  df_gh = bind_rows(df_gh,more_df)
  df_gh[is.na(df_gh[,1]),1] = as.Date(l2)
}
# Interpolation for positive data
if (any(!is.na(df_gh$total_positive))) {
  df_gh <- df_gh %>%
    # From the cumulative tests with missing data we can use interpolation to 
    # find data
    mutate(total_interpolated_positive = na_interpolation(total_positive, option = "linear")) %>%
    # Calculate daily interpolated tests
    mutate(daily_interpolated_positive = if_else(Dates == (lag(Dates, 1) + 1), total_interpolated_positive - lag(total_interpolated_positive, 1), NA_real_))
} else {
  df_gh <- df_gh %>%
    mutate(total_interpolated_positive = total_positive,
           daily_interpolated_positive = if_else(Dates == (lag(Dates, 1) + 1), total_positive - lag(total_positive, 1), NA_real_))
}
# Interpolation for death data
if (any(!is.na(df_gh$total_death))) {
  df_gh <- df_gh %>%
    # From the cumulative tests with missing data we can use interpolation to 
    # find data
    mutate(total_interpolated_death = na_interpolation(total_death, option = "linear")) %>%
    # Calculate daily interpolated tests
    mutate(daily_interpolated_death = if_else(Dates == (lag(Dates, 1) + 1), total_interpolated_death - lag(total_interpolated_death, 1), NA_real_))
} else {
  df_gh <- df_gh %>%
    mutate(total_interpolated_death = total_death,
           daily_interpolated_death = if_else(Dates == (lag(Dates, 1) + 1), total_death - lag(total_death, 1), NA_real_))
}
# Interpolation for recovery data
if (any(!is.na(df_gh$total_recovery))) {
  df_gh <- df_gh %>%
    # From the cumulative tests with missing data we can use interpolation to 
    # find data
    mutate(total_interpolated_recovery = na_interpolation(total_recovery, option = "linear")) %>%
    # Calculate daily interpolated tests
    mutate(daily_interpolated_recovery= if_else(Dates == (lag(Dates, 1) + 1), total_interpolated_recovery - lag(total_interpolated_recovery, 1), NA_real_))
} else {
  df_gh <- df_gh %>%
    mutate(total_interpolated_recovery = total_recovery,
           daily_interpolated_recovery = if_else(Dates == (lag(Dates, 1) + 1), total_recovery - lag(total_recovery, 1), NA_real_))
}
# Calculate 7 day rolling average
df_gh <- df_gh %>%
  mutate(daily_smoothed_positive = round(frollmean(daily_interpolated_positive, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_positive = if_else(daily_smoothed_positive >= 0, daily_smoothed_positive, NA_real_)) %>%
  # Replace any negative by NA
  mutate(daily_smoothed_death = round(frollmean(daily_interpolated_death, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_death = if_else(daily_smoothed_death >= 0, daily_smoothed_death, NA_real_)) %>%
  mutate(daily_smoothed_recovery = round(frollmean(daily_interpolated_recovery, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_recovery = if_else(daily_smoothed_recovery >= 0, daily_smoothed_recovery, NA_real_)) %>%
  mutate(interpolated_active = ifelse((!is.na(total_interpolated_positive) & !is.na(total_interpolated_death) & !is.na(total_interpolated_recovery)),total_interpolated_positive - (total_interpolated_death+total_interpolated_recovery),NA_real_))

data = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")
#data_gh <- data[data$ISO.code=="TGO",c("Date","Daily.change.in.cumulative.total","Cumulative.total","Daily.change.in.cumulative.total.per.thousand","Cumulative.total.per.thousand","X7.day.smoothed.daily.change","X7.day.smoothed.daily.change.per.thousand")]
# colnames(data_gh)=c("Dates","DailyTests","CumTests","DailyTestsK","CumTestsK","SmoothedTests","SmoothedTestsK")
data_gh <- data[data$ISO.code=="GHA",c("Date","Daily.change.in.cumulative.total","Cumulative.total")]
colnames(data_gh)=c("Dates","daily_tests","total_tests")
date_test_reported_gh = data_gh$Dates[nrow(data_gh)] # Get the date of the last reported data
# Get the data to today
l1 = as.numeric(as.Date(Sys.Date()) - as.Date(data_gh$Dates[nrow(data_gh)]))
l2 = as.Date(data_gh$Dates[nrow(data_gh)])+1:l1
n = nrow(data_gh)
more_df = data.frame(matrix(nrow = l1, ncol = ncol(data_gh)))
names(more_df) <- names(data_gh)
if(l1>0){
  data_gh = bind_rows(data_gh,more_df)
  data_gh$Dates[which(is.na(data_gh$Dates))] = as.character(l2)
}
# Avoid extrapolation over more than 3 days.
if(l1>3){
  data_gh = data_gh[1:(n+3),]
}
# Do interpolation for test data
# There are missing data, let's find them
if (any(!is.na(data_gh$total_tests))) {
  data_gh <- data_gh %>%
    mutate(total_interpolated_tests = na_interpolation(total_tests, option = "linear")) %>%
    mutate(daily_interpolated_tests = total_interpolated_tests - lag(total_interpolated_tests, 1))
} else {
  data_gh <- data_gh %>%
    mutate(total_interpolated_tests = total_tests,
           daily_interpolated_tests = daily_tests)
}
# Calculate 7 day rolling average
data_gh <- data_gh %>%
  mutate(daily_smoothed_tests = round(frollmean(daily_interpolated_tests, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_tests = if_else(daily_smoothed_tests >= 0, daily_smoothed_tests, NA_real_))
# Merge test and case data
df_gh = data.frame(df_gh,daily_tests = NA_real_,total_tests = NA_real_, total_interpolated_tests = NA_real_, daily_interpolated_tests = NA_real_, daily_smoothed_tests = NA_real_)
df_gh[which(df_gh$Dates==data_gh$Dates[1]):which(df_gh$Dates==data_gh$Dates[nrow(data_gh)]),c(15:19)] = data_gh[,-1]
df = df_gh[which(df_gh$Dates<=date_case_reported_gh),] # Limite the data to the current reported
Inc = df$daily_smoothed_positive
Inc[is.na(Inc)]=0
# Calculate Rt using epiestim
Start <- which(df$total_positive>=15) # When do we have at least 15 cases
t_start<-seq(Start[1],(length(Inc)-1),1)
t_end<-seq((Start[1]+1),length(Inc),1)

# The mean interval was 3.96 days (95% CI 3.53–4.39 days), SD 4.75 days (95% CI 4.46–5.07 days)
# https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
EStR <- estimate_R(incid = Inc, # This is the function from Anne Corie package
                   method = "parametric_si",
                   config = make_config(list(
                     mean_si = 3.96, std_si = 4.75, # Declares SI
                     t_start = t_start, # Timesteps
                     t_end = t_end)))
R_gh = data.frame(Dates=df$Dates[(Start[1]+1):nrow(df)],
                  meanR=EStR$R$`Mean(R)`,lci=EStR$R$`Quantile.0.05(R)`,
                  uci=EStR$R$`Quantile.0.95(R)`)

melt_gh <- data.frame(Dates = rep(df$Dates,3), 
                      Casedata = c(df$total_interpolated_death,df$total_interpolated_recovery,df$interpolated_active),
                      Casetype = c(rep("Deaths",nrow(df)),rep("Recovered",nrow(df)),rep("Active",nrow(df))))

# Type of vaccines being administred in GHANA
data = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")
data_gh <- data[data$iso_code=="GHA",c("last_observation_date","vaccines")]
vaccines_gh <- data_gh$vaccines # The vaccines
date_vaccines_gh <- data_gh$last_observation_date # as of
# Vaccine data
vaccinated_gh = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Ghana.csv")
dates <- seq(as.Date(vaccinated_gh$date[1]),as.Date(vaccinated_gh$date[nrow(vaccinated_gh)]),'day')

# Date is a sequence of dates from first reported vaccination date to the current.
data = read.csv("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Population%20by%20country%2C%201800%20to%202100%20(Gapminder%20%26%20UN)/Population%20by%20country%2C%201800%20to%202100%20(Gapminder%20%26%20UN).csv")
population_gh = data[data$Entity == "Ghana" & data$Year==2021,3]
data_vaccinated_gh = as.data.frame(dates) %>%
  mutate(population = population_gh,
         total_vaccinated = NA_real_,
         fully_vaccinated = NA_real_)
data_vaccinated_gh[data_vaccinated_gh$dates %in% as.Date(vaccinated_gh$date),"total_vaccinated"] = vaccinated_gh$people_vaccinated
data_vaccinated_gh[data_vaccinated_gh$dates %in% as.Date(vaccinated_gh$date),"fully_vaccinated"] = vaccinated_gh$people_fully_vaccinated
data_vaccinated_gh = data_vaccinated_gh %>%
  mutate(fraction_vaccinated = 100*total_vaccinated/population,
         fraction_fully_vaccinated = 100*fully_vaccinated/population) %>%
  filter(!is.na(total_vaccinated))
data_vaccinated_gh[is.na(data_vaccinated_gh$fully_vaccinated),"fully_vaccinated"] = 0
data_vaccinated_gh[is.na(data_vaccinated_gh$fraction_fully_vaccinated),"fraction_fully_vaccinated"] = 0

