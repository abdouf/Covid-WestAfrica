#############################################################################################################################
# This code retreives raw COVID data (cases + testing + vaccination) published by institutions for TOGO
# Calculate key metrics to be displayed on the dashbord 
#############################################################################################################################
# create the server functions for the dashboard 
source("utils.R") # load packages
reload_source()

data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data_tg <- data[data$Country.Region=="Togo",]
total_positive = as.numeric(data_tg[5:length(data_tg)])
data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data_tg <- data[data$Country.Region=="Togo",]
total_death = as.numeric(data_tg[5:length(data_tg)])
data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
data_tg <- data[data$Country.Region=="Togo",]
total_recovery = as.numeric(data_tg[5:length(data_tg)])

Dates = seq(as.Date("2020-01-22"),(as.Date("2020-01-22")+length(total_positive)-1),"day")
df_tg = data.frame(Dates,total_positive,total_death,total_recovery)
date_case_reported_tg = df_tg$Dates[nrow(df_tg)] # Get the date of the last reported data
# We are going to extend the data to today
l1 = as.numeric(as.Date(Sys.Date()) - as.Date(df_tg$Dates[nrow(df_tg)]))
l2 = df_tg$Dates[nrow(df_tg)]+1:l1
more_df = data.frame(matrix(nrow = l1, ncol = ncol(df_tg)))
names(more_df) <- names(df_tg)
if(l1>0){
  df_tg = bind_rows(df_tg,more_df)
  df_tg[is.na(df_tg[,1]),1] = as.Date(l2)
}
# Interpolation for positive data
if (any(!is.na(df_tg$total_positive))) {
  df_tg <- df_tg %>%
    # From the cumulative tests with missing data we can use interpolation to 
    # find data
    mutate(total_interpolated_positive = na_interpolation(total_positive, option = "linear")) %>%
    # Calculate daily interpolated tests
    mutate(daily_interpolated_positive = if_else(Dates == (lag(Dates, 1) + 1), total_interpolated_positive - lag(total_interpolated_positive, 1), NA_real_))
} else {
  df_tg <- df_tg %>%
    mutate(total_interpolated_positive = total_positive,
           daily_interpolated_positive = if_else(Dates == (lag(Dates, 1) + 1), total_positive - lag(total_positive, 1), NA_real_))
}
# Interpolation for death data
if (any(!is.na(df_tg$total_death))) {
  df_tg <- df_tg %>%
    # From the cumulative tests with missing data we can use interpolation to 
    # find data
    mutate(total_interpolated_death = na_interpolation(total_death, option = "linear")) %>%
    # Calculate daily interpolated tests
    mutate(daily_interpolated_death = if_else(Dates == (lag(Dates, 1) + 1), total_interpolated_death - lag(total_interpolated_death, 1), NA_real_))
} else {
  df_tg <- df_tg %>%
    mutate(total_interpolated_death = total_death,
           daily_interpolated_death = if_else(Dates == (lag(Dates, 1) + 1), total_death - lag(total_death, 1), NA_real_))
}
# Interpolation for recovery data
if (any(!is.na(df_tg$total_recovery))) {
  df_tg <- df_tg %>%
    # From the cumulative tests with missing data we can use interpolation to 
    # find data
    mutate(total_interpolated_recovery = na_interpolation(total_recovery, option = "linear")) %>%
    # Calculate daily interpolated tests
    mutate(daily_interpolated_recovery= if_else(Dates == (lag(Dates, 1) + 1), total_interpolated_recovery - lag(total_interpolated_recovery, 1), NA_real_))
} else {
  df_tg <- df_tg %>%
    mutate(total_interpolated_recovery = total_recovery,
           daily_interpolated_recovery = if_else(Dates == (lag(Dates, 1) + 1), total_recovery - lag(total_recovery, 1), NA_real_))
}
# Calculate 7 day rolling average
df_tg <- df_tg %>%
  mutate(daily_smoothed_positive = round(frollmean(daily_interpolated_positive, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_positive = if_else(daily_smoothed_positive >= 0, daily_smoothed_positive, NA_real_)) %>%
  # Replace any negative by NA
  mutate(daily_smoothed_death = round(frollmean(daily_interpolated_death, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_death = if_else(daily_smoothed_death >= 0, daily_smoothed_death, NA_real_)) %>%
  mutate(daily_smoothed_recovery = round(frollmean(daily_interpolated_recovery, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_recovery = if_else(daily_smoothed_recovery >= 0, daily_smoothed_recovery, NA_real_)) %>%
  mutate(interpolated_active = ifelse((!is.na(total_interpolated_positive) & !is.na(total_interpolated_death) & !is.na(total_interpolated_recovery)),total_interpolated_positive - (total_interpolated_death+total_interpolated_recovery),NA_real_))

data = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")

data_tg <- data[data$ISO.code=="TGO",c("Date","Daily.change.in.cumulative.total","Cumulative.total")]
colnames(data_tg)=c("Dates","daily_tests","total_tests")
date_test_reported_tg = data_tg$Dates[nrow(data_tg)] # Get the date of the last reported data
# Get the data to today
l1 = as.numeric(as.Date(Sys.Date()) - as.Date(data_tg$Dates[nrow(data_tg)]))
l2 = as.Date(data_tg$Dates[nrow(data_tg)])+1:l1
n = nrow(data_tg)
more_df = data.frame(matrix(nrow = l1, ncol = ncol(data_tg)))
names(more_df) <- names(data_tg)
if(l1>0){
  data_tg = bind_rows(data_tg,more_df)
  data_tg$Dates[which(is.na(data_tg$Dates))] = as.character(l2)
}
# Avoid extrapolation over more than 3 days.
if(l1>3){
  data_tg = data_tg[1:(n+3),]
}
# Do interpolation for test data
# There are missing data, let's find them
if (any(!is.na(data_tg$total_tests))) {
  data_tg <- data_tg %>%
    mutate(total_interpolated_tests = na_interpolation(total_tests, option = "linear")) %>%
    mutate(daily_interpolated_tests = total_interpolated_tests - lag(total_interpolated_tests, 1))
} else {
  data_tg <- data_tg %>%
    mutate(total_interpolated_tests = total_tests,
           daily_interpolated_tests = daily_tests)
}
# Calculate 7 day rolling average
data_tg <- data_tg %>%
  mutate(daily_smoothed_tests = round(frollmean(daily_interpolated_tests, n = 7, align = "right"))) %>%
  mutate(daily_smoothed_tests = if_else(daily_smoothed_tests >= 0, daily_smoothed_tests, NA_real_))
# Merge test and case data
df_tg = data.frame(df_tg,daily_tests = NA_real_,total_tests = NA_real_, total_interpolated_tests = NA_real_, daily_interpolated_tests = NA_real_, daily_smoothed_tests = NA_real_)
df_tg[df_tg$Dates %in% as.Date(data_tg$Dates),c("daily_tests","total_tests","total_interpolated_tests","daily_interpolated_tests","daily_smoothed_tests")] = data_tg[,c("daily_tests","total_tests","total_interpolated_tests","daily_interpolated_tests","daily_smoothed_tests")]
df = df_tg[which(df_tg$Dates<=date_case_reported_tg),] # Limite the data to the current reported
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
R_tg = data.frame(Dates=df$Dates[(Start[1]+1):nrow(df)],
                  meanR=EStR$R$`Mean(R)`,lci=EStR$R$`Quantile.0.05(R)`,
                  uci=EStR$R$`Quantile.0.95(R)`)

melt_tg <- data.frame(Dates = rep(df$Dates,3), 
                      Casedata = c(df$total_interpolated_death,df$total_interpolated_recovery,df$interpolated_active),
                      Casetype = c(rep("Deaths",nrow(df)),rep("Recovered",nrow(df)),rep("Active",nrow(df))))
# Type of vaccines being administred in Togo
data = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")
data_tg <- data[data$iso_code=="TGO",c("last_observation_date","vaccines")]
vaccines_tg <- data_tg$vaccines # The vaccines
date_vaccines_tg <- data_tg$last_observation_date # as of
# Vaccine data
vaccinated_tg = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/country_data/Togo.csv")
dates <- seq(as.Date(vaccinated_tg$date[1]),as.Date(vaccinated_tg$date[nrow(vaccinated_tg)]),'day')
# Population data
data = read.csv("https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Population%20by%20country%2C%201800%20to%202100%20(Gapminder%20%26%20UN)/Population%20by%20country%2C%201800%20to%202100%20(Gapminder%20%26%20UN).csv")
population_tg = data[data$Entity == "Togo" & data$Year==2021,3]
data_vaccinated_tg = as.data.frame(dates) %>%
  mutate(population = population_tg,
         total_vaccinated = NA_real_,
         fully_vaccinated = NA_real_)
data_vaccinated_tg[data_vaccinated_tg$dates %in% as.Date(vaccinated_tg$date),"total_vaccinated"] = vaccinated_tg$people_vaccinated
data_vaccinated_tg[data_vaccinated_tg$dates %in% as.Date(vaccinated_tg$date),"fully_vaccinated"] = vaccinated_tg$people_fully_vaccinated
data_vaccinated_tg = data_vaccinated_tg %>%
  mutate(fraction_vaccinated = 100*total_vaccinated/population,
         fraction_fully_vaccinated = 100*fully_vaccinated/population) %>%
  filter(!is.na(total_vaccinated))
data_vaccinated_tg[is.na(data_vaccinated_tg$fully_vaccinated),"fully_vaccinated"] = 0
data_vaccinated_tg[is.na(data_vaccinated_tg$fraction_fully_vaccinated),"fraction_fully_vaccinated"] = 0

