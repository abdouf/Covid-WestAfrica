#############################################################################################################################
# This code generates the user interface, the structure of what the user sees in the App
#############################################################################################################################
# The following packages are used for data transformation and manipulation
library(shiny)
library(shinydashboard)
library(rappdirs)
source("covid_data_tg.R")
source("covid_data_gh.R")
source("covid_data_ng.R")
source("server.R")
# Make a dashboard with header, sidebar and body
# Generate the values on the dashbord body
tg_row1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4",width=7)
)
# Generate the first 2 plots on the dashbord body
tg_row2 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("positive_smooth_tg", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("reproduction_smooth_tg", height = "300px")
  ) 
)
# Generate the 2 middle plots
tg_row3 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("death_rec_active_tg", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("death_positive_tg", height = "300px")
  ) 
)
# Generate the last 2 plots
tg_row4 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("total_vaccination_tg", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("fraction_vaccination_tg", height = "300px")
  ) 
)
# Replicate for Ghana
gh_row1 <- fluidRow(
  valueBoxOutput("value5")
  ,valueBoxOutput("value6")
  ,valueBoxOutput("value7")
  ,valueBoxOutput("value8",width=7)
)
# Generate the first 2 plots on the dashbord body
gh_row2 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("positive_smooth_gh", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("reproduction_smooth_gh", height = "300px")
  ) 
)
gh_row3 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("death_rec_active_gh", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("death_positive_gh", height = "300px")
  ) 
)
gh_row4 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("total_vaccination_gh", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("fraction_vaccination_gh", height = "300px")
  ) 
)
# Nigeria
ng_row1 <- fluidRow(
  valueBoxOutput("value9")
  ,valueBoxOutput("value10")
  ,valueBoxOutput("value11")
  ,valueBoxOutput("value12", width=7)
)
ng_row2 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("positive_smooth_ng", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("reproduction_smooth_ng", height = "300px")
  ) 
)
ng_row3 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("death_rec_active_ng", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("death_positive_ng", height = "300px")
  ) 
)
ng_row4 <- fluidRow( 
  box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("total_vaccination_ng", height = "300px")
  )
  ,box(
    title = " "
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("fraction_vaccination_ng", height = "300px")
  ) 
)
# User Interface
# Header is dynamic
# The sidebar is static
ui <- dashboardPage(
  skin='red',
  dashboardHeader(title = "COVID in West Africa"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TOGO", tabName = "TOGO"),
      menuItem("GHANA", tabName = "GHANA"),
      menuItem("NIGERIA", tabName = "NIGERIA"),
      menuItem("About this site", tabName = "About_this_site")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "TOGO",
              tg_row1,tg_row2,tg_row3,tg_row4
      ),
      tabItem(tabName = "GHANA",
              gh_row1,gh_row2,gh_row3,gh_row4
      ),
      tabItem(tabName = "NIGERIA",
              ng_row1,ng_row2,ng_row3,ng_row4
      ),
      tabItem(tabName = "About_this_site",
              HTML(paste("Last updated:", format(today(), format="%B %d %Y")),paste(" "," ",sep="<br/>")),
              HTML(paste("This website is updated daily. The data displayed reflect the latest testing and case reporting data available from data source websites.\n",
                         " ", " ",
                         sep="<br/>")),
              HTML(paste("Testing data are retrieved from: OUR WORLD IN DATA",
                         "Citation: Max Roser, Hannah Ritchie, Esteban Ortiz-Ospina and Joe Hasell (2020) - Coronavirus Pandemic (COVID-19).,
                 Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/coronavirus' [Online Resource]",
                         " ",
                         "COVID cases and deaths data are retrieved from: Center for Systems Science and Engineering (CSSE), Johns Hopkins University",
                         "Citation: Dong E, Du H, Gardner L. An interactive web-based dashboard to track COVID-19 in real time. \n
                 Lancet Inf Dis. 20(5):533-534. doi: 10.1016/S1473-3099(20)30120-1
                 ", 
                         " ",
                         "We calculated the average daily tests performed during current week and last week, and we quantified the relative percent change in testing current week compared to last week. 
                  We calculated the average positivity rate during current week and last week, and we quantified the relative percent change in positivity rate current week compared to last week. 
                  We calculated the average daily death due to COVID reported during current week and last week, and we quantified the relative percent change in COVID deaths during current week compared to last week.
                  A week is 6 days from the most recent data report date, and last week is from 7 to 13 days from the most recent data report date. For the daily tests, positive tests and deaths we used 7-day smoothed average to correct for reporting delays.
                  If daily tests during last week or current week are not reported then NA% is printed on the dashboard for testing and positivity rate. 
                  ",
                         "The effective reproduction number is estimated using EpiEstim R package. Only reported positive cases,
                 have been considered. Asymptomatic transmission, unreported cases and intervention are ignored. 
                 7-day smoothed average of reported positive cases was used as incidence.
                 "," The mean serial interval is 3.96 days (95% CI 3.53–4.39 days), and SD is 4.75 days (95% CI 4.46–5.07 days). Please see: Du, Z.et al. (2020). Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerging Infectious Diseases, 26(6), 1341-1343.",
                         " ",
                         "Author: Abdou Fofana. You can always contact us at covid.shiny@gmail.com.
                         The code to generate this dashborad is freely available at https://github.com/abdouf/Covid-WestAfrica",
                         sep="<br/>"))
      )
      
    )
  )
)
# UI is the user interface
#shinyApp(ui, server)