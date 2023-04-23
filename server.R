#############################################################################################################################
# This code generates all the content shown in the App
#############################################################################################################################
server <- function(input, output) { 
  # For nice plot let's decide to have only 10 ticks on the date axis
  nticks = 5
  tg_date_breaks1 = round(as.numeric(difftime(as.Date(df_tg$Dates[nrow(df_tg)]),as.Date(df_tg$Dates[1])))/nticks)
  tg_date_breaks2 = round(as.numeric(difftime(as.Date(data_vaccinated_tg$dates[nrow(data_vaccinated_tg)]),as.Date(data_vaccinated_tg$dates[1])))/nticks)
  
  # Togo data to generate the numbers
  dataSet = df_tg[which(df_tg$Dates<=date_test_reported_tg),] # Limit the calculations to reported data
  last_week_tests = dataSet$daily_smoothed_tests[(nrow(dataSet)-13):(nrow(dataSet)-7)]
  this_week_tests = dataSet$daily_smoothed_tests[(nrow(dataSet)-6):nrow(dataSet)]
  last_week_positive = dataSet$daily_smoothed_positive[(nrow(dataSet)-13):(nrow(dataSet)-7)]
  this_week_positive = dataSet$daily_smoothed_positive[(nrow(dataSet)-6):nrow(dataSet)]
  
  Testing = 100*((mean(this_week_tests)) - (mean(last_week_tests)))/(mean(last_week_tests))
  if(is.na(Testing)){
    Testing_tg = "NA"
  } else {
    Testing_tg = round(Testing, digits = 2)
  }
  positivity_rate = 100*(mean((this_week_positive/this_week_tests))-mean((last_week_positive/last_week_tests)))/mean((last_week_positive/last_week_tests))
  if(is.na(positivity_rate)){
    positivity_rate_tg = "NA"
  } else {
    positivity_rate_tg = round(positivity_rate, digits = 2)
  }
  
  dataSet = df_tg[which(df_tg$Dates<=date_case_reported_tg),] # Limit the calculations to reported data
  this_week_death = mean(dataSet$daily_smoothed_death[(nrow(dataSet)-6):nrow(dataSet)])
  last_week_death = mean(dataSet$daily_smoothed_death[(nrow(dataSet)-13):(nrow(dataSet)-7)])
  if (last_week_death == 0 & this_week_death ==0){
    death_rate = 0
    death_rate_tg = round(death_rate, digits = 2)
  } else if (last_week_death == 0 & this_week_death !=0) {
    death_rate_tg = 9999
  } else {
    death_rate = 100*(this_week_death - last_week_death)/last_week_death
    death_rate_tg = round(death_rate, digits = 2)
  }
  
  # Ghana data to generate the numbers
  gh_date_breaks1 = round(as.numeric(difftime(as.Date(df_gh$Dates[nrow(df_gh)]),as.Date(df_gh$Dates[1])))/nticks)
  gh_date_breaks2 = round(as.numeric(difftime(as.Date(data_vaccinated_gh$dates[nrow(data_vaccinated_gh)]),as.Date(data_vaccinated_gh$dates[1])))/nticks)
  
  dataSet = df_gh[which(df_gh$Dates<=date_test_reported_gh),]
  last_week_tests = dataSet$daily_smoothed_tests[(nrow(dataSet)-13):(nrow(dataSet)-7)]
  this_week_tests = dataSet$daily_smoothed_tests[(nrow(dataSet)-6):nrow(dataSet)]
  last_week_positive = dataSet$daily_smoothed_positive[(nrow(dataSet)-13):(nrow(dataSet)-7)]
  this_week_positive = dataSet$daily_smoothed_positive[(nrow(dataSet)-6):nrow(dataSet)]
  
  Testing = 100*((sum(this_week_tests)) - (sum(last_week_tests)))/(sum(last_week_tests))
  if(is.na(Testing)){
    Testing_gh = "NA"
  } else {
    Testing_gh = round(Testing, digits = 2)
  }
  positivity_rate = 100*(mean((this_week_positive/this_week_tests))-mean((last_week_positive/last_week_tests)))/mean((last_week_positive/last_week_tests))
  if(is.na(positivity_rate)){
    positivity_rate_gh = "NA"
  } else {
    positivity_rate_gh = round(positivity_rate, digits = 2)
  }
  dataSet = df_gh[which(df_gh$Dates<=date_case_reported_gh),]
  this_week_death = mean(dataSet$daily_smoothed_death[(nrow(dataSet)-6):nrow(dataSet)])
  last_week_death = mean(dataSet$daily_smoothed_death[(nrow(dataSet)-13):(nrow(dataSet)-7)])
  if (last_week_death == 0 & this_week_death ==0){
    death_rate = 0
    death_rate_gh = round(death_rate, digits = 2)
  } else if (last_week_death == 0 & this_week_death !=0) {
    death_rate_gh = 9999
  } else {
    death_rate = 100*(this_week_death - last_week_death)/last_week_death
    death_rate_gh = round(death_rate, digits = 2)
  }
  
  # Nigeria data to generate the numbers
  ng_date_breaks1 = round(as.numeric(difftime(as.Date(df_ng$Dates[nrow(df_ng)]),as.Date(df_ng$Dates[1])))/nticks)
  ng_date_breaks2 = round(as.numeric(difftime(as.Date(data_vaccinated_ng$dates[nrow(data_vaccinated_ng)]),as.Date(data_vaccinated_ng$dates[1])))/nticks)
  
  dataSet = df_ng[which(df_ng$Dates<=date_test_reported_ng),]
  last_week_tests = dataSet$daily_smoothed_tests[(nrow(dataSet)-13):(nrow(dataSet)-7)]
  this_week_tests = dataSet$daily_smoothed_tests[(nrow(dataSet)-6):nrow(dataSet)]
  last_week_positive = dataSet$daily_smoothed_positive[(nrow(dataSet)-13):(nrow(dataSet)-7)]
  this_week_positive = dataSet$daily_smoothed_positive[(nrow(dataSet)-6):nrow(dataSet)]
  
  Testing = 100*((sum(this_week_tests)) - (sum(last_week_tests)))/(sum(last_week_tests))
  if(is.na(Testing)){
    Testing_ng = "NA"
  } else {
    Testing_ng = round(Testing, digits = 2)
  }
  positivity_rate = 100*(mean((this_week_positive/this_week_tests))-mean((last_week_positive/last_week_tests)))/mean((last_week_positive/last_week_tests))
  if(is.na(positivity_rate)){
    positivity_rate_ng = "NA"
  } else {
    positivity_rate_ng = round(positivity_rate, digits = 2)
  }
  
  dataSet = df_ng[which(df_ng$Dates<=date_case_reported_ng),]
  this_week_death = mean(dataSet$daily_smoothed_death[(nrow(dataSet)-6):nrow(dataSet)])
  last_week_death = mean(dataSet$daily_smoothed_death[(nrow(dataSet)-13):(nrow(dataSet)-7)])
  if (last_week_death == 0 & this_week_death ==0){
    death_rate = 0
    death_rate_ng = round(death_rate, digits = 2)
  } else if (last_week_death == 0 & this_week_death !=0) {
    death_rate_ng = 9999
  } else {
    death_rate = 100*(this_week_death - last_week_death)/last_week_death
    death_rate_ng = round(death_rate, digits = 2)
  }
  
  #creating the valueBoxOutput content for Togo
  output$value1 <- renderValueBox({
    valueBox(
      paste(formatC(Testing_tg, format="g"),'%') # g = significant digits
      ,paste('Is average daily testing up or down (Last vs this week)? as of', format(as.Date(date_test_reported_tg), format="%b-%d"))
      ,(if (Testing_tg < 0){
        icon = icon("arrow-down")
      } else if (Testing_tg == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "blue")
  })
  output$value2 <- renderValueBox({
    valueBox(
      paste(formatC(positivity_rate_tg, format="g"),'%')
      ,paste('Is positivity rate up or down (Last vs this week)? as of', format(as.Date(date_test_reported_tg), format="%b-%d"))
      ,(if (positivity_rate_tg < 0){
        icon = icon("arrow-down")
      } else if (positivity_rate_tg == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "red")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      paste(formatC(death_rate_tg, format="g"),'%')
      ,paste('Is COVID death rate up or down (Last vs this week)? as of', format(as.Date(date_case_reported_tg), format="%b-%d"))
      ,(if (death_rate_tg < 0){
        icon = icon("arrow-down")
      } else if (death_rate_tg == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "purple")  
  })
  output$value4 <- renderValueBox({
    valueBox(
      paste(vaccines_tg)
      ,paste('COVID Vaccines being administred as of', format(as.Date(date_vaccines_tg), format="%b-%d"))
      ,color = "green")  
  })
  #create the plot content Togo
  output$positive_smooth_tg <- renderPlot({
    ggplot(data=df_tg[which(df_tg$Dates<=date_case_reported_tg),])+
      geom_bar(aes(x = as.Date(Dates,"%Y-%m-%d"), y=daily_interpolated_positive, colour = "Inc1"), stat = "identity")+
      geom_line(aes(x=as.Date(Dates,"%Y-%m-%d"), y=daily_smoothed_positive, colour = "Inc2"), size = 1)+
      labs( x="Calendar date", y="Daily positive tests"
      ) +
      scale_x_date(date_breaks = paste0(tg_date_breaks1," days"), date_labels = "%b-%Y")+
      scale_colour_manual(name = " ",
                          values=c(Inc1="deepskyblue2",Inc2 = "blue"),
                          labels=c(Inc1="Positive tests", Inc2="7-day average positive tests")) +
      guides(color = guide_legend(override.aes = list(
        color = c("deepskyblue2", "blue"),
        fill  = c("white", "white"),
        linetype = c("solid", "solid")))) +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.key = element_rect(fill = "white"),
            legend.position = c(0.8, 0.99), legend.justification = c(1, 1))
    
  })
  output$reproduction_smooth_tg <- renderPlot({
    ggplot(data=R_tg) +
      geom_ribbon(aes(x=as.Date(Dates,"%Y-%m-%d"),ymin=lci,ymax=uci), fill = "lightpink1")+
      geom_line(aes(x=as.Date(Dates), y=meanR, colour = "Rt"), size = 1) + 
      geom_line(aes(x=as.Date(Dates), y=1, colour = "base"), size = 1) +
      scale_colour_manual(name = " ",
                          values=c(base="black",Rt = "red"),
                          labels=c(base="Continued spread threshold", Rt="Reproduction number + 95% CI")) +
      labs( x="Calendar date", y="New infections per infected (Rt)"
            ,title = "Note: Only reported positive cases are considered"
      )+
      scale_x_date(date_breaks = paste0(tg_date_breaks1," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.8, 0.99), legend.justification = c(1, 1))
  })
  output$death_rec_active_tg <- renderPlot({
    ggplot(data=melt_tg, aes(x = Dates, y = Casedata, fill = Casetype))+
      geom_bar(stat = 'identity',position = 'dodge') +
      scale_fill_manual(" ", values = c("Active" = "deepskyblue1", "Deaths" = "black", "Recovered" = "goldenrod3")) +
      labs( x="Calendar date", y="Cumulative case "
      ) +
      scale_x_date(date_breaks = paste0(tg_date_breaks1," days"),date_labels = "%b-%Y")+
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  # Remove the interpolated NA from the last days.  
  df_tg[which(df_tg$Dates>date_test_reported_tg),"daily_smoothed_tests"] = NA_real_
  df_tg[which(df_tg$Dates>date_case_reported_tg),"total_interpolated_positive"] = NA_real_
  df_tg[which(df_tg$Dates>date_case_reported_tg),"total_interpolated_death"] = NA_real_
  output$death_positive_tg <- renderPlot({
    ggplot(data=df_tg)+
      geom_line(aes(x=Dates, y=daily_smoothed_positive), color = "red", size = 1)+
      geom_line(aes(x=Dates, y=daily_smoothed_death*50),color = "blue", size = 1)+
      scale_y_continuous(sec.axis = sec_axis(~.*(1/50), name = "7-day average deaths"))+
      labs( x="Calendar date", y="7-day average positive tests"
      ) +
      scale_x_date(date_breaks = paste0(tg_date_breaks1," days"),date_labels = "%b-%Y")+
      theme_linedraw() +
      theme_light()+
      theme(text=element_text(size=18,family="serif"),
            axis.line.y.right = element_line(color = "blue"),
            axis.text.y.right = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "blue"),
            axis.line.y.left = element_line(color = "red"),
            axis.text.y.left = element_text(color = "red"),
            axis.title.y.left = element_text(color = "red"),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })
  # Vaccination data total TG
  df_vaccinated_tg = data.frame(dates = rep(data_vaccinated_tg$dates,2),
             vaccinated = c(data_vaccinated_tg$total_vaccinated,data_vaccinated_tg$fully_vaccinated),
             gr = c(rep("Received 1 dose",nrow(data_vaccinated_tg)),rep("Fully vaccinated",nrow(data_vaccinated_tg))))
  output$total_vaccination_tg <- renderPlot({
    ggplot(data = df_vaccinated_tg, aes(x=dates, y=vaccinated, fill=gr))+
      geom_area(color='black', size=0.3, alpha=1) + # Puts a black line to separate the 2
      scale_fill_brewer(palette = 'Greens', direction = -1) +
      labs( x="Calendar date", y="Number of people vaccinated ", fill = " ") +
      scale_x_date(date_breaks = paste0(tg_date_breaks2," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  # Vaccination data fraction
  df_fr_vaccinated_tg = data.frame(dates = rep(data_vaccinated_tg$dates,2),
                                vaccinated = c(data_vaccinated_tg$fraction_vaccinated,data_vaccinated_tg$fraction_fully_vaccinated),
                                gr = c(rep("Received 1 dose",nrow(data_vaccinated_tg)),rep("Fully vaccinated",nrow(data_vaccinated_tg))))
  output$fraction_vaccination_tg <- renderPlot({
    ggplot(data = df_fr_vaccinated_tg, aes(x=dates, y=vaccinated, fill=gr))+
      geom_area(color='black', size=0.3, alpha=1) + # Puts a black line to separate the 2
      scale_fill_brewer(palette = 'Greens', direction = -1) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs( x="Calendar date", y="% of the population vaccinated ", fill = " ") +
      scale_x_date(date_breaks = paste0(tg_date_breaks2," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  # For GH
  output$value5 <- renderValueBox({
    valueBox(
      paste(formatC(Testing_gh, format="g"),'%') # g = significant digits
      ,paste('Is average daily testing up or down (Last vs this week)? as of', format(as.Date(date_test_reported_gh), format="%b-%d"))
      ,(if (Testing_gh < 0){
        icon = icon("arrow-down")
      } else if (Testing_gh == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "blue")
  })
  output$value6 <- renderValueBox({
    valueBox(
      paste(formatC(positivity_rate_gh, format="g"),'%')
      ,paste('Is positivity rate up or down (Last vs this week)? as of',format(as.Date(date_test_reported_gh), format="%b-%d"))
      ,(if (positivity_rate_gh < 0){
        icon = icon("arrow-down")
      } else if (positivity_rate_gh == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "red")  
  })
  output$value7 <- renderValueBox({
    valueBox(
      paste(formatC(death_rate_gh, format="g"),'%')
      ,paste('Is COVID death rate up or down (Last vs this week)? as of',format(as.Date(date_case_reported_gh), format="%b-%d"))
      ,(if (death_rate_gh < 0){
        icon = icon("arrow-down")
      } else if (death_rate_gh == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "purple")  
  })
  output$value8 <- renderValueBox({
    valueBox(
      paste(vaccines_gh)
      ,paste('COVID Vaccines being administred as of', format(as.Date(date_vaccines_gh), format="%b-%d"))
      ,color = "green")  
  })
  #create the plot content GH
  output$positive_smooth_gh <- renderPlot({
    ggplot(data=df_gh[which(df_gh$Dates<=date_case_reported_gh),])+
      geom_bar(aes(x = as.Date(Dates,"%Y-%m-%d"), y=daily_interpolated_positive, colour = "Inc1"), stat = "identity")+
      geom_line(aes(x=as.Date(Dates,"%Y-%m-%d"), y=daily_smoothed_positive, colour = "Inc2"), size = 1)+
      labs( x="Calendar date", y="Daily positive tests"
      ) +
      scale_x_date(date_breaks = paste0(gh_date_breaks1," days"),date_labels = "%b-%Y")+
      scale_colour_manual(name = " ",
                          values=c(Inc1="deepskyblue2",Inc2 = "blue"),
                          labels=c(Inc1="Positive tests", Inc2="7-day average positive tests")) +
      guides(color = guide_legend(override.aes = list(
        color = c("deepskyblue2", "blue"),
        fill  = c("white", "white"),
        linetype = c("solid", "solid")))) +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.key = element_rect(fill = "white"),
            legend.position = c(0.8, 0.99), legend.justification = c(1, 1))
    
  })
  output$reproduction_smooth_gh <- renderPlot({
    ggplot(data=R_gh) +
      geom_ribbon(aes(x=as.Date(Dates,"%Y-%m-%d"),ymin=lci,ymax=uci), fill = "lightpink1")+
      geom_line(aes(x=as.Date(Dates), y=meanR, colour = "Rt"), size = 1) + 
      geom_line(aes(x=as.Date(Dates), y=1, colour = "base"), size = 1) +
      scale_colour_manual(name = " ",
                          values=c(base="black",Rt = "red"),
                          labels=c(base="Continued spread threshold", Rt="Reproduction number + 95% CI")) +
      labs( x="Calendar date", y="New infections per infected (Rt)"
            ,title = "Note: Only reported positive cases are considered"
      )+
      scale_x_date(date_breaks = paste0(gh_date_breaks1," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.8, 0.99), legend.justification = c(1, 1))
  })
  output$death_rec_active_gh <- renderPlot({
    ggplot(data=melt_gh, aes(x = Dates, y = Casedata, fill = Casetype))+
      geom_bar(stat = 'identity',position = 'dodge') +
      scale_fill_manual(" ", values = c("Active" = "deepskyblue1", "Deaths" = "black", "Recovered" = "goldenrod3")) +
      labs( x="Calendar date", y="Cumulative case "
      ) +
      scale_x_date(date_breaks = paste0(gh_date_breaks1," days"),date_labels = "%b-%Y")+
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  df_gh[which(df_gh$Dates>date_test_reported_gh),"daily_smoothed_tests"] = NA_real_
  df_gh[which(df_gh$Dates>date_case_reported_gh),"total_interpolated_positive"] = NA_real_
  df_gh[which(df_gh$Dates>date_case_reported_gh),"total_interpolated_death"] = NA_real_
  output$death_positive_gh <- renderPlot({
    ggplot(data=df_gh)+
      geom_line(aes(x=Dates, y=daily_smoothed_positive), color = "red", size = 1)+
      geom_line(aes(x=Dates, y=daily_smoothed_death*50),color = "blue", size = 1)+
      scale_y_continuous(sec.axis = sec_axis(~.*(1/50), name = "7-day average deaths"))+
      labs( x="Calendar date", y="7-day average positive tests"
      ) +
      scale_x_date(date_breaks = paste0(gh_date_breaks1," days"),date_labels = "%b-%Y")+
      theme_linedraw() +
      theme_light()+
      theme(text=element_text(size=18,family="serif"),
            axis.line.y.right = element_line(color = "blue"),
            axis.text.y.right = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "blue"),
            axis.line.y.left = element_line(color = "red"),
            axis.text.y.left = element_text(color = "red"),
            axis.title.y.left = element_text(color = "red"),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })
  # Vaccination data total GH
  df_vaccinated_gh = data.frame(dates = rep(data_vaccinated_gh$dates,2),
                                vaccinated = c(data_vaccinated_gh$total_vaccinated,data_vaccinated_gh$fully_vaccinated),
                                gr = c(rep("Received 1 dose",nrow(data_vaccinated_gh)),rep("Fully vaccinated",nrow(data_vaccinated_gh))))
  output$total_vaccination_gh <- renderPlot({
    ggplot(data = df_vaccinated_gh, aes(x=dates, y=vaccinated, fill=gr))+
      geom_area(color='black', size=0.3, alpha=1) + # Puts a black line to separate the 2
      scale_fill_brewer(palette = 'Greens', direction = -1) +
      labs( x="Calendar date", y="Number of people vaccinated ", fill = " ") +
      scale_x_date(date_breaks = paste0(gh_date_breaks2," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  # Vaccination data fraction
  df_fr_vaccinated_gh = data.frame(dates = rep(data_vaccinated_gh$dates,2),
                                vaccinated = c(data_vaccinated_gh$fraction_vaccinated,data_vaccinated_gh$fraction_fully_vaccinated),
                                gr = c(rep("Received 1 dose",nrow(data_vaccinated_gh)),rep("Fully vaccinated",nrow(data_vaccinated_gh))))
  output$fraction_vaccination_gh <- renderPlot({
    ggplot(data = df_fr_vaccinated_gh, aes(x=dates, y=vaccinated, fill=gr))+
      geom_area(color='black', size=0.3, alpha=1) + # Puts a black line to separate the 2
      scale_fill_brewer(palette = 'Greens', direction = -1) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs( x="Calendar date", y="% of the population vaccinated ", fill = " ") +
      scale_x_date(date_breaks = paste0(gh_date_breaks2," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  # For NIGERIA
  output$value9 <- renderValueBox({
    valueBox(
      paste(formatC(Testing_ng, format="g"),'%') # g = significant digits
      ,paste('Is average daily testing up or down (Last vs this week)? as of',format(as.Date(date_test_reported_ng), format="%b-%d"))
      ,(if (Testing_ng < 0){
        icon = icon("arrow-down")
      } else if (Testing_ng == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "blue")
  })
  output$value10 <- renderValueBox({
    valueBox(
      paste(formatC(positivity_rate_ng, format="g"),'%')
      ,paste('Is positivity rate up or down (Last vs this week)? as of', format(as.Date(date_test_reported_ng), format="%b-%d"))
      ,(if (positivity_rate_ng < 0){
        icon = icon("arrow-down")
      } else if (positivity_rate_ng == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "red")  
  })
  output$value11 <- renderValueBox({
    valueBox(
      paste(formatC(death_rate_ng, format="g"),'%')
      ,paste('Is COVID death rate up or down (Last vs this week)? as of',format(as.Date(date_case_reported_ng), format="%b-%d"))
      ,(if (death_rate_ng < 0){
        icon = icon("arrow-down")
      } else if (death_rate_ng == 0){
        icon = icon("arrows")
      } else {
        icon = icon("arrow-up")
      })
      ,color = "purple")  
  })
  output$value12 <- renderValueBox({
    valueBox(
      paste(vaccines_ng)
      ,paste('COVID Vaccines being administred as of', format(as.Date(date_vaccines_tg), format="%b-%d"))
      ,color = "green")  
  })
  #create the plot content NG
  output$positive_smooth_ng <- renderPlot({
    ggplot(data=df_ng[which(df_ng$Dates<=date_case_reported_ng),])+
      geom_bar(aes(x = as.Date(Dates,"%Y-%m-%d"), y=daily_interpolated_positive, colour = "Inc1"), stat = "identity")+
      geom_line(aes(x=as.Date(Dates,"%Y-%m-%d"), y=daily_smoothed_positive, colour = "Inc2"), size = 1)+
      labs( x="Calendar date", y="Daily positive tests"
      ) +
      scale_x_date(date_breaks = paste0(ng_date_breaks1," days"),date_labels = "%b-%Y")+
      scale_colour_manual(name = " ",
                          values=c(Inc1="deepskyblue2",Inc2 = "blue"),
                          labels=c(Inc1="Positive tests", Inc2="7-day average positive tests")) +
      guides(color = guide_legend(override.aes = list(
        color = c("deepskyblue2", "blue"),
        fill  = c("white", "white"),
        linetype = c("solid", "solid")))) +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.key = element_rect(fill = "white"),
            legend.position = c(0.8, 0.99), legend.justification = c(1, 1))
    
  })
  output$reproduction_smooth_ng <- renderPlot({
    ggplot(data=R_ng) +
      geom_ribbon(aes(x=as.Date(Dates,"%Y-%m-%d"),ymin=lci,ymax=uci), fill = "lightpink1")+
      geom_line(aes(x=as.Date(Dates), y=meanR, colour = "Rt"), size = 1) + 
      geom_line(aes(x=as.Date(Dates), y=1, colour = "base"), size = 1) +
      scale_colour_manual(name = " ",
                          values=c(base="black",Rt = "red"),
                          labels=c(base="Continued spread threshold", Rt="Reproduction number + 95% CI")) +
      labs( x="Calendar date", y="New infections per infected (Rt)"
            ,title = "Note: Only reported positive cases are considered"
      )+
      scale_x_date(date_breaks = paste0(ng_date_breaks1," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.8, 0.99), legend.justification = c(1, 1))
  })
  output$death_rec_active_ng <- renderPlot({
    ggplot(data=melt_ng, aes(x = Dates, y = Casedata, fill = Casetype))+
      geom_bar(stat = 'identity',position = 'dodge') +
      scale_fill_manual(" ", values = c("Active" = "deepskyblue1", "Deaths" = "black", "Recovered" = "goldenrod3")) +
      labs( x="Calendar date", y="Cumulative case "
      ) +
      scale_x_date(date_breaks = paste0(ng_date_breaks1," days"),date_labels = "%b-%Y")+
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  df_ng[which(df_ng$Dates>date_test_reported_ng),"daily_smoothed_tests"] = NA_real_
  df_ng[which(df_ng$Dates>date_case_reported_ng),"total_interpolated_positive"] = NA_real_
  df_ng[which(df_ng$Dates>date_case_reported_ng),"total_interpolated_death"] = NA_real_
  output$death_positive_ng <- renderPlot({
    ggplot(data=df_ng)+
      geom_line(aes(x=Dates, y=daily_smoothed_positive), color = "red", size = 1)+
      geom_line(aes(x=Dates, y=daily_smoothed_death*50),color = "blue", size = 1)+
      scale_y_continuous(sec.axis = sec_axis(~.*(1/50), name = "7-day average deaths"))+
      labs( x="Calendar date", y="7-day average positive tests"
      ) +
      scale_x_date(date_breaks = paste0(ng_date_breaks1," days"),date_labels = "%b-%Y")+
      theme_linedraw() +
      theme_light()+
      theme(text=element_text(size=18,family="serif"),
            axis.line.y.right = element_line(color = "blue"),
            axis.text.y.right = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "blue"),
            axis.line.y.left = element_line(color = "red"),
            axis.text.y.left = element_text(color = "red"),
            axis.title.y.left = element_text(color = "red"),
            axis.text.x = element_text(angle = 0, hjust = 1))
  })
  # Vaccination data total NG
  df_vaccinated_ng = data.frame(dates = rep(data_vaccinated_ng$dates,2),
                                vaccinated = c(data_vaccinated_ng$total_vaccinated,data_vaccinated_ng$fully_vaccinated),
                                gr = c(rep("Received 1 dose",nrow(data_vaccinated_ng)),rep("Fully vaccinated",nrow(data_vaccinated_ng))))
  output$total_vaccination_ng <- renderPlot({
    ggplot(data = df_vaccinated_ng, aes(x=dates, y=vaccinated, fill=gr))+
      geom_area(color='black', size=0.3, alpha=1) + # Puts a black line to separate the 2
      scale_fill_brewer(palette = 'Greens', direction = -1) +
      labs( x="Calendar date", y="Number of people vaccinated ", fill = " ") +
      scale_x_date(date_breaks = paste0(ng_date_breaks2," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
  # Vaccination data fraction
  df_fr_vaccinated_ng = data.frame(dates = rep(data_vaccinated_ng$dates,2),
                                vaccinated = c(data_vaccinated_ng$fraction_vaccinated,data_vaccinated_ng$fraction_fully_vaccinated),
                                gr = c(rep("Received 1 dose",nrow(data_vaccinated_ng)),rep("Fully vaccinated",nrow(data_vaccinated_ng))))
  output$fraction_vaccination_ng <- renderPlot({
    ggplot(data = df_fr_vaccinated_ng, aes(x=dates, y=vaccinated, fill=gr))+
      geom_area(color='black', size=0.3, alpha=1) + # Puts a black line to separate the 2
      scale_fill_brewer(palette = 'Greens', direction = -1) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs( x="Calendar date", y="% of the population vaccinated ", fill = " ") +
      scale_x_date(date_breaks = paste0(ng_date_breaks2," days"),date_labels = "%b-%Y") +
      theme_linedraw() +
      theme_light()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            text=element_text(size=18,family="serif"),
            plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text( size=12), legend.text=element_text(size=12),
            legend.position = c(0.5, 0.99), legend.justification = c(1, 1))
  })
}