
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(RJSONIO)
library(shiny)
library(plotly)
library(DT)
library(plyr)
library(dplyr)
library(zoo)
library(stringr)
shinyServer(function(input, output) {
  values <- reactiveValues(x = NULL)
  observeEvent(input$Go, {
    maxfile<-paste("TMAX",input$location,paste(input$year,'.csv',sep=''),sep = '/')
    filename<-paste(toupper(input$location),'_',input$year,'_TMIN.csv',sep='')
    minfile<-paste("TMIN",input$location,filename,sep = '/')
    writeLines(maxfile)
    writeLines(minfile)
    if(file.exists(maxfile)){
      values$x <- read.csv(maxfile,header = FALSE,sep=',')
    }
    if(file.exists(minfile)){
      values$y <- read.csv(minfile,header = FALSE,sep=',')
    }
  })

  output$Month <- renderPlotly({
    if(input$val==T){
      plot_ly(d<-filter(values$x,Date=(Date%/%100==input$month)),x=d$Date%%100,y=d$Tmax,text=paste("Date:",d$Date%%100))%>%
        layout(xaxis = list(title="DATE",showgrid=T),yaxis=list(title="MAX TEMPERATURE(Tmax in deg)"))
    }
    else
      plot_ly(values$x,x=values$x$Date/100,y=values$x$Tmax,group=values$x$Date%%100,mode="markers")%>%
      layout(xaxis = list(title="DATE",showgrid=T),yaxis=list(title="MAX TEMPERATURE(Tmax in deg)"))
  })
  
  observeEvent(input$Go, {
    names(values$x)<-c('ID','Date','Tmax')
    values$x$Date<-as.numeric(substr(y<-as.character(values$x$Date),5,nchar(y)))
    currentdate<-as.numeric(substr(y<-as.character(Sys.Date()),9,nchar(y)))
    currentmonth<-as.numeric(substr(y<-as.character(Sys.Date()),6,7))
    alldatesmax<-filter(values$x,Date=(Date%%100==currentdate))
    maxtemp<-filter(alldatesmax,Date=(Date%/%100==currentmonth))
    maxtemp<-maxtemp$Tmax
    maxtemp
    
    names(values$y)<-c('ID','Date','Tmax')
    values$y$Date<-as.numeric(substr(y<-as.character(values$y$Date),5,nchar(y)))
    alldatesmin<-filter(values$y,Date=(Date%%100==currentdate))
    mintemp<-filter(alldatesmin,Date=(Date%/%100==currentmonth))
    mintemp<-mintemp$Tmax
    mintemp
    
    f<-seq(Sys.Date(), by = "day", length.out = 7)
    #weekdaylist<-as.numeric(substr(y<-as.character(f),9,nchar(y)))
    #weekdaymonth<-Unique(as.numeric(substr(y<-as.character(f),6,7)))
    days=filter(values$x,Date=(Date%/%100==input$month))
    days=days$Date
    weekmaxtemp<-values$x[values$x$Date %in% days,3]
    weekmintemp<-values$y[values$y$Date %in% days,3]
    
    
#      for(i in 0:7){
#        output$Date<-renderText({
#        d<-paste(input$year,substr(y<-as.character(Sys.Date()),5,nchar(y)),sep = '')+i
#          as.character(format(as.Date(d),"%d %b %Y %A"))
#        })
#     }
      output$year<-renderText({
        input$year
      })
      
    
    output$curimage<-myimagefunc(maxtemp)
    output$image2<-myimagefunc(weekmaxtemp[2])
    output$image3<-myimagefunc(weekmaxtemp[3])
    output$image4<-myimagefunc(weekmaxtemp[4])
    output$image5<-myimagefunc(weekmaxtemp[5])
    output$image6<-myimagefunc(weekmaxtemp[6])
    output$image7<-myimagefunc(weekmaxtemp[7])
    
    output$mintemp<-mytempfunc(mintemp)
    output$mintemp_2<-mytempfunc(weekmintemp[2])
    output$mintemp_3<-mytempfunc(weekmintemp[3])
    output$mintemp_4<-mytempfunc(weekmintemp[4])
    output$mintemp_5<-mytempfunc(weekmintemp[5])
    output$mintemp_6<-mytempfunc(weekmintemp[6])
    output$mintemp_7<-mytempfunc(weekmintemp[7])

    output$maxtemp<-mytempfunc(maxtemp)    
    output$temp_2<-mytempfunc(weekmaxtemp[2])
    output$temp_3<-mytempfunc(weekmaxtemp[3])
    output$temp_4<-mytempfunc(weekmaxtemp[4])
    output$temp_5<-mytempfunc(weekmaxtemp[5])
    output$temp_6<-mytempfunc(weekmaxtemp[6])
    output$temp_7<-mytempfunc(weekmaxtemp[7])
    

    output$temp1<-mytempfunc(weekmaxtemp[1])
    output$temp2<-mytempfunc(weekmaxtemp[2])
    output$temp3<-mytempfunc(weekmaxtemp[3])
    output$temp4<-mytempfunc(weekmaxtemp[4])
    output$temp5<-mytempfunc(weekmaxtemp[5])
    output$temp6<-mytempfunc(weekmaxtemp[6])
    output$temp7<-mytempfunc(weekmaxtemp[7])
    output$temp8<-mytempfunc(weekmaxtemp[8])
    output$temp9<-mytempfunc(weekmaxtemp[9])
    output$temp10<-mytempfunc(weekmaxtemp[10])
    output$temp11<-mytempfunc(weekmaxtemp[11])
    output$temp12<-mytempfunc(weekmaxtemp[12])
    output$temp13<-mytempfunc(weekmaxtemp[13])
    output$temp14<-mytempfunc(weekmaxtemp[14])
    output$temp15<-mytempfunc(weekmaxtemp[15])
    output$temp16<-mytempfunc(weekmaxtemp[16])
    output$temp17<-mytempfunc(weekmaxtemp[17])
    output$temp18<-mytempfunc(weekmaxtemp[18])
    output$temp19<-mytempfunc(weekmaxtemp[19])
    output$temp20<-mytempfunc(weekmaxtemp[20])
    output$temp21<-mytempfunc(weekmaxtemp[21])
    output$temp22<-mytempfunc(weekmaxtemp[22])
    output$temp23<-mytempfunc(weekmaxtemp[23])
    output$temp24<-mytempfunc(weekmaxtemp[24])
    output$temp25<-mytempfunc(weekmaxtemp[25])
    output$temp26<-mytempfunc(weekmaxtemp[26])
    output$temp27<-mytempfunc(weekmaxtemp[27])
    output$temp28<-mytempfunc(weekmaxtemp[28])
    output$temp29<-mytempfunc(weekmaxtemp[29])
    output$temp30<-mytempfunc(weekmaxtemp[30])
    output$temp31<-mytempfunc(weekmaxtemp[31])
    
    output$mintemp1<-mytempfunc(weekmintemp[1])
    output$mintemp2<-mytempfunc(weekmintemp[2])
    output$mintemp3<-mytempfunc(weekmintemp[3])
    output$mintemp4<-mytempfunc(weekmintemp[4])
    output$mintemp5<-mytempfunc(weekmintemp[5])
    output$mintemp6<-mytempfunc(weekmintemp[6])
    output$mintemp7<-mytempfunc(weekmintemp[7])
    output$mintemp8<-mytempfunc(weekmintemp[8])
    output$mintemp9<-mytempfunc(weekmintemp[9])
    output$mintemp10<-mytempfunc(weekmintemp[10])
    output$mintemp11<-mytempfunc(weekmintemp[11])
    output$mintemp12<-mytempfunc(weekmintemp[12])
    output$mintemp13<-mytempfunc(weekmintemp[13])
    output$mintemp14<-mytempfunc(weekmintemp[14])
    output$mintemp15<-mytempfunc(weekmintemp[15])
    output$mintemp16<-mytempfunc(weekmintemp[16])
    output$mintemp17<-mytempfunc(weekmintemp[17])
    output$mintemp18<-mytempfunc(weekmintemp[18])
    output$mintemp19<-mytempfunc(weekmintemp[19])
    output$mintemp20<-mytempfunc(weekmintemp[20])
    output$mintemp21<-mytempfunc(weekmintemp[21])
    output$mintemp22<-mytempfunc(weekmintemp[22])
    output$mintemp23<-mytempfunc(weekmintemp[23])
    output$mintemp24<-mytempfunc(weekmintemp[24])
    output$mintemp25<-mytempfunc(weekmintemp[25])
    output$mintemp26<-mytempfunc(weekmintemp[26])
    output$mintemp27<-mytempfunc(weekmintemp[27])
    output$mintemp28<-mytempfunc(weekmintemp[28])
    output$mintemp29<-mytempfunc(weekmintemp[29])
    output$mintemp30<-mytempfunc(weekmintemp[30])
    output$mintemp31<-mytempfunc(weekmintemp[31])
    
  })
  
  
  mytempfunc<-function(temp){
    temp_sel<-renderText({
      paste(round(temp,2),"°C")
    })
    return(temp_sel)
  }
  
  myimagefunc<-function(temp){    
    if(temp>20){
      image_sel<-renderImage({
        filename <- normalizePath(file.path('www/partly_cloudy.png'))
        list(src = filename,
             alt = paste("Image Sunny"),
             height=100,width=100
        )
      },deleteFile = FALSE)
    }
    else{
      image_sel<-renderImage({
        filename <- normalizePath(file.path('www/sunny.png'))
        list(src = filename,
             alt = paste("Image cool"),
             height=100,width=100
        )
      },deleteFile = FALSE)
    }
    return(image_sel)
  }
  ###Sliding Window Algorithm
  observeEvent(input$Goo,{
    #reading the date
    
    prediction_date<-input$Date
    predictionDate<-as.Date(prediction_date)
    class(predictionDate)
    predictionDate
    
    #################################################################################
    #current temperature
    temp_data1 <- read.csv("bangalore_2016_csv.csv" , sep = ",") 
    total_data1<-temp_data1[, seq(1,3)]
    names(total_data1)<- c('DATE','TMAX','TMIN')
    total_data1
    total_data1$TMAX<-as.numeric(substr(as.character(total_data1$TMAX),1,2))
    total_data1$TMIN<-as.numeric(substr(as.character(total_data1$TMIN),1,2))
    total_data1
    
    cur_year_data<-total_data1[as.numeric(substr(y<-as.character((total_data1$DATE)),1,4)) %in% 2016,]
    cur_year_data
    
    
    #getting the date format from the curr year data
    cur_month_date<-seq(predictionDate-7, by = 'day',length.out = 7)
    days1<-as.numeric(paste(substr(y<-as.character(cur_month_date),6,7),substr(y<-as.character(cur_month_date),9,nchar(y)),sep=''))
    
    #matching the data
    #var<- filter(prev_year_data,DATE=(DATE%/%10000==2015))
    cur_year_data$DATE<-as.numeric(substr(y<-as.character(cur_year_data$DATE),5,nchar(y)))
    CD<-cur_year_data[cur_year_data$DATE %in% days1,c(2,3)]
    CD
    as.data.frame.matrix(CD)
    CD[1,1]
    
    #centigarde conversion
    for(i in 1:7)
    {
      CD[i,1]=((as.numeric(CD[i,1])-32)*(5/9))
      CD[i,2]=((as.numeric(CD[i,2])-32)*(5/9))
    }
    as.data.frame.matrix(CD)
    currYearData<-array(CD[1:2])
    names(currYearData)<- c('TMAX','TMIN')
    
    ################################################################################
    #previous years data - actual temperature from Accuweather API
    temp_data <- read.csv("Bangalore_2015_csv_02.csv" , sep = ",") 
    total_data<-temp_data[, seq(1,3)]
    names(total_data)<- c('DATE','TMAX','TMIN')
    total_data
    total_data$TMAX<-as.numeric(substr(as.character(total_data$TMAX),1,2))
    total_data$TMIN<-as.numeric(substr(as.character(total_data$TMIN),1,2))
    total_data
    
    prev_year_data<-total_data[as.numeric(substr(y<-as.character((total_data$DATE)),1,4)) %in% 2015,]
    prev_year_data
    
    #getting the date format from the previous year data
    prev_month_date<-seq(predictionDate-7, by = 'day',length.out = 14)
    days<-as.numeric(paste(substr(y<-as.character(prev_month_date),6,7),substr(y<-as.character(prev_month_date),9,nchar(y)),sep=''))
    
    #matching the data
    #var<- filter(prev_year_data,DATE=(DATE%/%10000==2015))
    prev_year_data$DATE<-as.numeric(substr(y<-as.character(prev_year_data$DATE),5,nchar(y)))
    PD<-prev_year_data[prev_year_data$DATE %in% days,c(2,3)]
    as.data.frame.matrix(PD)
    
    ################################################################################
    #dividing previous year data into 8 sliding window algorithms
    x<-PD
    y <- rollapply(x, width=7, FUN=function(x) {print(x);},align = "right")
    as.data.frame.matrix(y)
    
    sliding_windows <- array(, dim=c(7,2,8))
    j<-1
    for(i in 1:8)
    {
      sliding_windows[,j,i]=y[i,1:7]
      sliding_windows[,j+1,i]=y[i,8:14]
      sliding_windows[,,i]
    }
    
    
    #conversion to centigrade
    for(i in 1:8)
    {
      for(j in 1:7)
      {
        for(k in 1:2)
        {
          sliding_windows[j,k,i]=(as.numeric(sliding_windows[j,k,i]-32)*(5/9))
        }
      }
    }
    
    ###############################################################################
    #converting the current year dataframe to matrix
    curr <- array(, dim=c(7,2,1))
    for(i in 1:7)
    {
      for(j in 1:2)
      {
        curr[i,j,1]=as.numeric(currYearData[i,j])
      }
    }
    
    #ED_matrices contains the euclidean distances - 8 in number
    ED_matrices <- array(, dim=c(7,1,8))
    
    #function to find the euclidian distance
    euclidian_diat<- function(x1,x2,y1,y2){
      res<- sqrt(((x1-x2)^2)+((y1-y2)^2))
      
      return(res)
    }
    
    
    for(i in 1:8)
    {
      for(j in 1:7)
      {
        ED_matrices[j,1,i]<-euclidian_diat(curr[j,1,1],sliding_windows[j,1,i],curr[j,2,1],sliding_windows[j,2,i])
        
      }
    }
    
    #contains the mean of the each euclidean matrix
    ED_means<- array(,dim = c(8,1))
    for (i in 1:8) {
      ED_means[i,1]=mean(ED_matrices[,,i])
    }
    
    #finding the min(ED_means)
    matched_window_position=which(ED_means==min(ED_means))
    
    #matched_window
    sliding_windows[,,matched_window_position]
    matched_window=array(,dim = c(7,2))
    matched_window=sliding_windows[,,matched_window_position]
    matched_window
    ################################################################################
    
    #variation
    vc <- array(, dim = c(6,2))
    vp <- array(, dim=c(6,2))
    
    for(i in 1:6)
    {
      for(j in 1:2)
      {
        vc[i,j]=curr[i,j,1]-curr[i+1,j,1]    
      }
    }
    vc
    
    for(i in 1:6)
    {
      for(j in 1:2)
      {
        vp[i,j]=matched_window[i,j]-matched_window[i+1,j]    
      }
    }
    vp
    
    class(vp)
    colnames(vc)<-c('TMAX','TMIN')
    colnames(vp)<-c('TMAX','TMIN')
    colnames(curr) <- c('TMAX','TMIN')
    
    as.data.frame(vc)
    vc[,1]
    #mean of the variations
    mean_TMAX1=mean(vc[,1])
    mean_TMAX2=mean(vp[,1])
    
    mean_TMIN1=mean(vc[,2])
    mean_TMIN2=mean(vp[,2])
    
    v_TMAX=(mean_TMAX1+mean_TMAX2)/2
    v_TMIN=(mean_TMIN1+mean_TMIN2)/2
    
    res_TMAX <- as.data.frame(matrix(curr[,1,1]+v_TMAX, ncol = 1, nrow = 7))
    is.data.frame(res_TMAX)
    res_TMIN <- as.data.frame(matrix(curr[,2,1]+v_TMIN, ncol = 1, nrow = 7))
    
    res_TMAX
    res_TMIN
    colnames(res_TMAX)<-c('TMAX')
    colnames(res_TMIN)<-c('TMIN')
    
    ################################################################################
    #Actual Temperatures  
    
    actual_temp_data<- read.csv("bangalore_2016_csv.csv" , sep = ",") 
    actual_total_data<-actual_temp_data[, seq(1,3)]
    names(actual_total_data)<- c('DATE','TMAX','TMIN')
    actual_total_data
    actual_total_data$TMAX<-as.numeric(substr(as.character(actual_total_data$TMAX),1,2))
    actual_total_data$TMIN<-as.numeric(substr(as.character(actual_total_data$TMIN),1,2))
    actual_total_data
    
    actual_cur_year_data<-actual_total_data[as.numeric(substr(y<-as.character((actual_total_data$DATE)),1,4)) %in% 2016,]
    actual_cur_year_data
    
    
    #getting the date format from the curr year data
    actual_cur_month_date<-seq(predictionDate, by = 'day',length.out = 7)
    actual_days<-as.numeric(paste(substr(y<-as.character(actual_cur_month_date),6,7),substr(y<-as.character(actual_cur_month_date),9,nchar(y)),sep=''))
    
    #matching the data
    #var<- filter(prev_year_data,DATE=(DATE%/%10000==2015))
    actual_cur_year_data$DATE<-as.numeric(substr(y<-as.character(actual_cur_year_data$DATE),5,nchar(y)))
    actual_CD<-actual_cur_year_data[actual_cur_year_data$DATE %in% actual_days,c(2,3)]
    
    as.data.frame.matrix(actual_CD)
    #centigarde conversion
    for(i in 1:7)
    {
      actual_CD[i,1]=((as.numeric(actual_CD[i,1])-32)*(5/9))
      actual_CD[i,2]=((as.numeric(actual_CD[i,2])-32)*(5/9))
    }
    as.data.frame.matrix(actual_CD)
    actual_currYearData<-array(actual_CD[1:2])
    names(actual_currYearData)<- c('Actual_TMAX','Actual_TMIN')
    actual_currYearData
    
    ################################################################################
    
    dates<- seq(predictionDate, by = 'day',length.out = 7)
    resultant_data_frame<-cbind(dates,res_TMAX,res_TMIN,actual_currYearData$Actual_TMAX,actual_currYearData$Actual_TMIN)
    colnames(resultant_data_frame)<-c('Dates','P_TMAX','P_TMIN','A_TMAX','A_TMIN')
    resultant_data_frame
    write.csv(resultant_data_frame, file = "predicted_temperature_data.csv")
    predicted_temperature=read.csv("predicted_temperature_data.csv")
    str(predicted_temperature)
    predicted_temperature
    
    #drawing the comparision Plot
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "Blue"
    )
    x <- list(
      title = "Dates",
      titlefont = f
    )
    y <- list(
      title = "Temperature",
      titlefont = f
    )

  
  output$predicted_tmax_tmin_plot<-renderPlotly({
    plot_ly(predicted_temperature,x = Dates, y =P_TMAX , name="Predicted Max Temp", line = list(shape = "spline"))  %>%
      add_trace(x = Dates,y =P_TMIN, name="Predicted Min Temp", line = list(shape = "spline")) %>%
      layout(xaxis = x, yaxis = y)
  })
  ################################################################################
  #Comparision of Minimum and maximum Temperature
  
  output$comparision_tmax_tmin_plot<-renderPlotly({
    plot_ly(predicted_temperature,x = Dates, y =P_TMAX , name="Predicted Max Temp", line = list(shape = "spline"))  %>%
      add_trace(x = Dates,y =P_TMIN, name="Predicted Min Temp", line = list(shape = "spline")) %>%
      add_trace(x = Dates,y = A_TMIN, name="Actual Min Temp", line = list(shape = "spline")) %>%
      add_trace(x = dates,y = A_TMAX, name="Actual Max Temp", line = list(shape = "spline")) %>%
      layout(xaxis = x, yaxis = y)
  })
  ################################################################################
  #Comparision of Maximum Temperature
  
  
  output$comparision_tmax_plot<-renderPlotly({
    plot_ly(predicted_temperature,x = Dates, y =P_TMAX , name="Predicted Max Temp", line = list(shape = "spline"))  %>%
      add_trace(x = dates,y = A_TMAX, name="Actual Max Temp", line = list(shape = "spline")) %>%
      layout(xaxis = x, yaxis = y)
  })
  ################################################################################
  #Comparision of Minimum Temperature
  
  output$comparision_tmin_plot<-renderPlotly({
    plot_ly(predicted_temperature,x = Dates, y =P_TMIN , name="Predicted Min Temp", line = list(shape = "spline"))  %>%
      add_trace(x = dates,y = A_TMIN, name="Actual Min Temp", line = list(shape = "spline")) %>%
      layout(xaxis = x, yaxis = y)
  })
  })
})
