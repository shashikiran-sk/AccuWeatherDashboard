library(shiny)
library(plotly)
library(shinydashboard)
shinyUI(
  dashboardPage(skin="purple",
    dashboardHeader(title = "Weather",
                    #disable = TRUE,
                    dropdownMenu(type = "messages",
                                 messageItem(
                                   from = "Sales Dept",
                                   message = "Sales are steady this month."
                                 ),
                                 messageItem(
                                   from = "New User",
                                   message = "How do I register?",
                                   icon = icon("question"),
                                   time = "13:45"
                                 ),
                                 messageItem(
                                   from = "Support",
                                   message = "The new server is ready.",
                                   icon = icon("life-ring"),
                                   time = "2014-12-01"
                                 )
                    )),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Weekly Summary", tabName = "temperature", icon = icon("sun-o")),
        menuItem("Monthly Summary", tabName = "monthly_summary", icon = icon("area-chart")),
        menuItem("Graph", tabName = "graph", icon = icon("area-chart")),
        menuItem("Sliding Window",tabName = "sliding",icon = icon("sun-o"))
      ),
      sidebarMenu(
        selectInput("location",label="Select Location",
                    choices=c("Bangalore"="Bangalore","Calcutta"="Calcutta","Chennai"="Chennai","Goa"="Goa","New Delhi"="New Delhi","Patna"="Patna","Ranchi"="Ranchi","Vishakapatna"="Vishakapatnam"),
                    selected="Bangalore"),
        selectInput("year",label="Year",choices = c('2016'='2016','2017'='2017','2018'='2018','2019'='2019','2020'='2020'),selected = '2016'),
        selectInput("month",
                    label = "Choose a month to display",
                    choices = c("January"=1, "Febuary"=2,"March"=3,"April"=4,"May"=5,"June"=6,"July"=7,
                                "August"=8,"September"=9,"October"=10,"November"=11,"December"=12),
                    selected = as.numeric(substr(y<-as.character(Sys.Date()),6,7))),
        actionButton("Go","GO!",icon = icon("check")),
        radioButtons("val","Select the Graph period",c("Monthly" = T,"Year" = F),selected = T)
      )

    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "temperature",
                fluidRow(
                  column(4,h3(format(Sys.Date(),"%d %b")),h3(textOutput("year"))),
                  column(4,h3("TODAY"))
                ),
                fluidRow(
                  column(4,imageOutput("curimage",height = 102,width = 80)),
                  column(4,h1(em(textOutput("maxtemp"))),h4(textOutput("mintemp"),style="color:gray"))
                ),
                hr(),
                fluidRow(
                  column(2,imageOutput("image2",height=102,width = 80)),
                  column(2,imageOutput("image3",height=102,width = 80)),
                  column(2,imageOutput("image4",height=102,width = 80)),
                  column(2,imageOutput("image5",height=102,width = 80)),
                  column(2,imageOutput("image6",height=102,width = 80)),
                  column(2,imageOutput("image7",height=102,width = 80))
                ),
                fluidRow(
                  column(2,h5(format(Sys.Date()+1,"%d %b")),h4(em(textOutput("temp_2"))),h5(textOutput("mintemp_2"),style="color:gray")),
                  column(2,h5(format(Sys.Date()+2,"%d %b")),h4(em(textOutput("temp_3"))),h5(textOutput("mintemp_3"),style="color:gray")),
                  column(2,h5(format(Sys.Date()+3,"%d %b")),h4(em(textOutput("temp_4"))),h5(textOutput("mintemp_4"),style="color:gray")),
                  column(2,h5(format(Sys.Date()+4,"%d %b")),h4(em(textOutput("temp_5"))),h5(textOutput("mintemp_5"),style="color:gray")),
                  column(2,h5(format(Sys.Date()+5,"%d %b")),h4(em(textOutput("temp_6"))),h5(textOutput("mintemp_6"),style="color:gray")),
                  column(2,h5(format(Sys.Date()+6,"%d %b")),h4(em(textOutput("temp_7"))),h5(textOutput("mintemp_7"),style="color:gray"))
                ),
                hr()
        ),
        # Second tab content
        tabItem(tabName = "monthly_summary",
                fluidRow(
                  column(2,h3("1",style="color:#CC0000"),h4(textOutput("temp1")),h5(textOutput("mintemp1"),style="color:gray")),
                  column(2,h3("2",style="color:#CC0000"),h4(textOutput("temp2")),h5(textOutput("mintemp2"),style="color:gray")),
                  column(2,h3("3",style="color:#CC0000"),h4(textOutput("temp3")),h5(textOutput("mintemp3"),style="color:gray")),
                  column(2,h3("4",style="color:#CC0000"),h4(textOutput("temp4")),h5(textOutput("mintemp4"),style="color:gray")),
                  column(2,h3("5",style="color:#CC0000"),h4(textOutput("temp5")),h5(textOutput("mintemp5"),style="color:gray")),
                  column(2,h3("6",style="color:#CC0000"),h4(textOutput("temp6")),h5(textOutput("mintemp6"),style="color:gray"))
                ),
                hr(),
                fluidRow(
                  column(2,h3("7",style="color:#CC0000"),h4(textOutput("temp7")),h5(textOutput("mintemp7"),style="color:gray")),
                  column(2,h3("8",style="color:#CC0000"),h4(textOutput("temp8")),h5(textOutput("mintemp8"),style="color:gray")),
                  column(2,h3("9",style="color:#CC0000"),h4(textOutput("temp9")),h5(textOutput("mintemp9"),style="color:gray")),
                  column(2,h3("10",style="color:#CC0000"),h4(textOutput("temp10")),h5(textOutput("mintemp10"),style="color:gray")),
                  column(2,h3("11",style="color:#CC0000"),h4(textOutput("temp11")),h5(textOutput("mintemp11"),style="color:gray")),
                  column(2,h3("12",style="color:#CC0000"),h4(textOutput("temp12")),h5(textOutput("mintemp12"),style="color:gray"))
                ),
                hr(),
                fluidRow(
                  column(2,h3("13",style="color:#CC0000"),h4(textOutput("temp13")),h5(textOutput("mintemp13"),style="color:gray")),
                  column(2,h3("14",style="color:#CC0000"),h4(textOutput("temp14")),h5(textOutput("mintemp14"),style="color:gray")),
                  column(2,h3("15",style="color:#CC0000"),h4(textOutput("temp15")),h5(textOutput("mintemp15"),style="color:gray")),
                  column(2,h3("16",style="color:#CC0000"),h4(textOutput("temp16")),h5(textOutput("mintemp16"),style="color:gray")),
                  column(2,h3("17",style="color:#CC0000"),h4(textOutput("temp17")),h5(textOutput("mintemp17"),style="color:gray")),
                  column(2,h3("18",style="color:#CC0000"),h4(textOutput("temp18")),h5(textOutput("mintemp18"),style="color:gray"))
                ),
                hr(),
                fluidRow(
                  column(2,h3("19",style="color:#CC0000"),h4(textOutput("temp19")),h5(textOutput("mintemp19"),style="color:gray")),
                  column(2,h3("20",style="color:#CC0000"),h4(textOutput("temp20")),h5(textOutput("mintemp20"),style="color:gray")),
                  column(2,h3("21",style="color:#CC0000"),h4(textOutput("temp21")),h5(textOutput("mintemp21"),style="color:gray")),
                  column(2,h3("22",style="color:#CC0000"),h4(textOutput("temp22")),h5(textOutput("mintemp22"),style="color:gray")),
                  column(2,h3("23",style="color:#CC0000"),h4(textOutput("temp23")),h5(textOutput("mintemp23"),style="color:gray")),
                  column(2,h3("24",style="color:#CC0000"),h4(textOutput("temp24")),h5(textOutput("mintemp24"),style="color:gray"))
                ),
                hr(),
                fluidRow(
                  column(2,h3("25",style="color:#CC0000"),h4(textOutput("temp25")),h5(textOutput("mintemp25"),style="color:gray")),
                  column(2,h3("26",style="color:#CC0000"),h4(textOutput("temp26")),h5(textOutput("mintemp26"),style="color:gray")),
                  column(2,h3("27",style="color:#CC0000"),h4(textOutput("temp27")),h5(textOutput("mintemp27"),style="color:gray")),
                  column(2,h3("28",style="color:#CC0000"),h4(textOutput("temp28")),h5(textOutput("mintemp28"),style="color:gray")),
                  column(2,h3("29",style="color:#CC0000"),h4(textOutput("temp29")),h5(textOutput("mintemp29"),style="color:gray")),
                  column(2,h3("30",style="color:#CC0000"),h4(textOutput("temp30")),h5(textOutput("mintemp30"),style="color:gray"))
                ),
                hr(),
                fluidRow(
                  column(2,h3("31",style="color:#CC0000"),h4(textOutput("temp31")),h5(textOutput("mintemp31"),style="color:gray"))
                )
        ),
        
        # Third tab content
        tabItem(tabName = "graph",
                plotlyOutput("Month")
        ),
        
        #Fourth tab content
        tabItem(tabName = "sliding",
               fluidRow(
                 column(4,dateInput("Date","Select Date:",value = Sys.Date()+1)),
                 column(5,actionButton("Goo","Run",icon = icon("check")))
               ),
                box(title="Comparision of Predicted Tmax and Tmin",status = "primary",solidHeader = TRUE,
                    plotlyOutput("predicted_tmax_tmin_plot"),height = 500,width = 400
                    ),
               box(title="Comparision of  Tmax and Tmin",status = "warning",solidHeader = TRUE,
                   plotlyOutput("comparision_tmax_tmin_plot"),height = 500,width = 400
               ),
               box(title="Comparision of Maximum Temperature",status = "info",solidHeader = TRUE,
                   plotlyOutput("comparision_tmax_plot"),height = 500,width = 400
               ),
               box(title="Comparision of Minimum Temperature",status = "success",solidHeader = TRUE,
                   plotlyOutput("comparision_tmin_plot"),height = 500,width = 400
               )
        )
      )
    )
  )
)