library(shiny)
library("quantmod")
library("ggplot2")
library("tidyquant")
library("clipr")
library("lubridate")

ui <- fluidPage(
  
  fluidRow(
    tags$img(src = "Heading1.jpg")
  ),
  
  fluidRow(
    
    
    column(2,
           
           wellPanel(  
             h3("Company Data"),
             wellPanel(
               
               textInput(inputId = "compCode", label = "Company Code", value = "AMZN"),
               dateRangeInput(inputId = "dtrngPrice", "Date Range For Price Collection", start = "2020-01-01", end = Sys.Date(), max = Sys.Date())
             ),
             
             h3("Retracement Zones & Fans"),
             
             wellPanel(
               tabsetPanel(
                 tabPanel("Dow Retracement",
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num1_R1", "R1", value = NA)),
                              column(6,numericInput(inputId = "num1_R2", "R2", value = NA))
                            )
                            
                          )
                 ),
                 tabPanel("Fibonacci Retracement",
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num1_F1", "F1", value = NA)),
                              column(6,numericInput(inputId = "num1_F2", "F2", value = NA))
                            )
                            
                          )
                 ),
                 tabPanel("Speed Lines",
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num1_S1", "S1", value = NA)),
                              column(6,numericInput(inputId = "num1_S2", "S2", value = NA))
                            ),
                            
                            dateRangeInput(inputId = "speeddate", "Date Range", start = Sys.Date(), end = Sys.Date(), max = Sys.Date())
                          )
                 ),
                 tabPanel("Fibonacci Fans",
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num1_FF1", "FF1", value = NA)),
                              column(6,numericInput(inputId = "num1_FF2", "FF2", value = NA))
                            ),
                            
                            dateRangeInput(inputId = "fandate", "Date Range", start = "2020-06-15", end = Sys.Date(), max = Sys.Date())
                            
                          )
                 )
               )
             ),
             
             h3("Custom Lines"),
             
             wellPanel(
               tabsetPanel(
                 
                 tabPanel("Custom Line 1",
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num1_Y1", "Y1", value = NA)),
                              column(6,numericInput(inputId = "num1_Y2", "Y2", value = NA))
                            ),
                            
                            dateRangeInput(inputId = "dtrng1", "Date Range", start = Sys.Date(), end = Sys.Date(), max = Sys.Date())
                          )
                 ),
                 
                 tabPanel("Custom Line 2",
                          
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num2_Y1", "Y1", value = NA)),
                              column(6,numericInput(inputId = "num2_Y2", "Y2", value = NA))
                            ),
                            
                            dateRangeInput(inputId = "dtrng2", "Date Range", start = Sys.Date(), end = Sys.Date(), max = Sys.Date())
                          )
                 ),
                 
                 tabPanel("Custom Line 3",
                          
                          wellPanel(
                            
                            fluidRow(
                              column(6, numericInput(inputId = "num3_Y1", "Y1", value = NA)),
                              column(6,numericInput(inputId = "num3_Y2", "Y2", value = NA))
                            ),
                            
                            dateRangeInput(inputId = "dtrng3", "Date Range", start = Sys.Date(), end = Sys.Date(), max = Sys.Date())
                          )
                 )
               )
             ),
             
             actionButton(inputId = "updtBtn", "Update")
           )
    ),
    
    
    
    
    
    column(9,
           tabsetPanel(
             tabPanel("Main", 
                      fluidRow(
                        
                        column(6,{
                          plotOutput("RegPlot")
                        }),
                        
                        column(6,{
                          plotOutput("LogPlot")
                        })
                      ),
                      
                      
                      fluidRow(
                        plotOutput("CandlePlot", click = "plot_click")
                      ),
                      
                      fluidRow(
                        plotOutput("VolumePlot") 
                      )
             ),
             tabPanel("Week/Month",
                      fluidRow(
                        plotOutput("weeklyCandlePlot")
                      ),
                      fluidRow(
                        plotOutput("weeklyVolumePlot")
                      )
             ),
             tabPanel("MACD/RSI", 
                      fluidRow(
                        plotOutput("MACD_PLOT")
                      ),
                      
                      fluidRow(
                        
                      )
             ),
             tabPanel("SMA",
                      fluidRow(plotOutput("SMA_PLOT20")),
                      fluidRow(plotOutput("SMA_PLOT")),
                      fluidRow(plotOutput("Price_SMA_PLot"))
             ),
             tabPanel("Stats",
                      htmlOutput("inc")
             ),
             tabPanel("Data", 
                      br(),
                      downloadButton("downloadData", "download"),
                      br(),
                      strong(h3("Data")),
                      verbatimTextOutput("print")
             ),
             tabPanel("Information",
                      fluidRow(
                        column(5,
                               h3("How to Use the App"),
                               br(),
                               br(),
                               h4("Company Data"),
                               br(),
                               p("The company data consists of two sections: The company code, which determines which company should be analyzed, the default being amazon, and the timspan in which the company is analyzed. Once a company is chosen simply click the update button at the bottom of the page to redraw all the charts.")
                        ),
                        column(7,
                               fluidRow(
                                 
                                 br(),
                                 br(),

                                 tags$img(src = "CompanyData.png")
                               )
                        )
                      ),
                        fluidRow(
                          column(5,
                                 h4("Retracement Zones and Fans"),
                                 br(),
                                 p("This option will draw any of the four retracement zones or fans onto the interactive candlestick chart on the Main Page tab, simply choose on of the four options: Dow Retracement (Default), Fibonacci Retracement, Speed Lines, or Fibonacci Fans and enter the lowest and highest prices as well as the dates (If required) for the period you wish the zones/fans to be drawn."),
                                 p("Tip: by clicking on a pont on the interactive candlestick plot the app will atomatically copy the price at that point to your clipboard, allwing you to simply paste it into the retracement-zone/fan textboxes, unfortunately this does not work with the dates as well and the will still have to be manually entered "),
                                 p("As with the Company Data, to draw in the fans and retracement zones you will need to click the update button at the bottom of the page.")
                          ),
                          column(3,
                                 br(),
                                 br(),

                                 tags$img(src = "ReZones.png")
                                 
                                 
                          )

                        ),
                      fluidRow(
                        column(5,
                               h4("Custom Lines"),
                               br(),
                               p("The custom lines section works in a similar fasion to the previous section: Retracement Zones & Fans. The only difference being that the custom lines section allows you to draw three traight lines through a set of x (Dates) and y (Prices) coordinates of your choice. As previously you can copy the y coords off of the interactive candlestick chart by simply clicking on the desired point.")
                        ),
                        column(7,
                               fluidRow(
                                 br(),
                                 br(),
                                 tags$img(src = "CustLines.png")
                               )
                        )
                      ),
                      
                      
                      fluidRow(
                        hr(),
                        column(5,
                               
                               h3("About the App & Developer"),
                               br(),
                               br(),
                               strong("Developed by: "),  "Willem Ruan Nieuwenhuis",
                               br(),
                               strong("Purpose of App: "), "I built this app to hone my knowledge of R as well as showcase what I can do. I while intended the app for financial analysis I cannot reccomend its use in the current state. I do plan to continue working on this app to where it will be fit for use as well as to continually improve upon the app to make it as accurate and as user friendly as possible. ",
                               br(),
                               h3("Contact Info"),
                               br(),
                               strong("Email: "), "ruan.nieu@gmail.com",
                               br(),
                               strong("LinkedIn: "), tags$a(href="https://www.linkedin.com/in/wrniew/", "My Profile"),
                               br(),
                               strong("Cel: "), "+27798767372"

                        ),
                        column(7,
                               fluidRow(
                                 
                                 br(),
                                 br(),
                                 
                                 tags$img(src = "ProfilePic.png")
                               )
                        )
                      )
                      
                        
                      )
             )
           )
           
           
    ) 
  )


server <- function(input, output, session) {
  

  output$inc<-renderUI({
    link = paste('https://finance.yahoo.com/quote/', input$compCode, '/key-statistics?p=', input$compCode, sep = "")
    return((HTML(readLines(link))))
  })
  

  showModal(modalDialog(
    title = "Important Message",
    "Thank you for using my technical analysis shiny app.", br(), br(), "Please note that this app was designed to showcase my proficiency at coding in R and was not intended to be used for actual technical or financial analysis in any way. Furthermore all the of the values have been checked by me alone and thus I cannot garantee the accuracy of the results obtained and thus should not be used as a basis for drawing further conclusions.", br(), br(), "If you require help or information about the app please click on information in the tabs section.",
    easyClose = TRUE,
    footer = NULL
  ))


MainData = eventReactive(input$updtBtn,{
            getSymbols(Symbols = input$compCode, src ="yahoo", from = input$dtrngPrice[1], to = input$dtrngPrice[2])
            STOCK <- get(input$compCode)
            STCKData = data.frame(STOCK)
            Dates = index(STOCK)
            STCKData = data.frame(STOCK,Dates)
            colnames(STCKData) = c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Dates")
            Av = mean(STCKData$Close)
            stddev = sd(STCKData$Close)

            #Regression
            regres = lm(Close~Dates, data = STCKData)
            STCKData$regdata = fitted.values(regres)

            
            
            #EMA12
            
            if(length(STCKData$Close) > 12){
            
              n_12 = 12
              k_12 = 2/(n_12 + 1)
              x_12 = mean(STCKData$Close[1:12])
              l_12 = (length(STCKData$Close) - (n_12-1))
              EMA_12 = data.frame(numeric(length(STCKData$Close)))
              colnames(EMA_12) = "EMA"
  
              EMA_12[1:11,1] = NA
              EMA_12[12,1] = x_12
  
              for (i_12 in 2:l_12) {
  
                x_12 = STCKData$Close[i_12+n_12-1]*k_12+x_12*(1-k_12)
                EMA_12$EMA[i_12+11] = x_12
              }
  
              STCKData$EMA_12 = EMA_12$EMA
            }

            #EMA26
            
            if(length(STCKData$Close) > 26){

               n_26 = 26
               k_26 = 2/(n_26 + 1)
               x_26 = mean(STCKData$Close[1:26])
               l_26 = (length(STCKData$Close) - (n_26-1))
               EMA_26 = data.frame(numeric(length(STCKData$Close)))
               colnames(EMA_26) = "EMA"

               EMA_26[1:25,1] = NA
               EMA_26[26,1] = x_26

               for (i_26 in 2:l_26) {

                 x_26 = STCKData$Close[i_26+n_26-1]*k_26+x_26*(1-k_26)
                 EMA_26$EMA[i_26+25] = x_26
               }

               STCKData$EMA_26 = EMA_26$EMA
            }
            

            #MACD
            
            if(length(STCKData$Close) > 26){
              n_MD = 26
              x_MD = STCKData$EMA_12[26] - STCKData$EMA_26[26]
              l_MD = (length(STCKData$Close) - (n_MD-1))
              MACData = data.frame(numeric(length(STCKData$Close)))
              colnames(MACData) = "MACDVal"

              MACData[1:25,1] = NA
              MACData[26,1] = x_MD

              for (i_MD in 2:l_MD) {

                x_MD = STCKData$EMA_12[i_MD + n_MD - 1] - STCKData$EMA_26[i_MD + n_MD - 1]
                MACData$MACDVal[i_MD + 25] = x_MD
              }

              STCKData$MACD = MACData$MACDVal
            }
            
            #EMA9 of MACD
            
            if(length(STCKData$Close) > (25+9)){
              
              n_9 = 34
              k_9 = 2/(n_9 + 1)
              x_9 = mean(STCKData$MACD[26:34])
              l_9 = (length(STCKData$Close) - (n_9-1))
              MACD_EMA_9 = data.frame(numeric(length(STCKData$Close)))
              colnames(MACD_EMA_9) = "EMA"
              
              MACD_EMA_9[1:33,1] = NA
              MACD_EMA_9[34,1] = x_9
              
              for (i_9 in 2:l_9) {
                
                x_9 = STCKData$MACD[i_9+n_9-1]*k_9+x_9*(1-k_9)
                MACD_EMA_9$EMA[i_9+33] = x_9
              }
              
              STCKData$MACD_EMA_9 = MACD_EMA_9$EMA
            }
            #SMA20
            
            if(length(STCKData$Close) > 20){
              n_20 = 20
              l_20 = length(STCKData$Close)
              
              SMA_20 = data.frame(numeric(length(STCKData$Close)))
              colnames(SMA_20) = "SMA_20"
              SMA_20$SMA_20[1:19] = NA
              
              for (i_SMA20 in 20:l_20) {
                SMA_20$SMA_20[i_SMA20] = sum(STCKData$Close[(i_SMA20-19):i_SMA20])/20
              }
              
              STCKData$SMA_20 = unlist(SMA_20)
            }
            
            #SMA50
            
            if(length(STCKData$Close) > 50){
              n_50 = 50
              l_50 = length(STCKData$Close)
              
              SMA_50 = data.frame(numeric(length(STCKData$Close)))
              colnames(SMA_50) = "SMA_50"
              SMA_50$SMA_50[1:49] = NA
              
              for (i_SMA50 in 50:l_50) {
                SMA_50$SMA_50[i_SMA50] = sum(STCKData$Close[(i_SMA50-49):i_SMA50])/50
              }
              
              STCKData$SMA_50 = unlist(SMA_50)
            }
            
            #SMA200
            
            if(length(STCKData$Close) > 200){
              n_200 = 200
              l_200 = length(STCKData$Close)
              
              SMA_200 = data.frame(numeric(length(STCKData$Close)))
              colnames(SMA_200) = "SMA_200"
              SMA_200$SMA_200[1:199] = NA
              
              for (i_SMA200 in 200:l_200) {
                SMA_200$SMA_200[i_SMA200] = sum(STCKData$Close[(i_SMA200-199):i_SMA200])/200
              }
              
              STCKData$SMA_200 = unlist(SMA_200)
            }
            
            rownames(STCKData) <- 1:nrow(STCKData)
            STCKData
  }, ignoreNULL = FALSE)

WeekData = reactive({
  
  Initialdata = data.frame(MainData()$Dates, MainData()$Close, MainData()$Open, MainData()$High, MainData()$Low, MainData()$Volume)
  colnames(Initialdata) = c("Dates", "Close", "Open", "High", "Low", "Volume")
  
  strt = 1
  for (i in 1:5) {
    if(wday(Initialdata[i,1]) == 2)
      strt = i
  }
  
  FinData1 = numeric(length(seq(strt,length(Initialdata$Dates),5 )) - strt)
  FinData2 = numeric(length(seq(strt,length(Initialdata$Dates),5 )) - strt)
  FinData3 = numeric(length(seq(strt,length(Initialdata$Dates),5 )) - strt)
  FinData4 = numeric(length(seq(strt,length(Initialdata$Dates),5 )) - strt)
  FinData5 = numeric(length(seq(strt,length(Initialdata$Dates),5 )) - strt)
  FinData6 = numeric(length(seq(strt,length(Initialdata$Dates),5 )) - strt)
  cntr = 1
  
  for (j in seq(strt,length(Initialdata$Dates),5 )) {
    
    FinData1[cntr] = Initialdata[j,1]
    FinData2[cntr] = Initialdata[j+4,2]
    FinData3[cntr] = Initialdata[j,3]
    FinData4[cntr] = max(Initialdata[j,4],Initialdata[(j+1),4],Initialdata[(j+2),4],Initialdata[(j+3),4],Initialdata[(j+4),4])
    FinData5[cntr] = min(Initialdata[j:j+4,5])
    FinData6[cntr] = sum(Initialdata[j:j+4,6])
    cntr = cntr + 1
  }
  
  FinData = data.frame(as.Date(FinData1), FinData2, FinData3, FinData4, FinData5, FinData6)
  colnames(FinData) = c("Dates", "Close", "Open", "High", "Low", "Volume")
  
  FinData 
})

#Custom Line Data

observeEvent(input$plot_click$y, {
  clickdata = c(input$plot_click$y, as.Date(input$plot_click$x)) 
  
  write_clip(round(input$plot_click$y,2))
  
  showModal(modalDialog(
    title = "Coords",
    paste0("The coords you have clicked on are ",round(clickdata[1],1)," and ",as.Date(clickdata[2]),"."),
    easyClose = TRUE,
    footer = NULL
  ))
})

SpeedLine = eventReactive(c(input$updtBtn), {
  
  Fanval = c(input$num1_S1, input$num1_S2)
  Fanrange = max(Fanval) - min(Fanval)
  l_end = length(c(MainData()$Dates[1]:MainData()$Dates[length(MainData()$Dates)]))
  l_end = l_end - (as.numeric(input$fandate[1]) - as.numeric(MainData()$Dates[1]))
  
  m3 = (Fanval[1] - (max(Fanval)-0.33*Fanrange))/(as.numeric(input$fandate[1]) - as.numeric(input$fandate[2]))
  c3 = -m3*Fanval[1] + as.numeric(input$fandate[1]) 
  
  thirtyeight = numeric(l_end)
  thirtyeight[1] = Fanval[1]
  
  m5 = (Fanval[1] - (max(Fanval)-0.5*Fanrange))/(as.numeric(input$fandate[1]) - as.numeric(input$fandate[2]))
  c5 = -m5*Fanval[1] + as.numeric(input$fandate[1]) 
  
  fivetyF = numeric(l_end)
  fivetyF[1] = Fanval[1]
  
  m6 = (Fanval[1] - (max(Fanval)-0.66*Fanrange))/(as.numeric(input$fandate[1]) - as.numeric(input$fandate[2]))
  c6 = -m6*Fanval[1] + as.numeric(input$fandate[1]) 
  
  sixtyone = numeric(l_end)
  sixtyone = Fanval[1]
  
  for (i356 in 2:l_end) {
    
    thirtyeight[i356] = Fanval[1] + m3*(i356-1)
    fivetyF[i356] = Fanval[1] + m5*(i356-1)
    sixtyone[i356] = Fanval[1] + m6*(i356-1)
    
  }
  
  dateFval = as.Date(c(input$fandate[1]:MainData()$Dates[length(MainData()$Dates)]))
  
  ansF = data.frame(dateFval, thirtyeight, fivetyF, sixtyone)
  ansF
})

FanLine = eventReactive(c(input$updtBtn), {
  
  Fanval = c(input$num1_FF1, input$num1_FF2)
  Fanrange = max(Fanval) - min(Fanval)
  l_end = length(c(MainData()$Dates[1]:MainData()$Dates[length(MainData()$Dates)]))
  l_end = l_end - (as.numeric(input$fandate[1]) - as.numeric(MainData()$Dates[1]))
  
  m3 = (Fanval[1] - (max(Fanval)-0.382*Fanrange))/(as.numeric(input$fandate[1]) - as.numeric(input$fandate[2]))
  c3 = -m3*Fanval[1] + as.numeric(input$fandate[1]) 
  
  thirtyeight = numeric(l_end)
  thirtyeight[1] = Fanval[1]
  
  m5 = (Fanval[1] - (max(Fanval)-0.5*Fanrange))/(as.numeric(input$fandate[1]) - as.numeric(input$fandate[2]))
  c5 = -m5*Fanval[1] + as.numeric(input$fandate[1]) 
  
  fivetyF = numeric(l_end)
  fivetyF[1] = Fanval[1]
  
  m6 = (Fanval[1] - (max(Fanval)-0.618*Fanrange))/(as.numeric(input$fandate[1]) - as.numeric(input$fandate[2]))
  c6 = -m6*Fanval[1] + as.numeric(input$fandate[1]) 
  
  sixtyone = numeric(l_end)
  sixtyone = Fanval[1]
  
  for (i356 in 2:l_end) {
    
    thirtyeight[i356] = Fanval[1] + m3*(i356-1)
    fivetyF[i356] = Fanval[1] + m5*(i356-1)
    sixtyone[i356] = Fanval[1] + m6*(i356-1)
    
  }
  
  dateFval = as.Date(c(input$fandate[1]:MainData()$Dates[length(MainData()$Dates)]))
  
  ansF = data.frame(dateFval, thirtyeight, fivetyF, sixtyone)
  ansF
})

RetFLine = eventReactive(c(input$updtBtn), {
  
  retFval = c(input$num1_F1, input$num1_F2)
  retFrange = max(retFval) - min(retFval)
  
  thirtyeight = c(max(retFval)-0.382*retFrange, max(retFval)-0.382*retFrange)
  fivetyF = c(max(retFval)-0.50*retFrange, max(retFval)-0.50*retFrange)
  sixtyone = c(max(retFval)-0.618*retFrange, max(retFval)-0.618*retFrange)
  
  dateFval = as.Date(c(MainData()$Dates[1], MainData()$Dates[length(MainData()$Dates)]))
  
  ansF = data.frame(dateFval, thirtyeight, fivetyF, sixtyone)
  ansF
})

RetLine = eventReactive(c(input$updtBtn), {
  
  retval = c(input$num1_R1, input$num1_R2)
  retrange = max(retval) - min(retval)
  
  thirtythree = c(max(retval)-0.33*retrange, max(retval)-0.33*retrange)
  fivety = c(max(retval)-0.50*retrange, max(retval)-0.50*retrange)
  sixtysix = c(max(retval)-0.66*retrange, max(retval)-0.66*retrange)

  dateval = as.Date(c(MainData()$Dates[1], MainData()$Dates[length(MainData()$Dates)]))
  
  ans = data.frame(dateval, thirtythree, fivety, sixtysix)
  ans
})

CSLine = eventReactive(c(input$updtBtn), {
  
  
  custy_1 = c(input$num1_Y1,input$num1_Y2)
  custy_2 = c(input$num2_Y1,input$num2_Y2)
  custy_3 = c(input$num3_Y1,input$num3_Y2)
  custx_1 = as.Date(c(input$dtrng1[1],input$dtrng1[2]))
  custx_2 = as.Date(c(input$dtrng2[1],input$dtrng2[2]))
  custx_3 = as.Date(c(input$dtrng3[1],input$dtrng3[2]))
     
  cust = data.frame(custy_1,custx_1,custy_2,custx_2,custy_3,custx_3)
  cust

})

#Main Tab Plot Outputs

output$RegPlot = renderPlot({
            
            ggplot() + geom_line(data = MainData(), aes(x = Dates, y = Close)) + 
              geom_line(data = MainData(), aes(x = Dates, y = regdata, color = "red")) +
              ylab("Prices (Main Monitary Unit)") + 
              labs(title = "Closing Price Line Chart") +
              scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
  })

output$VolumePlot = renderPlot({
  
            ggplot() + geom_bar(data = MainData() ,stat = 'identity', aes(x = Dates, y = Volume)) + 
              labs(title = "Volume Chart") +
              scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
  })

output$LogPlot = renderPlot({
  
  ggplot() + geom_line(data = MainData(), aes(x = Dates, y = Close)) + 
            ylab("Prices (Main Monitary Unit)") + 
            labs(title = "Log Scale Closing Price Line Chart") +
            coord_trans( y="log10") +
            scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
  })

output$weeklyCandlePlot = renderPlot({

  ggplot() + geom_candlestick(data = WeekData(), aes(x = Dates, open = Open, high = High, low = Low, close = Close) ) +
      ylab("Prices (Main Monitary Unit)") +
      labs(title = "Weekly Candlestick Chart") +
      scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
  })

output$weeklyVolumePlot = renderPlot({
  
  ggplot() + geom_bar(data = WeekData() ,stat = 'identity', aes(x = Dates, y = Volume)) + 
    labs(title = "Weekly Volume Chart") +
    scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
})

output$CandlePlot = renderPlot({
  
  if(is.na(CSLine()$custy_1)[1] || is.na(CSLine()$custy_1[2]))  {
    
  candleplt =  ggplot() + geom_candlestick(data = MainData(), aes(x = Dates, open = Open, high = High, low = Low, close = Close) ) +
      ylab("Prices (Main Monitary Unit)") +
      labs(title = "Interactive Candlestick Chart") +
      scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
    
  }else if(is.na(CSLine()$custy_2[1]) || is.na(CSLine()$custy_2[2])){
    
    candleplt = ggplot() + geom_candlestick(data = MainData(), aes(x = Dates, open = Open, high = High, low = Low, close = Close) ) +
      geom_line(data = CSLine(), aes(x = custx_1, y = custy_1, color = "black")) +
      ylab("Prices (Main Monitary Unit)") +
      labs(title = "Interactive Candlestick Chart") +
      scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
    
  }else if(is.na(CSLine()$custy_3[1]) || is.na(CSLine()$custy_3[2])){
    
    candleplt = ggplot() + geom_candlestick(data = MainData(), aes(x = Dates, open = Open, high = High, low = Low, close = Close) ) +
      geom_line(data = CSLine(), aes(x = custx_1, y = custy_1, color = "black")) +
      geom_line(data = CSLine(), aes(x = custx_2, y = custy_2, color = "black")) +
      ylab("Prices (Main Monitary Unit)") +
      labs(title = "Interactive Candlestick Chart") +
      scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
    
  }else{
  
    candleplt = ggplot() + geom_candlestick(data = MainData(), aes(x = Dates, open = Open, high = High, low = Low, close = Close) ) +
      geom_line(data = CSLine(), aes(x = custx_1, y = custy_1, color = "black")) +
      geom_line(data = CSLine(), aes(x = custx_2, y = custy_2, color = "black")) +
      geom_line(data = CSLine(), aes(x = custx_3, y = custy_3, color = "black")) +
      ylab("Prices (Main Monitary Unit)") +
      labs(title = "Candlestick Chart") +
      scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
  }
  
  if(!is.na(RetLine()[1]) && !is.na(RetLine()[2])){
    candleplt = candleplt +
      geom_line(data = RetLine(), aes(x = dateval, y = thirtythree, color = "black")) +
      geom_line(data = RetLine(), aes(x = dateval, y = fivety, color = "black")) +
      geom_line(data = RetLine(), aes(x = dateval, y = sixtysix, color = "black"))
  }
  
  if(!is.na(RetFLine()[1]) && !is.na(RetFLine()[2])){
    candleplt = candleplt +
      geom_line(data = RetFLine(), aes(x = dateFval, y = thirtyeight, color = "black")) +
      geom_line(data = RetFLine(), aes(x = dateFval, y = fivetyF, color = "black")) +
      geom_line(data = RetFLine(), aes(x = dateFval, y = sixtyone, color = "black"))
  }
  
  if(!is.na(FanLine()[1]) && !is.na(FanLine()[2]) && MainData()$Dates[1] < input$fandate[1] ){
    candleplt = candleplt +
      geom_line(data = FanLine(), aes(x = dateFval, y = thirtyeight, color = "black")) +
      geom_line(data = FanLine(), aes(x = dateFval, y = fivetyF, color = "black")) +
      geom_line(data = FanLine(), aes(x = dateFval, y = sixtyone, color = "black")) +
      ylim(min(MainData()$Close), max(MainData()$Close))
  }
  
  
  if(!is.na(SpeedLine()[1]) && !is.na(SpeedLine()[2]) && MainData()$Dates[1] < input$speeddate[1] ){
    candleplt = candleplt +
      geom_line(data = SpeedLine(), aes(x = dateFval, y = thirtyeight, color = "black")) +
      geom_line(data = SpeedLine(), aes(x = dateFval, y = fivetyF, color = "black")) +
      geom_line(data = SpeedLine(), aes(x = dateFval, y = sixtyone, color = "black")) +
      ylim(min(MainData()$Close), max(MainData()$Close))
  }
  
  candleplt
  })

output$MACD_PLOT = renderPlot({
  
  colors = c("MACD" = "darkblue", "EMA 9" = "darkgreen")
  
  ggplot() + geom_line(data = MainData(), aes(x = Dates, y = MACD, color = "MACD")) +
    geom_line(data = MainData(), aes(x = Dates, y = MACD_EMA_9, color = "MACD EMA 9")) +
    labs(title = "MACD + Signal 9") +
    scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
})

output$SMA_PLOT20 = renderPlot({
  
  colors = c("SMA 20" = "darkblue", "SMA 50" = "darkgreen")
  
  ggplot() + geom_line(data = MainData(), aes(x = Dates, y = SMA_20, color = "SMA 20")) +
    geom_line(data = MainData(), aes(x = Dates, y = SMA_50, color = "SMA 50")) +
    labs(title = "SMA 20 + 50") +
    scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
})

output$SMA_PLOT = renderPlot({
  
  colors = c("SMA 50" = "darkblue", "SMA 200" = "darkgreen")
  
  ggplot() + geom_line(data = MainData(), aes(x = Dates, y = SMA_50, color = "SMA 50")) +
            geom_line(data = MainData(), aes(x = Dates, y = SMA_200, color = "SMA 200")) +
            labs(title = "SMA 50 + 200") +
            scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
})


output$Price_SMA_PLot = renderPlot({
  
  ggplot() + geom_line(data = MainData(), aes(x = Dates, y = Close)) + 
            ylab("Prices (Main Monitary Unit)") + 
            labs(title = "Closing Price Line Chart") +
            scale_x_date(breaks = seq(min(MainData()$Dates), max(MainData()$Dates), by = round(length(MainData()$Dates)/5,1)))
})
  
#Downloading Data


output$downloadData <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(MainData(), file)
  }
)

#Debug and Data Output

output$print = renderPrint({
  options(max.print=1000000)
  print(MainData())
}
)
}

shinyApp(ui, server)