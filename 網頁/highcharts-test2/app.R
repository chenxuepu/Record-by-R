#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# pk <- c("shiny","highcharter","xts","tidyverse")
# pk = pk[!( pk %in% installed.packages()[,"Package"] )]
# if(length(pk)) install.packages(pk)


library(shiny)
library(highcharter)
library(xts)
library(tidyverse)
# data <- read.csv("C:\\Users\\admin\\Desktop\\test\\point.csv")
data <- read.csv("point.csv")
data$date <- data$date %>% as.POSIXct()
data$ym <- format(data$date,format="%Y-%m")
data$ymd <- format(data$date,format="%Y-%m-%d")
data_rm <- group_by(data,ymd) %>%
    summarise(point=sum(point),times = n()) 
data_rm$ymd <- data_rm$ymd %>% as.POSIXct()
data$group <- sapply(data$date, function(x){
    if(format(x,format="%u") %in% c("6","7") ){
        week <- "holiday"
    }else{
        week <- "normal day"
    }
    y <- format(x,format="%H") %>% as.numeric()
    if(y<12){
        time <- "morning"
    }else if(y<18){
        time <- "noon"
    }else{
        time <- "night"
    }
    paste(week,time,sep = "-")
})
ym <- data$ym %>% unique()
# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("highchart test"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         mainPanel(
#             highchartOutput("Plot1"),
#             hr(),
#             highchartOutput("Plot2"),
#             hr(),
#             highchartOutput("Plot3"),
#             hr(),
#             highchartOutput("Plot4")
#             # tableOutput("value")
#         )
#     )
# )

ui <- fluidPage(
    titlePanel("highchart test"),
    tags$body('background'="bg_main.jpg",'style'="background-repeat:repeat; background-attachment:fixed; background-position:center top"),
    # HTML('<img src="logo.jpg">'),
    sidebarLayout(
        sidebarPanel(
            HTML('<img src="logo.jpg">'),
            br(),
            br(),
            br(),
            actionButton("main", label = "main"),
            br(),
            br(),
            br(),
            actionButton("stat", label = "stat")
        ),
        mainPanel(
            uiOutput("test")
        )
    )
)

# ui <- navbarPage("highchart test",
#                  tabPanel("main",
#                           tags$body('background'="bg_main.jpg",'style'="background-repeat:repeat; background-attachment:fixed; background-position:center top"),
#                           htmlOutput("test"),
#                           # tags$img('src'="data.png"),
#                           HTML('<center>
# 				<img src="data.png">
# 			</center>'),
#                           br(),
#                           br(),
#                           br(),
#                           br(),
#                           br(),
#                           br(),
# #                           HTML('<center><p><font><i>
# # 				Background vector of main.html designed by
# # 				<a href="https://www.freepik.com/layerace" target="_blank">Layerace</a>
# # 				on www.freepik.com
# # 				</i>
# # 			</font>			
# # 		</p></center>'),
#                           HTML('<center><a href="https://www.freepik.com/free-photos-vectors/background">Background vector created by Layerace - www.freepik.com</a></center>'),
# #                           tags$p(
# #                               HTML(text = '<font face="Times New Roman" size="4　style=&quot;color:gray;" background-color:#dcdcdc"=""><i>
# # 				※Background vector of main.html designed by
# # 				<a href="https://www.freepik.com/layerace" target="_blank">@Layerace</a>
# # 				on www.freepik.com
# # 				</i>
# # 			</font>')
# #                           )
#                          ),
#                  tabPanel("month",
#                           tabsetPanel(
#                               tabPanel("point",highchartOutput("Plot1")),
#                               tabPanel("times",highchartOutput("Plot2"))
#                           )
#                           # highchartOutput("Plot1")
#                           ),
#                  # tabPanel("Plot 2",highchartOutput("Plot2")),
#                  tabPanel("day",
#                           tabsetPanel(
#                               tabPanel("point",highchartOutput("Plot3")),
#                               tabPanel("times",highchartOutput("Plot4"))
#                           )
#                           # highchartOutput("Plot3")
#                           ),
#                  # tabPanel("Plot 4",highchartOutput("Plot4")),
#                  tabPanel("weekday",highchartOutput("Plot5")),
#                  tabPanel("shop",htmlOutput("Plot6")),
#                  tabPanel("people",htmlOutput("Plot7"))
# )
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # output$value <- renderPrint({
    #     input$file
    # })
    output$Plot1 <- renderHighchart({
        group_by(data,ym) %>%
            summarise(point=sum(point)) %>%
            hchart("column",hcaes(x = ym, y = point)) %>%
            hc_title(text ="point") 
    })
    output$Plot2 <- renderHighchart({
        group_by(data,ym) %>%
            summarise(times=n()) %>%
            hchart("column",hcaes(x = ym, y = times)) %>%
            hc_title(text ="times") 
    })
    output$Plot3 <- renderHighchart({
        xts(data_rm$point,order.by = data_rm$ymd) %>%
            hchart(type = "bar") %>%
            hc_title(text ="point") 
    })
    output$Plot4 <- renderHighchart({
        xts(data_rm$times,order.by = data_rm$ymd) %>%
            hchart(type = "bar") %>%
            hc_title(text ="times") 
        
    })
    output$Plot5 <- renderHighchart({
        group_by(data,ym,group) %>%
            summarise(point=mean(point)) %>%
            hchart( "line",hcaes(x = ym, y = point,group=group)) %>%
            hc_title(text ="point") %>%
            hc_tooltip(crosshairs = TRUE,shared = TRUE)
        
    })
    output$Plot6 <- renderUI({
        hc <- list()
        j <- 0
        for(i in ym){
            hc[[(j <- j+1)]] <- data$shop[data$ym==i] %>% 
                table() %>% 
                sort(decreasing = T) %>% 
                .[1:10] %>% 
                data.frame() %>%
                hchart("pyramid",hcaes(x = ., y = Freq)) %>%
                hc_title(text =i) 
        }
        # htmltools::tagList(hc) 
        do.call(tagList,hc)
    })
    output$Plot7 <- renderUI({
        hc <- list()
        j <- 0
        for(i in ym){
            hc[[(j <- j+1)]] <- data$id[data$ym==i] %>% 
                table() %>% 
                sort(decreasing = T) %>% 
                .[1:10] %>% 
                data.frame() %>%
                hchart("pyramid",hcaes(x = ., y = Freq)) %>%
                hc_title(text =i) 
        }
        # htmltools::tagList(hc)
        do.call(tagList,hc)
    })
    x <- reactiveValues()
    x$data <- 0
    observeEvent(input$main, {
        x$data <- 0
    })
    observeEvent(input$stat, {
        x$data <- 1
    })
    output$test <- renderUI({
        if(x$data==0){
            fluidPage(
                                        includeHTML("main.html"),
                                       # tags$img('src'="data.png"),
                                       HTML('<center>
            				<img src="data.png">
            			</center>'),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
            HTML('<center><a href="https://www.freepik.com/free-photos-vectors/background">Background vector created by Layerace - www.freepik.com</a></center>')
            )
        }else{
            tabsetPanel(
                tabPanel("month",
                         tabsetPanel(
                             tabPanel("point",highchartOutput("Plot1")),
                             tabPanel("times",highchartOutput("Plot2"))
                         )
                         # highchartOutput("Plot1")
                ),
                # tabPanel("Plot 2",highchartOutput("Plot2")),
                tabPanel("day",
                         tabsetPanel(
                             tabPanel("point",highchartOutput("Plot3")),
                             tabPanel("times",highchartOutput("Plot4"))
                         )
                         # highchartOutput("Plot3")
                ),
                # tabPanel("Plot 4",highchartOutput("Plot4")),
                tabPanel("weekday",highchartOutput("Plot5")),
                tabPanel("shop",htmlOutput("Plot6")),
                tabPanel("people",htmlOutput("Plot7"))
            )
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
