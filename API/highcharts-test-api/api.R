# api.R


library(highcharter)
library(xts)
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(jsonlite)

# data <- read.csv("point.csv")
# data$date <- data$date %>% as.POSIXct()
# data$ym <- format(data$date,format="%Y-%m")
# data$ymd <- format(data$date,format="%Y-%m-%d")
# data_rm <- group_by(data,ymd) %>%
#     summarise(point=sum(point),times = n()) 
# data_rm$ymd <- data_rm$ymd %>% as.POSIXct()
# data$group <- sapply(data$date, function(x){
#     if(format(x,format="%u") %in% c("6","7") ){
#         week <- "holiday"
#     }else{
#         week <- "normal day"
#     }
#     y <- format(x,format="%H") %>% as.numeric()
#     if(y<12){
#         time <- "morning"
#     }else if(y<18){
#         time <- "noon"
#     }else{
#         time <- "night"
#     }
#     paste(week,time,sep = "-")
# })
# ym <- data$ym %>% unique()

#' @get /test
#' @serializer text
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    group_by(data,ym) %>%
        summarise(point=sum(point)) %>%
        hchart("column",hcaes(x = ym, y = point)) %>%
        hc_title(text ="point") %>%
        renderTags() %>%
        .$html
}



#' @get /Plot1
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    group_by(data,ym) %>%
        summarise(point=sum(point)) %>%
        hchart("column",hcaes(x = ym, y = point)) %>%
        hc_title(text ="point") 
}



#' @get /Plot2
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    group_by(data,ym) %>%
        summarise(times=n()) %>%
        hchart("column",hcaes(x = ym, y = times)) %>%
        hc_title(text ="times")
}


#' @get /Plot3
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ymd <- format(data$date,format="%Y-%m-%d")
    data_rm <- group_by(data,ymd) %>%
        summarise(point=sum(point),times = n()) 
    data_rm$ymd <- data_rm$ymd %>% as.POSIXct()
    xts(data_rm$point,order.by = data_rm$ymd) %>%
        hchart(type = "bar") %>%
        hc_title(text ="point") 
}



#' @get /Plot4
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ymd <- format(data$date,format="%Y-%m-%d")
    data_rm <- group_by(data,ymd) %>%
        summarise(point=sum(point),times = n()) 
    data_rm$ymd <- data_rm$ymd %>% as.POSIXct()
    xts(data_rm$times,order.by = data_rm$ymd) %>%
        hchart(type = "bar") %>%
        hc_title(text ="times") 
    
}



#' @get /Plot5
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
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
    group_by(data,ym,group) %>%
        summarise(point=mean(point)) %>%
        hchart( "line",hcaes(x = ym, y = point,group=group)) %>%
        hc_title(text ="point") %>%
        hc_tooltip(crosshairs = TRUE,shared = TRUE) 
    
    
}


#' @get /Plot6
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    ym <- data$ym %>% unique()
    hc <- createWidget(name = "shop",x = NULL)
    for(i in ym){
        hc <- data$shop[data$ym==i] %>% 
            table() %>% 
            sort(decreasing = T) %>% 
            .[1:10] %>% 
            data.frame() %>%
            hchart("pyramid",hcaes(x = ., y = Freq)) %>%
            hc_title(text =i) %>%
            prependContent(x = hc,... = .)
    }
    # htmltools::tagList(hc) 
    # do.call(tagList,hc)
    hc
}



#' @get /Plot7
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    ym <- data$ym %>% unique()
    hc <- createWidget(name = "user",x = NULL)
    for(i in ym){
        hc <- data$id[data$ym==i] %>% 
            table() %>% 
            sort(decreasing = T) %>% 
            .[1:10] %>% 
            data.frame() %>%
            hchart("pyramid",hcaes(x = ., y = Freq)) %>%
            hc_title(text =i) %>%
            prependContent(x = hc,... = .)
    }
    # htmltools::tagList(hc) 
    # do.call(tagList,hc)
    hc
}


#' @get /Plot8
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    ym <- data$ym %>% unique()
    hc <- createWidget(name = "shop",x = NULL)
    for(i in ym){
        hc <- data$shop[data$ym==i] %>% 
            table() %>% 
            sort(decreasing = T) %>% 
            .[1:10] %>% 
            data.frame() %>%
            hchart("bar",hcaes(x = ., y = Freq,color =  Freq)) %>%
            hc_title(text =i) %>%
            prependContent(x = hc,... = .)
    }
    # htmltools::tagList(hc) 
    # do.call(tagList,hc)
    hc
}



#' @get /Plot9
#' @serializer htmlwidget
function(){
    data <- read.csv("point.csv")
    data$date <- data$date %>% as.POSIXct()
    data$ym <- format(data$date,format="%Y-%m")
    ym <- data$ym %>% unique()
    hc <- createWidget(name = "user",x = NULL)
    for(i in ym){
        hc <- data$id[data$ym==i] %>% 
            table() %>% 
            sort(decreasing = T) %>% 
            .[1:10] %>% 
            data.frame() %>%
            hchart("bar",hcaes(x = ., y = Freq,color =  Freq)) %>%
            hc_title(text =i) %>%
            prependContent(x = hc,... = .)
    }
    # htmltools::tagList(hc) 
    # do.call(tagList,hc)
    hc
}



