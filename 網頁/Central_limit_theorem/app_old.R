library(shiny)
library(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", label = "dist", choices = list("Normal(mean,Sd^2)" = 1, "Uniform(a,b)" = 2,"Binomial(n,prob)"= 3)),
      uiOutput("mu"),
      uiOutput("sd"),
      uiOutput("min"),
      uiOutput("max"),
      uiOutput("size"),
      uiOutput("p"),
      numericInput("n", label = ("sample size"), value = 10,min=1),
      sliderInput("k", label = "repeat times", min = 1000, max = 2000,value = 1000, step= 10),
      radioButtons("random", label = "need seed?", choices = list("YES" = 1, "NO" = 2)),
      uiOutput("seed"),
      actionButton("action", label = "Action")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

var<-function(x){
  y<-mean((x-mean(x))^2)
  return(y)
}






server <- function(input, output) {
  output$mu<-renderUI(
    
      if (input$dist == "1")
      {
        numericInput("mu", label = ("mean"), value = 0)
      }
  )
  output$sd<-renderUI(
    
    if (input$dist == "1")
    {
      sliderInput("sd", label = "Standard deviation", min = 0.5, max = 1.5, value = 1, step= 0.01)
    }
  )
  output$min<-renderUI(
    
    if (input$dist == "2")
    {
      numericInput("min", label = ("a"), value = 0)
    }
  )
  output$max<-renderUI(
    
    if (input$dist == "2")
    {
      numericInput("max", label = ("b"), value = 1)
    }
  )
  output$size<-renderUI(
    
    if (input$dist == "3")
    {
      numericInput("size", label = ("n"), value = 1)
    }
  )
  output$p<-renderUI(
    
    if (input$dist == "3")
    {
      sliderInput("p", label = "prob", min = 0, max = 1, value = 0.5, step= 0.01)
    }
  )
  output$seed<-renderUI(
    
    if (input$random == "1")
    {
      numericInput("seed", label = ("seed"), value = 1,min=1)
    }
  )
  output$plot<-renderPlot({
    if (input$action == 0){
      return()
    }
    isolate(
      if(input$dist == "1"){
        if(input$random == "1"){
          #固定隨機序列
          set.seed(input$seed)
        }
        x<-matrix(rnorm(input$n*input$k,mean=input$mu,sd=input$sd),input$n,input$k)
        data=data.frame(x=apply(x,2,mean))
        plot1<-ggplot(data=data, aes(x = x))+geom_histogram(aes(y=..density..),bins=20,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+annotate("text", x= Inf, y= Inf,label="mean",hjust=2,vjust=2,size=7)
        #mu<-paste("mean = ",round(mean(data$x),3))
        #sd<-paste("sd = ",round((mean((data$x-mean(data$x))^2))^(1/2),3))
        #plot1+annotate("text", x= Inf, y= Inf,label= c(mu,sd),hjust=c(1,1),vjust=c(1,2),size=5)
        mu<-input$mu
        sd<-input$sd/((input$n)^(1/2))
        z<-function(x){
          y<-dnorm(x,mu,sd)
          return(y)
        }
        plot1<-plot1+stat_function(fun=z,col="red")
        data_var=data.frame(x=apply(x,2,var))
        plot2<-ggplot(data=data_var, aes(x = x))+geom_histogram(aes(y=..density..),bins=20,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+annotate("text", x= Inf, y= Inf,label="sd",hjust=2,vjust=2,size=7)
        multiplot(plot1,plot2)
      }else if(input$dist == "2"){
        if(input$random == "1"){
          #固定隨機序列
          set.seed(input$seed)
        }
        x<-matrix(runif(input$n*input$k,min = input$min,max =input$max),input$n,input$k)
        data=data.frame(x=apply(x,2,mean))
        plot1<-ggplot(data=data, aes(x = x))+geom_histogram(aes(y=..density..),bins=20,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+annotate("text", x= Inf, y= Inf,label="mean",hjust=2,vjust=2,size=7)
        #mu<-paste("mean = ",round(mean(data$x),3))
        #sd<-paste("sd = ",round((mean((data$x-mean(data$x))^2))^(1/2),3))
        #plot1+annotate("text", x= Inf, y= Inf,label= c(mu,sd),hjust=c(1,1),vjust=c(1,2),size=5)
        mu<-(input$max+input$min)/2
        sd<-sqrt((input$max-input$min)^2/12)/((input$n)^(1/2))
        z<-function(x){
          y<-dnorm(x,mu,sd)
          return(y)
        }
        plot1<-plot1+stat_function(fun=z,col="red")
        data_var=data.frame(x=apply(x,2,var))
        plot2<-ggplot(data=data_var, aes(x = x))+geom_histogram(aes(y=..density..),bins=20,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+annotate("text", x= Inf, y= Inf,label="sd",hjust=2,vjust=2,size=7)
        multiplot(plot1,plot2)
      }else if(input$dist == "3"){
        if(input$random == "1"){
          #固定隨機序列
          set.seed(input$seed)
        }
        x<-matrix(rbinom(input$n*input$k,input$size,input$p),input$n,input$k)
        data=data.frame(x=apply(x,2,mean))
        plot1<-ggplot(data=data, aes(x = x))+geom_histogram(aes(y=..density..),bins=20,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+annotate("text", x= Inf, y= Inf,label="mean",hjust=2,vjust=2,size=7)
        #mu<-paste("mean = ",round(mean(data$x),3))
        #sd<-paste("sd = ",round((mean((data$x-mean(data$x))^2))^(1/2),3))
        #plot1+annotate("text", x= Inf, y= Inf,label= c(mu,sd),hjust=c(1,1),vjust=c(1,2),size=5)
        mu<-input$size*input$p
        sd<-sqrt(input$size*input$p*(1-input$p))/((input$n)^(1/2))
        z<-function(x){
          y<-dnorm(x,mu,sd)
          return(y)
        }
        plot1<-plot1+stat_function(fun=z,col="red")
        data_var=data.frame(x=apply(x,2,var))
        plot2<-ggplot(data=data_var, aes(x = x))+geom_histogram(aes(y=..density..),bins=20,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+annotate("text", x= Inf, y= Inf,label="sd",hjust=2,vjust=2,size=7)
        multiplot(plot1,plot2)
      }
    )}
    
  )
  
}

shinyApp(ui = ui, server = server)