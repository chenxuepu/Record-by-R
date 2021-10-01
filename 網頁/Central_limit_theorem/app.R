library(shiny)
library(ggplot2)

var<-function(x){
  y<-mean((x-mean(x))^2)
  return(y)
}



ui <- fluidPage(
  withMathJax(),
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", label = "dist", choices = list("Normal(\\(\\mu\\),\\(\\sigma^2\\))" = 1, "Uniform(a,b)" = 2,"Binomial(n,prob)"= 3)),
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
      tabsetPanel(
        tabPanel("X","red line is pdf/pmf of X", plotOutput("plot1")),
        tabPanel("\\(\\bar{X}\\)","red line is pdf/pmf of \\(\\bar{X}\\)", plotOutput("plot2")),
        tabPanel("\\(\\bar{X}\\) var", plotOutput("plot3"))
      )
    )
  )
)








server <- function(input, output) {
  output$mu<-renderUI(
    
      if (input$dist == "1")
      {
        numericInput("mu", label = withMathJax("\\(\\mu\\)"), value = 0)
      }
  )
  output$sd<-renderUI(
    
    if (input$dist == "1")
    {
      sliderInput("sd", label = withMathJax("\\(\\sigma\\)"), min = 0.5, max = 1.5, value = 1, step= 0.01)
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
  x <- reactive({
    if (input$action == 0){
      return()
    }
    isolate({
      if(input$random == "1"){
        #固定隨機序列
        set.seed(input$seed)
      }
      if(input$dist == "1"){
        x<-rnorm(input$n*input$k,mean=input$mu,sd=input$sd)
        # mu<-input$mu
        # sd<-input$sd/((input$n)^(1/2))
        # z<-function(x){
        #   y<-dnorm(x,mu,sd)
        #   return(y)
        # }
      }else if(input$dist == "2"){
        x<-runif(input$n*input$k,min = input$min,max =input$max)
        # mu<-(input$max+input$min)/2
        # sd<-sqrt((input$max-input$min)^2/12)/((input$n)^(1/2))
        # z<-function(x){
        #   y<-dnorm(x,mu,sd)
        #   return(y)
        # }
      }else if(input$dist == "3"){
        x<-rbinom(input$n*input$k,input$size,input$p)
        # mu<-input$size*input$p
        # sd<-sqrt(input$size*input$p*(1-input$p))/((input$n)^(1/2))
        # z<-function(x){
        #   y<-dnorm(x,mu,sd)
        #   return(y)
        # }
      }
    })
  }) 
  mu<-reactive({
    if(input$dist == "1"){
      mu<-input$mu
    }else if(input$dist == "2"){
      mu<-(input$max+input$min)/2
    }else if(input$dist == "3"){
      mu<-input$size*input$p
    }  
  })
  sd<-reactive({
    if(input$dist == "1"){
      sd<-input$sd/((input$n)^(1/2))
    }else if(input$dist == "2"){
      sd<-sqrt((input$max-input$min)^2/12)/((input$n)^(1/2))
    }else if(input$dist == "3"){
      sd<-sqrt(input$size*input$p*(1-input$p))/((input$n)^(1/2))
    }  
  })
  bin<-reactive({
    bin<-10+ceiling(input$k/100)
  })
  
  
  # observe({
  #   if (input$action == 0){
  #     return()
  #   }
  #   isolate({
  #     if(input$random == "1"){
  #       #固定隨機序列
  #       set.seed(input$seed)
  #     }
  #     if(input$dist == "1"){
  #       x<-rnorm(input$n*input$k,mean=input$mu,sd=input$sd)
  #       mu<-input$mu
  #       sd<-input$sd/((input$n)^(1/2))
  #       z<-function(x){
  #         y<-dnorm(x,mu,sd)
  #         return(y)
  #       }
  #     }else if(input$dist == "2"){
  #       x<-runif(input$n*input$k,min = input$min,max =input$max)
  #       mu<-(input$max+input$min)/2
  #       sd<-sqrt((input$max-input$min)^2/12)/((input$n)^(1/2))
  #       z<-function(x){
  #         y<-dnorm(x,mu,sd)
  #         return(y)
  #       }
  #     }else if(input$dist == "3"){
  #       x<-rbinom(input$n*input$k,input$size,input$p)
  #       mu<-input$size*input$p
  #       sd<-sqrt(input$size*input$p*(1-input$p))/((input$n)^(1/2))
  #       z<-function(x){
  #         y<-dnorm(x,mu,sd)
  #         return(y)
  #       }
  #     }
  #   })
  # })
  output$plot2<-renderPlot({
    if (input$action == 0){
      return()
    }
    isolate({
      mu<-mu()
      sd=sd()
      bin<-bin()
      z<-function(x){
        y<-dnorm(x,mu,sd)
        return(y)
      }
      y<-matrix(x(),input$n,input$k)
      data=data.frame(x=apply(y,2,mean))
      ggplot(data=data, aes(x = x))+geom_histogram(aes(y=..density..),bins=bin,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+stat_function(fun=z,col="red")
    })
  })
  output$plot3<-renderPlot({
    if (input$action == 0){
      return()
    }
    isolate({
      bin<-bin()
      y<-matrix(x(),input$n,input$k)
      data_var=data.frame(x=apply(y,2,var))
      ggplot(data=data_var, aes(x = x))+geom_histogram(aes(y=..density..),bins=bin,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")
    })
  })
  output$plot1<-renderPlot({
    if (input$action == 0){
      return()
    }
    isolate({
      data_Ori=data.frame(x=x())
      mu<-input$mu
      sd<-input$sd
      bin<-bin()
      if(input$dist == "1"){
        dist<-function(x){
          y<-dnorm(x,mean=mu,sd=sd)
          return(y)
        }
        ggplot(data=data_Ori, aes(x = x))+geom_histogram(aes(y=..density..),bins=bin,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+stat_function(fun=dist,col="red")
      }else if(input$dist == "2"){
        min<-input$min
        max<-input$max
        dist<-function(x){
          y<-dunif(x,min = min,max =max)
          return(y)
        }#histogram ,bins=20
        ggplot(data=data_Ori, aes(x = x))+geom_histogram(aes(y=..density..),bins=bin,colour="black", fill="white") +geom_density(alpha=.2, fill="#000000")+stat_function(fun=dist,col="red")
      }else if(input$dist == "3"){
        size<-input$size
        p<-input$p
        dist<-function(x){
          y<-dbinom(x,size,p)
          return(y)
        }
        
        ggplot(data=data_Ori, aes(x = x))+
          geom_histogram(aes(y=..density..),bins=bin,colour="black", fill="white") +
          geom_density(bw="bcv",alpha=.2, fill="#000000")+
          geom_point(aes(y=dist(x)), colour="red")
      } 
      
    })}
    
  )
  
}

shinyApp(ui = ui, server = server)