#必須
library(shiny)
#畫圖用
library(ggplot2)

#顯示介面
ui <- fluidPage(
  #標題
  withMathJax(),
  titlePanel("law of large number"),
  sidebarLayout(
    #左邊的操作介面
    sidebarPanel(
      #多個中選一個，可以不用等於數值，文字也可以
      radioButtons("dist", label = "dist", choices = list("Normal(\\(\\mu\\),\\(\\sigma^2\\))" = 1, "Uniform(a,b)" = 2,"Binomial(n,prob)"= 3)),
      #根據上面的dist去控制uiOutput要顯示哪幾個
      uiOutput("mu"),
      uiOutput("sd"),
      uiOutput("min"),
      uiOutput("max"),
      uiOutput("size"),
      uiOutput("p"),
      #樣本數
      numericInput("n", label = ("sample size"), value = 10,min=2),
      #用set.seed(input$seed)來改變隨機的序列
      radioButtons("random", label = "need seed?", choices = list("YES" = 1, "NO" = 2)),
      uiOutput("seed"),
      #numericInput("seed", label = ("seed"), value = 1,min=1),
      #按鈕
      actionButton("action", label = "Action")
    ),
    #右邊的顯示介面
    mainPanel(
      #顯示圖形
      plotOutput("plot")
    )
  )
)






#程式
server <- function(input, output) {
  #normal的兩個變數
  output$mu<-renderUI(
      
      if (input$dist == "1")
      {
        numericInput("mu", label = withMathJax("\\(\\mu\\)"), value = 0)
      }
  )
  output$sd<-renderUI(
    
    if (input$dist == "1")
    {
      sliderInput("sd", label =  withMathJax("\\(\\sigma\\)"), min = 0.5, max = 1.5, value = 1, step= 0.01)
    }
  )
  #Uniform的兩個變數
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
  #Binomial的兩個變數
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
  #seed
  output$seed<-renderUI(
    
    if (input$random == "1")
    {
      numericInput("seed", label = ("seed"), value = 1,min=1)
    }
  )
  #產生圖
  output$plot<-renderPlot({
    #如果還沒按下按鈕，就不輸出圖。
    if (input$action == 0){
      return()
    }
    #隔離，每次按下按紐，跑一次isolate裡面的程式
    isolate(
      #這邊應該可以更簡短，但是我用的時候不知道出了什麼BUG。
      #所以就寫的比較長了一點
      if(input$dist == "1"){
        if(input$random == "1"){
          #固定隨機序列
          set.seed(input$seed)
        }
        #產生n個normal，根據要求的變數
        x<-rnorm(input$n,mean=input$mu,sd=input$sd)
        #建立長度為n的向量y
        y<-rep(0,input$n)
        #將前i個x做平均,存入第i個y
        for(i in 1:input$n){
          y[i]<-mean(x[1:i])
        }
        #建立ggplot需要的格式
        data=data.frame(n=seq(1,input$n,1),mean=y)
        #最後產生圖的函數裡，不能有input，不然隔離會失效。
        #只要改變那個在函數裡的input，不用按按鈕，圖的output也會改變。
        mu<-input$mu
        #產生圖
        ggplot(data=data, aes(x = n, y =mean))+geom_line()+ geom_hline(aes(yintercept=mu))
      #下面就只是一樣的事，不同dist而已  
      }else if(input$dist == "2"){
        if(input$random == "1"){
          #固定隨機序列
          set.seed(input$seed)
        }
        x<-runif(input$n,min = input$min,max =input$max)
        y<-rep(0,input$n)
        for(i in 1:input$n){
          y[i]<-mean(x[1:i])
        }
        data=data.frame(n=seq(1,input$n,1),mean=y)
        mu<-(input$min+input$max)/2
        ggplot(data=data, aes(x = n, y =mean))+geom_line()+ geom_hline(aes(yintercept=mu))
        
      }else if(input$dist == "3"){
        if(input$random == "1"){
          #固定隨機序列
          set.seed(input$seed)
        }
        x<-rbinom(input$n,input$size,input$p)
        y<-rep(0,input$n)
        for(i in 1:input$n){
          y[i]<-mean(x[1:i])
        }
        data=data.frame(n=seq(1,input$n,1),mean=y)
        mu<-input$size*input$p
        ggplot(data=data, aes(x = n, y =mean))+geom_line()+ geom_hline(aes(yintercept=mu))
        
      }
    )
  }
    
  )
  
}

#如果要寫在一個檔案，就要加這行。
shinyApp(ui = ui, server = server)