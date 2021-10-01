library(plumber)
# setwd("C:\\Users\\admin\\Desktop\\test\\highcharts-test3")
# setwd("G:\\我的雲端硬碟\\Q-point\\highcharts-test3")
pr <- plumber::plumb("api.R")
pr$run(port=8000)

