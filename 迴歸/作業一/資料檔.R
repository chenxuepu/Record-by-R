library(dplyr)
setwd("G:\\我的雲端硬碟\\統計專論\\作業一")
Econ9815 <- read.table("Econ9815.txt")
colnames(Econ9815) <- c("Year",
                        "S&P 500 return",
                        "Treasury rate",
                        "CMT rate",
                        "ΔMB",
                        "INFL rate",
                        "ΔIP",
                        "ΔGDP",
                        "GDP per capita",
                        "CFNAI",
                        "Unemployment rate")

finaldata9815 <- read.table("finaldata9815.txt")
# readLines("finaldata9815.txt")
finaldata <- data.frame(matrix(ncol = 21))
for(i in 1:(nrow(finaldata9815)/7)){
  for(j in 1:7){
    finaldata[i,(3*j-2):(3*j)] <- finaldata9815[(i-1)*7+j,]
  }
}
colnames(finaldata) <- c("gvkey",
                         "Year",
                         "LTR rating number",
                         "sic",
                         "IC1",
                         "IC2",
                         "IC3",
                         "IC4",
                         "OM",
                         "LDL",
                         "TDL",
                         "DP",
                         "MBA",
                         "RDA",
                         "REA",
                         "CEA",
                         "CBA",
                         "TA",
                         "beta",
                         "sigma",
                         "size")
finaldata[,c("sic","LDL")] <- NULL
finaldata[finaldata==-9999] <- NA
finaldata$IC <- finaldata$IC1+finaldata$IC2+finaldata$IC3+finaldata$IC4
finaldata[,c("IC1","IC2","IC3","IC4")] <- NULL
data <- left_join(finaldata,Econ9815,by = "Year")
# data[data==-9999] <- NA
for(i in 1:ncol(data)){
  cat(colnames(data)[i],":",sum(is.na(data[,i])),"\n")
}
# sum(is.na(data[,1]))


data$'LTR rating number' <- sapply(data$'LTR rating number',FUN = function(x){
  if(is.na(x)){
    categories <- NA
  }else if(x==2){
    categories <- "aaa"
  }else if(x %in% c(4,5,6)){
    categories <- "aa"
  }else if(x %in% c(7,8,9)){
    categories <- "a"
  }else if(x %in% c(10,11,12)){
    categories <- "bbb"
  }else if(x %in% c(13,14,15)){
    categories <- "bb"
  }else if(x %in% c(16,17,18)){
    categories <- "b"
  }else if(x %in% c(19,20,21)){
    categories <- "ccc"
  }else if(x ==23){
    categories <- "cc"
  }else if(x %in% c(24,26)){
    categories <- "c"
  }else if(x %in% c(27,29)){
    categories <- "d"
  }   
  return(categories)
})

data$`LTR rating categories` <- data$'LTR rating number'
data$'LTR rating number' <- NULL
# data[data==-9999] <- NA
write.csv(data,"data.csv",row.names = F)
for(i in 1:10){
  data_x <- data[,c(1:15,15+i,26)]
  write.csv(data_x,paste0("data_",i,".csv"),row.names = F)
}
