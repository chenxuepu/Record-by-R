library(dplyr)
setwd("G:\\我的雲端硬碟\\統計專論\\作業二")
XData2018IN <- readLines("XData2018IN.txt")
XData2018OUT <- readLines("XData2018OUT.txt")
colname <- c("Instrument ID",
                        "Date of Obligor Default",
                        "LGD",
                        "Debt cushion",
                        "Industry Distress",
                        "Ranking_1",
                        "Ranking_2",
                        "Ranking_3",
                        "Collateral",
                        "Instrument type_1",
             "Instrument type_2",
             "Instrument type_3",
             "Instrument type_4",
             "Instrument type_5",
             "Utility industry")
XData2018IN <- strsplit(XData2018IN,split = "[ ]+")
XData2018OUT <- strsplit(XData2018OUT,split = "[ ]+")


data_in <- data.frame(matrix(ncol = 15)) %>%
  `colnames<-`(colname)

data_out <- data.frame(matrix(ncol = 15)) %>%
  `colnames<-`(colname)

for(i in 1:length(XData2018IN)){
  if(i%%5==4){
    data_in[i%/%5+1,13:15] <- XData2018IN[[i]][2:4]
  }else if(i%%5==0){
    
  }else{
    data_in[i%/%5+1,((i%%5-1)*4+1):((i%%5)*4)] <- XData2018IN[[i]][2:5]
  }
}

for(i in 1:length(XData2018OUT)){
  if(i%%5==4){
    data_out[i%/%5+1,13:15] <- XData2018OUT[[i]][2:4]
  }else if(i%%5==0){
    
  }else{
    data_out[i%/%5+1,((i%%5-1)*4+1):((i%%5)*4)] <- XData2018OUT[[i]][2:5]
  }
}


setwd("G:/我的雲端硬碟/統計專論/作業二")
write.csv(data_in,"data_in.csv",row.names = F)
write.csv(data_out,"data_out.csv",row.names = F)
