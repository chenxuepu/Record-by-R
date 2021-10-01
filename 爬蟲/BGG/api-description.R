library(xml2)
load("C:/Users/USER/Desktop/BGG/data.RData")
data <- data[-which(is.na(data$type)),]
description <- rep(NA,nrow(data))
data <- cbind(data,description)


for(i in 1:nrow(data)){
  url <- paste0("https://api.geekdo.com/xmlapi2/thing?id=",data$id[i])
  Description_xml <- read_xml(x=url)
  Description_list <- as_list(Description_xml)
  data$description[i] <- Description_list$items$item$description[[1]]
}

save(data,file = "C:/Users/USER/Desktop/BGG/data.RData")