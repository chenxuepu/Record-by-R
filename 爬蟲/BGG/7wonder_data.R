library(magrittr)
# data <- read.csv(file = "D:/7wondersDataMinning-master/datasets/7wonders.csv",sep = ",")
data_7wonders_3 <- read.csv(file = "D:/7wondersDataMinning-master/datasets/7wonders3players.csv",sep = ",",header = F)
data_7wonders_4 <- 
  read.csv(file = "D:/7wondersDataMinning-master/datasets/7wonders4players.csv",sep = ";",header = F)
data_7wonders_5 <- 
  read.csv(file = "D:/7wondersDataMinning-master/datasets/7wonders5players.csv",sep = ";",header = F)
data_7wonders_6 <- 
  read.csv(file = "D:/7wondersDataMinning-master/datasets/7wonders6players.csv",sep = ";",header = F)
data_7wonders_7 <- 
  read.csv(file = "D:/7wondersDataMinning-master/datasets/7wonders7players.csv",sep = ";",header = F)


x <- data_7wonders_3[,1]
data <- matrix(NA,1500,28) %>%
  data.frame()
names(data) <- c("People","team","Ranking","point",as.character(x)[2:25])
# data_7wonders_3
k <- 0 
l <- 0 #team
for(i in 1:65){
  l <- l+1
  for(j in 1:3){
    n <- 4*i-2+j
    data[(k <- k+1),5:28] <- data_7wonders_3[2:25,n]
    data[k,"People"] <- 3
    data[k,"team"] <- l
    data[k,"Ranking"] <- j
    data[k,"point"] <- data_7wonders_3[1,n] %>% 
      as.character() %>%
      substr(x = .,start = nchar(.)-2,stop = nchar(.)-1)
  }
} 



# data_7wonders_4
for(i in 1:55){
  l <- l+1
  for(j in 1:4){
    n <- 5*i-3+j
    data[(k <- k+1),5:28] <- data_7wonders_4[2:25,n]
    data[k,"People"] <- 4
    data[k,"team"] <- l
    data[k,"Ranking"] <- j
    data[k,"point"] <- data_7wonders_4[1,n] %>% 
      as.character() %>%
      substr(x = .,start = nchar(.)-2,stop = nchar(.)-1)
  }
}

# data_7wonders_5
for(i in 1:58){
  l <- l+1
  for(j in 1:5){
    n <- 6*i-4+j
    data[(k <- k+1),5:28] <- data_7wonders_5[2:25,n]
    data[k,"People"] <- 5
    data[k,"team"] <- l
    data[k,"Ranking"] <- j
    data[k,"point"] <- data_7wonders_5[1,n] %>% 
      as.character() %>%
      substr(x = .,start = nchar(.)-2,stop = nchar(.)-1)
  }
}

# data_7wonders_6
for(i in 1:52){
  l <- l+1
  for(j in 1:6){
    n <- 7*i-5+j
    data[(k <- k+1),5:28] <- data_7wonders_6[2:25,n]
    data[k,"People"] <- 6
    data[k,"team"] <- l
    data[k,"Ranking"] <- j
    data[k,"point"] <- data_7wonders_6[1,n] %>% 
      as.character() %>%
      substr(x = .,start = nchar(.)-2,stop = nchar(.)-1)
  }
}

# data_7wonders_7
for(i in 1:62){
  l <- l+1
  for(j in 1:7){
    n <- 8*i-6+j
    data[(k <- k+1),5:28] <- data_7wonders_7[2:25,n]
    data[k,"People"] <- 7
    data[k,"team"] <- l
    data[k,"Ranking"] <- j
    data[k,"point"] <- data_7wonders_7[1,n] %>% 
      as.character() %>%
      substr(x = .,start = nchar(.)-2,stop = nchar(.)-1)
  }
}

data_7wonder <- data[!is.na(data$People),]

save(data_7wonder,file = "C:/Users/admin/Desktop/data_7wonder.RData")
