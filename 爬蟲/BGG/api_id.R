# find all id
#page need to check
page <- 1128
library(rvest) ##載入

x_3<-function(x){
  return(x[3])
}



id<-rep(NA,page*100)
j <- 1
for(i in 1:page){
  url <- paste("https://www.boardgamegeek.com/browse/boardgame/page/",i,sep="")
  id_html<-read_html(x=url)
  id_url <- html_nodes(id_html,".collection_objectname a")%>% 
    html_attr("href")
  id_list <- strsplit(id_url,"/")
  length(id_list)
  id[j:(j+length(id_list)-1)]<-lapply(id_list,x_3)
  j <- j+length(id_list)
  Sys.sleep(2)
}

id <- id[-which(is.na(id))]
save(id ,file = "C:/Users/USER/Desktop/BGG/id.RData")
