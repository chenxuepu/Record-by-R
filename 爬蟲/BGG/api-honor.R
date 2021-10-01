# https://boardgamegeek.com/browse/boardgamehonor/page/
page <- 34
library(rvest) ##載入
honor <- data.frame(name = rep(NA,page*100),url = rep(NA,page*100),type = rep(NA,page*100))
# honor <- rep(NA,page*100)
for(i in 1:page){
  url <- paste0("https://boardgamegeek.com/browse/boardgamehonor/page/",i)
  honor_html <- read_html(x=url)
  
  honor_name <- html_nodes(honor_html,".forum_table a") %>%
    html_text()
  honor$name[((i-1)*100+1):((i-1)*100+length(honor_name))] <- honor_name
  
  honor_url <- html_nodes(honor_html,".forum_table a") %>%
    html_attr("href")
  honor$url[((i-1)*100+1):((i-1)*100+length(honor_url))] <- honor_url
  
  Sys.sleep(2)
}

honor <- honor[-which(is.na(honor$name)),]



honor_data <- data.frame(id = NA,type = NA,honor = NA)
k <- 1
for(i in 1:nrow(honor)){
  url <- paste0("https://boardgamegeek.com/",honor$url[i])
  honor_html <- read_html(x=url)
  honor_length <- xml_length(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(honor_html, 2), 1), 3), 2), 2), 2), 1), 1), 2), 1), 1), 1), 2), 2), 2), 1), 1), 1), 1), 1), 1), 3), 2), 2))
  which_type <- xml_length(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(honor_html, 2), 1), 3), 2), 2), 2), 1), 1), 2), 1), 1), 1), 2), 2), 2), 1), 1), 1), 1), 1), 1))-2
  type <- html_text(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(honor_html, 2), 1), 3), 2), 2), 2), 1), 1), 2), 1), 1), 1), 2), 2), 2), 1), 1), 1), 1), 1), 1), which_type), 2), 2), 1))
  type <- gsub("\t","",type)
  type <- gsub("\n","",type)
  honor$type[i] <- type
  for(j in 1:honor_length){
    honor_data[k,] <- NA
    honor_data$id[k] <- xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(honor_html, 2), 1), 3), 2), 2), 2), 1), 1), 2), 1), 1), 1), 2), 2), 2), 1), 1), 1), 1), 1), 1), 3), 2), 2),j),1) %>%
      html_attr("href") %>%
      strsplit("/") %>%
      .[[1]] %>%
      .[3]
    honor_data$type[k] <- type
    honor_data$honor[k] <- honor$name[i]
    k <- k+1
  }
  Sys.sleep(2)
}



save(honor,honor_data,file = "C:/Users/USER/Desktop/BGG/honor.RData")

