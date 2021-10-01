library(rvest)
library(Rwebdriver)
library(magrittr)
# run in cmd or Terminal
# java -Dwebdriver.chrome.driver=D:\chromedriver.exe -Dwebdriver.gecko.driver=D:\geckodriver.exe -jar D:\selenium-server-standalone-3.141.59.jar
# java -Dwebdriver.chrome.driver=D:\chromedriver.exe -Dwebdriver.gecko.driver=D:\geckodriver.exe -jar D:\selenium-server-standalone-3.4.0.jar


start_session(root = "http://localhost:4444/wd/hub/", browser ="chrome")


url <-"https://en.boardgamearena.com/account?redirect="
post.url(url=url)
Sys.sleep(2)
# 登入
# names <- list("oERTo")
# pw <- list("ndhuam313")


len <- 1000 #67688265
data <- data.frame(matrix(NA,nrow = 1,ncol = 34))
names(data) <- c("i",
"People",
"time",
"date",
"game_finish",
"mode",
"speed",
"side",
"player",
"Ranking",
"Thinking_time",
"VP_from_Military_Conflicts_Victory",
"VP_from_Military_Conflicts_Defeat",
"VP_from_Treasury_Contents",
"VP_from_Wonder",
"VP_from_Civilian_Structures",
"VP_from_Scientific_Structures",
"VP_from_Commercial_Structures",
"VP_from_Guilds",
"Constructed_stages_of_the_Wonder",
"Cards_discarded",
"Chained_constructions",
"Coins_spent_on_commerce",
"Coins_gained_through_commerce",
"Shields",
"Wonder_side_A",
"Wonder_ID",
"Civilian_Structures",
"Scientific_Structures",
"Guilds",
"Military_Structures",
"Commercial_Structures",
"Raw_Materials",
"Manufactured_Goods",
"ELO_rank")
j <- 1
#前1000沒有
# 69525007
for(i in 69519292:69529292){
  url <- paste0("https://en.boardgamearena.com/table?table=",i)
  post.url(url=url)

  Sys.sleep(2)

  bga_html <- page_source() %>%
    read_html(x = .,encoding="UTF-8")

  nothing <- html_nodes(bga_html,"#bga_fatal_error_descr") %>%
    length()

  if(nothing == 1){
    next
  }
  names <-  html_nodes(bga_html,"#table_name") %>%
    html_text()
  if(names!= "7 Wonders"){
    next
  }
  # num[(j <- j+1)] <- i
    
  times <- html_nodes(bga_html,"#estimated_duration") %>%
    html_text()
  
  game_abandonned <- html_nodes(bga_html,"#game_abandonned ")%>% html_attr("style")
  game_unranked <- html_nodes(bga_html,"#game_unranked ")%>% html_attr("style")
  game_cancelled <- html_nodes(bga_html,"#game_cancelled ")%>% html_attr("style")
  
  if("display: block;" %in% c(game_abandonned,game_unranked,game_cancelled)){
    next
  }
  dates <- html_nodes(bga_html,"#creationtime") %>%
    html_text()
  
  game_finish <- html_nodes(bga_html,"#table_duration_label") %>% 
    html_text() 
  
  if(game_finish=="Estimated duration"){
    next
  }
  if(game_finish=="Game has started from"){
    next
  }
  people <- html_nodes(bga_html,".name") %>% length()
  
  data[j:(j+people-1),"i"] <- i
  data[j:(j+people-1),"People"] <- people
  data[j:(j+people-1),"time"] <- times
  data[j:(j+people-1),"date"] <- dates
  data[j:(j+people-1),"game_finish"] <- game_finish
  
  data[j:(j+people-1),"player"] <- html_nodes(bga_html,".name") %>% 
    html_text()
    # paste(collapse = "/")
  data[j:(j+people-1),"ELO_rank"]html_nodes(bga_html,"#game_result .gamerank_value") %>% html_text()
  data[j:(j+people-1),10:34] <- html_nodes(bga_html,"td")  %>% html_text() %>% matrix(ncol = 26)%>% .[,1:25]
  data[j:(j+people-1),"mode"] <-  html_nodes(bga_html,xpath = '//*[@id="mob_gameoption_201_displayed_value"]') %>% html_text()
  data[j:(j+people-1),"speed"] <-  html_nodes(bga_html,xpath = '//*[@id="mob_gameoption_200_displayed_value"]') %>% html_text()
  data[j:(j+people-1),"side"] <-  html_nodes(bga_html,xpath = '//*[@id="mob_gameoption_100_displayed_value"]') %>% html_text()
  
  
  # html_nodes(bga_html,"#table_stats .row-value")
  # html_nodes(bga_html,"td")  %>% html_text()
  
  j <- j+people
}

data <- data[data$Ranking!="not ranked ()",]

f1 <- function(x){
  return(x[1])
}
fmax <- function(x){
  return(x[length(x)])
}

point <- data$Ranking %>%
  strsplit(split = "[^0-9]") %>%
  sapply(FUN = fmax)

data$Ranking <- data$Ranking %>%
  strsplit(split = "[^0-9]") %>%
  sapply(f1)

data <- cbind(data,point)

save(data ,file = "C:/Users/admin/Desktop/data_test.RData")

