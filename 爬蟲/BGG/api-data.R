library(xml2)
load("C:/Users/USER/Desktop/BGG/id.RData")
na <- rep(NA,length(id))
data <- data.frame(id=na,
                   type=na,
                   name=na,
                   yearpublished=na,
                   minplayers=na,
                   maxplayers=na,
                   playingtime=na,
                   minplaytime=na,
                   maxplaytime=na,
                   minage=na,
                   users_rated=na,
                   average_rating=na,
                   bayes_average_rating=na,
                   total_owners=na,
                   total_traders=na,
                   total_wanters=na,
                   total_wishers=na,
                   total_comments=na,
                   total_weights=na,
                   average_weight=na,
                   types=na,
                   categories =na,
                   mechanics  =na,
                   family=na,
                   designers=na
                   )
#for(i in 1:length(id)){
# for(i in 103527:length(id)){
for(i in 112721:length(id)){
#for(i in 1:10){
  url <- paste("https://api.geekdo.com/xmlapi2/thing?id=",id[[i]],"&stats=1",sep="")
  game_xml<-read_xml(x=url)
  game_list <- as_list(game_xml)
  data$id[i] <- id[[i]]
  data$type[i] <- attr(game_list$items$item,"type")
  data$name[i] <- attr(game_list$items$item$name,"value")
  data$yearpublished[i] <- 
    as.integer(attr(game_list$items$item$yearpublished,"value"))
  data$minplayers[i] <- as.integer(attr(game_list$items$item$minplayers,"value"))
  data$maxplayers[i] <- as.integer(attr(game_list$items$item$maxplayers,"value"))
  data$playingtime[i] <- as.integer(attr(game_list$items$item$playingtime,"value"))
  data$minplaytime[i] <- as.integer(attr(game_list$items$item$minplaytime,"value"))
  data$maxplaytime[i] <- as.integer(attr(game_list$items$item$maxplaytime,"value"))
  data$minage[i] <- as.integer(attr(game_list$items$item$minage,"value"))
  data$users_rated[i] <- as.integer(attr(game_list$items$item$statistics$ratings$usersrated,"value"))
  data$average_rating[i] <- as.numeric(attr(game_list$items$item$statistics$ratings$average,"value"))
  data$bayes_average_rating[i] <- as.numeric(attr(game_list$items$item$statistics$ratings$bayesaverage,"value"))
  data$total_owners[i] <- as.integer(attr(game_list$items$item$statistics$ratings$owned,"value"))
  data$total_traders[i] <- as.integer(attr(game_list$items$item$statistics$ratings$trading,"value"))
  data$total_wanters[i] <- as.integer(attr(game_list$items$item$statistics$ratings$wanting,"value"))
  data$total_wishers[i] <- as.integer(attr(game_list$items$item$statistics$ratings$wishing,"value"))
  data$total_comments[i] <- as.numeric(attr(game_list$items$item$statistics$ratings$numcomments,"value"))
  data$total_weights[i] <- as.numeric(attr(game_list$items$item$statistics$ratings$numweights,"value"))
  data$average_weight[i] <- as.numeric(attr(game_list$items$item$statistics$ratings$averageweight,"value"))
  node <- xml_find_all(game_xml,"/items/item/statistics/ratings/ranks/rank")
  data$types[i] <- paste0(xml_attr(node,"name"),collapse=",")
  rm(node)
  node <- xml_find_all(game_xml,"/items/item/link")
  game_type<- xml_attr(node,"type")
  game_value<- xml_attr(node,"value")
  
  data$categories[i] <- paste0(game_value[which(game_type=="boardgamecategory")],collapse=",")
  data$mechanics[i] <- paste0(game_value[which(game_type=="boardgamemechanic")],collapse=",")
  data$family[i] <- paste0(game_value[which(game_type=="boardgamefamily")],collapse=",")
  data$designers[i] <- paste0(game_value[which(game_type=="boardgamedesigner")],collapse=",")
  Sys.sleep(2)
}

save(data,file = "C:/Users/USER/Desktop/BGG/data.RData")
