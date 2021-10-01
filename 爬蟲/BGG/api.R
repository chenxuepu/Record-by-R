# #library(jsonlite)
# library(xml2)
# x<-read_xml("https://api.geekdo.com/xmlapi2/user?name=cwolf&top=1")
# y<-as_list(x)
# i<-"cwolf"
# y<-paste("https://api.geekdo.com/xmlapi2/user?name=",i,"&top=1",sep="")
# read_xml(x=y)
# Sys.sleep(1)


#[[paste(i)]]

#174430   2017
#1137     2000
# id<-c(174430,1137)
# library(xml2)
# x<-read_xml(x="https://api.geekdo.com/xmlapi2/thing?id=10033&ratingcomments=1&page=1")
# y<-as_list(x)
# attr(y$items$item$comments[[1]],"username")
# attr(y$items$item$comments[[1]],"rating")
# x<-read_xml(x="https://api.geekdo.com/xmlapi2/thing?id=10033&ratingcomments=1&page=2")
# length(y$items$item$comments)
# for(i in 1:50){
#   cat(attr(y$items$item$comments[[i]],"username"),"\n")
# }
# attr(game_list$items$item$comments,"totalitems")



library(xml2)
id<-c(13,169786,822)
rated<-vector("list",length(id))
names(rated)<-id
for(i in id){
  url<-paste("https://api.geekdo.com/xmlapi2/thing?id=",i,"&ratingcomments=1",sep="")
  game_xml<-read_xml(x=url)
  game_list<-as_list(game_xml)
  usersrated<-as.integer(attr(game_list$items$item$comments,"totalitems"))
  rated[[as.character(i)]]<-data.frame(username=rep(NA,usersrated),yearregistered=rep(NA,usersrated),rating=rep(NA,usersrated))
  stop<-0
  j<-0
  k<-1
  Sys.sleep(2)
  while(stop==0){
    j<-j+1
    url<-paste("https://api.geekdo.com/xmlapi2/thing?id=",i,"&ratingcomments=1&page=",j,sep="")
    game_xml<-read_xml(x=url)
    game_list<-as_list(game_xml)
    if(j>ceiling(usersrated/100)){
      stop<-1
      break
    }
    for(l in 1:length(game_list$items$item$comments)){
      n_rep<-1
      if(attr(game_list$items$item$comments[[l]],"username") %in% rated[[as.character(i)]]$username){
        n_rep<-0
      }
      if(n_rep==1){
        rated[[as.character(i)]]$username[k]<-attr(game_list$items$item$comments[[l]],"username")
        rated[[as.character(i)]]$rating[k]<-attr(game_list$items$item$comments[[l]],"rating")
        k<-k+1
      }
    }
    Sys.sleep(2)
  }
  rm(k,l,j)
  #length(unique(rated[[as.character(i)]]$username)))
  for(j in 1:length(rated[[as.character(i)]]$username)){
    url<-paste("https://api.geekdo.com/xmlapi2/user?name=",rated[[as.character(i)]]$username[j],sep="")
    url<-gsub(" ","%20",url)
    user_xml<-read_xml(x=url)
    user_list<-as_list(user_xml)
    rated[[as.character(i)]]$yearregistered[j]<-attr(user_list$user$yearregistered,"value")
    Sys.sleep(2)
  }
  
}

