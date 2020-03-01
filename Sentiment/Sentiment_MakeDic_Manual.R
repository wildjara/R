# 1. 단어사전(dictionary) 직접 만들기 

# 자료 불러오기: 50,000개

food_score <- read.csv("hotplace.csv", stringsAsFactors = F)

score <- food_score$score
comment <- food_score$text

# 샘플링하기: 2,000개

sample_index<-sample(1:length(comment),2000)

sam_score<-score[sample_index]
sam_com<-comment[sample_index]

# table(sam_score)


library(KoNLP)
library(Stringr)

#1) 부정어 사전
nega_txt <- sam_com[sam_score <=1]

nega_list<-list() # 빈 리스트 

for(i in 1:length(nega_txt)){
  
  if(class(try(ss<-SimplePos09(nega_txt[i])))=="try-error"){
    nega_list[[i]]<-NA    
  }else{
    nega_list[[i]]<-ss
  }
  cat("\n",i)
}

divide<-function(x){
  
  sapply(str_split(x,"/"),function(x){x[1]})
}

nega_list<-sapply(nega_list,divide)

nega_word<- unlist(nega_list)
nega_words <- names(sort(table(nega_word),decreasing = T))

write.csv(nega_words, "manual_nega_dic.csv", row.names = F)

#2) 긍정어 사전
posi_txt <- sam_com[sam_score >=4]

posi_list<-list() # 빈 리스트 

for(i in 1:length(posi_txt)){
  
  if(class(try(ss<-SimplePos09(posi_txt[i])))=="try-error"){
    posi_list[[i]]<-NA    
  }else{
    posi_list[[i]]<-ss
  }
  cat("\n",i)
}

divide<-function(x){
  
  sapply(str_split(x,"/"),function(x){x[1]})
}

posi_list<-sapply(posi_list,divide)

posi_word<- unlist(posi_list)
posi_words <- names(sort(table(posi_word),decreasing = T))

write.csv(posi_words, "manual_posi_dic.csv", row.names = F)
