# 감성분석

# 1. 사전불러오기 

posi<-c("정말강추","진짜맛있었어요","맛있","맛있습니","진짜맛","싸","최고","너무맛있어","강추입니다",
        "매콤한","쵝오","좋","친절","깔끔","달콤","고소","저렴","편하","훈남","훈녀","괜찮","맛나",
        "나쁘진","잘하는집","대박나","배터지게","최고","최고인거같아요","너무맛있네요","마시써영","마시써")
nega<-c("별로","맛없","쓰레기","맛은별로","망","접으세요","시켜먹","싫","질기","안받네요",
        "못먹어요","토할것만같아요","가지마","절대","비위생","느려터짐","더럽","미숙함","못하",
        "썰렁","부실","불친절","비추이","가관")

# 2. 분석문장 샘플링하기: 1,000개

food_score <- read.csv("hotplace.csv", stringsAsFactors = F)

score <- food_score$score
comment <- food_score$text

sample_index<-sample(1:length(comment),1000)

sam_score<-score[sample_index]
sam_com<-comment[sample_index]


# 3. 문장 형태소 분석하기

library(KoNLP)

data_list<-list()
for(i in 1:length(sam_com)){
  
  if(class(try(ss<-SimplePos09(sam_com[i])))=="try-error"){
    data_list[[i]]<-NA
    
  }else{
    
    data_list[[i]]<-ss
  }
  cat("\n",i)
}

divide<-function(x){
  
  sapply(str_split(x,"/"),function(x){x[1]})

}

data_list<-sapply(data_list,divide)


# 4. 점수 계산하기

posi_list<-list()
nega_list<-list()
score_list<-c()

for(j in 1:length(data_list)){
  
  posi_list[[j]]<- posi[(posi %in% data_list[[j]] )]
  nega_list[[j]]<- nega[(nega %in% data_list[[j]] )]  
  
  posi_score <- sum(posi %in% data_list[[j]] )
  nega_score <- sum(nega %in% data_list[[j]] )
  
  text_score<- posi_score + nega_score
  score_list[j]<-text_score
  cat("\n",j)
  
}

hist(score_list)
final_score<-ifelse(score_list > 0,"긍정","부정")
table(final_score)

# 5. 오차 계산하기
true_score<-ifelse(sam_score>3,"긍정","부정")
sum(final_score == true_score)/1000
# which(final_score != true_score)

