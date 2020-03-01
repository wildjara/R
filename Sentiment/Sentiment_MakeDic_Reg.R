# 단어사전(dictionary): Ordinay Regression 이용 만들기 

# 1. 자료 불러오기: 50,000개

food_score <- read.csv("hotplace.csv", stringsAsFactors = F)

score <- food_score$score
comment <- food_score$text

# 2. 샘플링하기: 2,000개

sample_index<-sample(1:length(comment),2000)

sam_score<-score[sample_index]
sam_com<-comment[sample_index]
# table(sam_score)

# 3. 변수 만들기
install.packages("tm")
library(tm)

## 불용어 stopwords
stopwds<-c("으로","하는","에서","입니다","합니다","있습니다","하고","에게","있는","라고","에도","이고","하게","또한","하기",
       "되는","되지","이라고","이고","라는","됩니다","않습니다","그리고","해야","때문","까지","하지","하여","해서","했습니다","부터"
       ,"있게","다시","하면","이다","있다","또는","했으며","게시","해주십시오","질문","것임","되었습니다","이글","있었다","로는","하는것",
       "않았습니다","되는데","하는","입니다","이라도","니요","에는","한다","한다고",
       "했다","없는","니까","되어","께서","인가요","아니면","이번","되고","아래","대한","있기","두고","지는","이로","으로만",
       "와중","봐도","있음에도","있을까","됐다","되고있는","하던","에선","된다면","했으면","들입니다","하면서","했지만",
       "어느","드립니다","겁니다","아닙니다","싶습니다","한다면","해주세요","인데","에서","바랍니다","이는","이라는","인데","하며","정도",
       "있도록","이나","않는","하였습니다","밖에","처럼","것으로","것은","그래서","그러니까","것이라고","말했습니다","말했다","그러면","있다고",
       "지금","그렇게","것이","떄문에","재배포","이어","보면","같이")

# 말뭉치 corpus: UTF-8->VCorpus!!
corpus <- VCorpus(VectorSource(sam_com))

# DTM 
dtm <- DocumentTermMatrix(corpus, control=list(removePunctuation=TRUE, 
                                               removeNumbers=TRUE,
                                               stopwords = stopwds,
                                               weighting=weightTf))

# X 독립변수
X <- as.matrix(dtm)
# X[1:5, 1:5]
# dim(X)


# Y 종속변수
sentiment<-ifelse(sam_score>3,1,0)
Y <- sentiment

# 4. Regression 분석하기
# 1) 일반 회귀
install.packages("glmnet")
library(glmnet)

options(scipen = 100000)

lm_results <- glmnet(X, Y, family = "binomial", lambda = 0)
# lm_results -> 개수

coef_lm <- coef(lm_results)[,1]
#coef_lm[1:5]


pos_lm <- coef_lm[coef_lm > 0]
neg_lm <- coef_lm[coef_lm < 0]

pos_lm <- sort(pos_lm, decreasing = T)
neg_lm <- sort(neg_lm, decreasing = F)
# pos_lm[1:5]
# neg_lm[1:5]

length(pos_lm)
length(neg_lm)
dim(X)


write.csv(pos_lm, "reg_posi_dic.csv")
write.csv(neg_lm, "reg_nega_dic.csv")




