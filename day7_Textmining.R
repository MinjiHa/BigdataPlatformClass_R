########################### KoNLP 설치문제 해결 ###########################

install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241')  # 설치한 JAVA version에 따라 달라집니다
buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
useNIADic()  # "NIADic" dic을 불러옵니다

library(KoNLP)

###########################################################################

install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library("textdata")
install.packages("tidyverse")
library("tidyverse")

AFINN <- data.frame(get_sentiments("afinn"))
summary(AFINN)
par("mar")
par(mar=c(1,1,1,1))
hist(AFINN$value, breaks = 20, xlim = c(-6,6), col = 'blue' , xlab='senti score')

get_sentiments("bing")
oplex <- data.frame(get_sentiments("bing"))
table(oplex$sentiment)

emolex<-data.frame(get_sentiments("nrc"))
table(emolex$sentiment)

library(tm)
library(stringr)
library(dplyr)

# 코퍼스 만들고자 하는 파일들이 들어있는 폴더
my.text.location <-"papers"
# 코퍼스만들 때 쓰는 함수
mypaper<-VCorpus(DirSource(my.text.location))
mypaper
mypaper[[1]][1] #코퍼스 내용을 확인 (첫번째 텍스트파일)
mypaper[[33]][1] #코퍼스 내용을 확인 (마지막 텍스트파일)

mytxt <- c(rep(NA,33))

for(i in 1:33){
  #mytxt[i] <- mypaper[[i]][1] #코퍼스내용을 리스트로저장
  mytxt[i] <- as.character(mypaper[[i]][1]) #문자형태의 벡터로 저장됨
}

mytxt
typeof(mytxt)

my.df.txt <- tibble(paper.id=1:33, doc=mytxt) #데이터프레임화시키는 함수?
my.df.txt

my.df.text.word <- my.df.txt %>% unnest_tokens(word,doc)

# %>% 파이프오퍼레이터 : 오브젝트%>%함수
# 함수를 오브젝트에 적용하라는 의미

text <- c("How are you",
          "He is a boy",
          "She is a girl")
textDf<-data_frame(line=1:3,text=text)


#tidy패키지 안에 있는 함수
#토큰단위로 분해해라
textDf %>% unnest_tokens(word,text) #반드시 적용될객체가 있어야 한다.
library(tidyr)
myresult.sa <-  my.df.text.word %>% 
                  inner_join(get_sentiments("bing")) %>% 
                  count(word,paper.id,sentiment) %>%  # 문서별(paper.id)로 해당 단어가 몇개씩 있는지 세는 함수
                  spread(sentiment, n, fill=0)

myresult.sa

#긍정적 감정단어와 부정적 감정단어의 합을 구해보자
myagg <- summarise(group_by(myresult.sa,paper.id), #(paper.id) 기준으로 그룹화해서 요약해보자.
                    pos.sum = sum(positive),
                    neg.sum = sum(negative),
                    pos.sent = pos.sum - neg.sum)

myagg

install.packages("textstem")
library(textstem)
x<-c("well","doggies","running")
lemmatize_words(x)


#감성어휘사전 기반, 감성분석 프로젝트
#스크래핑(파이썬, 해외언론사(통신사), 넷플릭스...영화줄거리) -> txt형식으로 저장
#데이터전처리 -> 감성분석 -> 결과 출력 
#스크래핑 어려운 사람은 아래코드로 실습 
#data(acq)
#acq[[50]][1]

library(SnowballC)
install.packages("analogue")
library(analogue)

data(acq)
acq[[1]][1]
typeof(acq)
summary(acq)

myacq <- c(rep(NA,50))

for (i in 1:50){
  myacq[i] <- as.character(acq[[i]][1])
  as.character(acq[[i]][2])
}


