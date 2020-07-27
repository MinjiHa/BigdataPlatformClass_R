smsraw<-read.csv("sms_spam_ansi.txt", stringsAsFactors = FALSE)
str(smsraw)
smsraw$type<-factor(smsraw$type) #팩터 타입으로 바꾼다 
str(smsraw)
table(smsraw$type) # ham과 spam의 개수를 세어보자 
library(tm)

smsCorpus <- VCorpus(VectorSource(smsraw$text))
smsCorpus
inspect(smsCorpus[1])
smsCorpus[[1]]$Content
as.character(smsCorpus[[1]])
lapply(smsCorpus[1], as.character) #코퍼스 안에 있는 내용을 들여다보고싶다

# 텍스트 전처리 패키지 : tm, 함수 : tm_map(코퍼스 전체에 대한 변환)
# tm_map(코퍼스변수, content_transformer(변환함수))

# 코퍼스에 있는 모든 단어를 소문자로 변환
corpusClean <- tm_map(smsCorpus, content_transformer(tolower)) # 모든 코퍼스에 적용. 이 형식 꼭 기억하기
corpusClean[[4]]$content

# 코퍼스에 있는 모든 숫자를 제거
corpusClean <- tm_map(corpusClean, removeNumbers)
corpusClean[[4]]$content

#stopwords(불용어) 제거
stopwords()

corpusClean <- tm_map(corpusClean, removeWords, stopwords()) #3번째 인수로 제거할 단어 벡터를 줘라.
# c("they","i","you") # 사용자정의 불용어사전
corpusClean[[4]]$content

#구두점 제거
corpusClean <- tm_map(corpusClean, removePunctuation)
corpusClean[[4]]$content

corpusClean[[2]]$content
removePunctuation("hello.............world") # 점이 모두 제거됨

# 함수이용방법 (구두점을 다른걸로 바꾸기)
myRemPunc <- function(X){
  #gsub(패턴식,치환,x)
  gsub("[[:punct:]]+"," ", x) #점 7개를 공백문자 1개로 치환해라.
}

myRemPunc("hello.............world")

# 어근 형태로 변환
library(SnowballC)
wordStem(c("learn","learned","learning","learns")) # 벡터안에 있는 문자들을 어근으로 변환

corpusClean2 <-tm_map(corpusClean, stemDocument)
corpusClean[[4]]$content
corpusClean2[[4]]$content
corpusClean[[2]]$content
corpusClean2[[2]]$content

corpusClean2 <- tm_map(corpusClean, stripWhitespace)
lapply(corpusClean2[1:5], as.character)
lapply(smsCorpus[1:5], as.character)

# 텍스트 전처리 -> DTM(Document(행,문서) Term(열) Matrix) 생성
smsDtm <- DocumentTermMatrix((corpusClean2))
smsDtm

#전처리 한꺼번에 하기 1
smsDtm2 <- DocumentTermMatrix(smsCorpus,control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))
smsDtm2

#전처리 한꺼번에 하기 2
smsDtm3 <- DocumentTermMatrix(smsCorpus,control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=function(x){removeWords(x,stopwords())},
  removePunctuation=TRUE,
  stemming=TRUE
))
smsDtm3

#트레이닝/테스트데이터 나누기
smsDtmTrain <- smsDtm[1:4169,] #문제만 있고 갑은 없다.
smsDtmTest <- smsDtm[4170:5559,]

smsDtmTrainLables <- smsraw[1:4169,]$type #트레이닝 답 저장
smsDtmTestLables <- smsraw[4170:5559,]$type #테스트 답 저장

# spam/ham 비율확인
prop.table(table(smsDtmTrainLables))
prop.table(table(smsDtmTestLables))


#워드클라우드 설치
#install.packages("wordcloud")
library(wordcloud)

#워드클라우드 - 50회이상 단어만 출력
wordcloud(corpusClean2, min.freq = 50)
#워드클라우드 - 빈도 높은 단어 가운데로
wordcloud(corpusClean2, min.freq = 50, random.order = FALSE)

#스팸 / 햄 구분
spam <- subset(smsraw, type=='spam')
ham <- subset(smsraw, type=='ham')


blue <- brewer.pal(9,"Paired") #글자색상지정
wordcloud(spam$text, max.words = 40, 
          random.order = FALSE,
          colors = blue)

#베이지안 필터기(5번 미만 등장단어는 제외)
#5500개 문서에서 5번 미만 (0.5%)
smsFreqWords <- findFreqTerms(smsDtmTrain,5) #1137개단어
removeSparseTerms(smsDtmTrain,0.999) #희소도가 인자2보다 작아야 쓰겠다

# 희소도 : 0,999 => 1000칸 중에서 1개 칸이 0이 아니고 나머지는 0

str(smsFreqWords)
smsDtmFreqTrain <- smsDtmTrain[,smsFreqWords] # smsFreqWords와 일치하는 것만 추출
smsDtmFreqTest <-smsDtmTest[,smsFreqWords]

# 나이브베이즈 분류기는 범주형 데이터에 대해 훈련
convertCounts <- function(x){
  x <- ifelse(x>0,"Yes","No")
}

smsTrain <- apply(smsDtmFreqTrain, MARGIN = 2, convertCounts)
# MARGIN : 행=1,열=2
smsTrain[4000,] # 첫번째 이메일에 있는 컬럼
smsTest <- apply(smsDtmFreqTest, MARGIN = 2, convertCounts)



#모델 만들기
install.packages("e1071")
library(e1071) #나이브 베이즈 분류기 패키지
#우도표 만들기
smsClassifier<- naiveBayes(smsTrain,smsDtmTrainLables) 
#우도표에 test데이터 넘겨서 예측
smsTestPred<- predict(smsClassifier,smsTest) 
smsTestPred
#예측결과 정확도 확인
library(gmodels)
CrossTable(smsTestPred, smsDtmTestLables)


########## 테스트데이터 불러오기 ##########
smsraw<-read.csv("testdata.txt", stringsAsFactors = FALSE)
str(smsraw)

# 코퍼스 만들기
library(tm)
testCorpus <- VCorpus(VectorSource(smsraw))
inspect(testCorpus[1])
as.character(testCorpus[1])
lapply(testCorpus,as.character)

# 코퍼스 전처리
testDtm<-DocumentTermMatrix(testCorpus, control=list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=function(x){removeWords(x,stopwords())},
  removePunctuation=TRUE,
  stemming=TRUE
))
inspect(testDtm)
lapply(testDtm,as.character)

#x>0 smsDtmFreqTrain이 0보다 크면= 문서에서 단어가 들어가 있으면 yes, 아니면 no다.
convertCounts<-function(x){
  x<-ifelse(x>0, "Yes", "No")  
} # x를 전달 받는다.

test<-apply(testDtm, MARGIN=2,convertCounts) # MARGIN: 행(1), 열(2)
test # smsDtmFreqTrain을 convertcounts-function(x)로 전달 받는다. 

#우도표에 test데이터 넘겨서 예측
smsTestPred2<- predict(smsClassifier,test) 
smsTestPred2
#예측결과 정확도 확인
library(gmodels)
CrossTable(smsTestPred2, smsDtmTestLables)