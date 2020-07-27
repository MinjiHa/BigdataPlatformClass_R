# 데이터 불러오기
smsraw <- read.csv("./sms_spam_ansi.txt", stringsAsFactors=FALSE)

# Corpus로 변환
library(tm)
smsCorpus <- VCorpus(VectorSource(smsraw$text))
smsCorpus

# corpus에 있는 단어를 소문자로 변환
corpusClean <- tm_map(smsCorpus, content_transformer(tolower))

# 모든 숫자를 제거
corpusClean1 <- tm_map(corpusClean, content_transformer(removeNumbers))

# 불용어 제거
stopwords()
corpusClean2 <- tm_map(corpusClean1, removeWords, stopwords())
corpusClean2[[1]]$content

# 특수문자 제거
corpusClean3 <- tm_map(corpusClean2, removePunctuation)

# 어근 형태로 변환
library(SnowballC)
corpusClean4 <- tm_map(corpusClean3, stemDocument)

# DTM(document...행,문서, term...열 matrix 생성)
smsDtm <- DocumentTermMatrix(corpusClean4)
smsDtm

# train test 분리
smsTrainLabels <- smsraw[1:4169,]$type
smsTestLabels <- smsraw[4170:5559,]$type

# 베이지안 필터기 (5번 미만 등장 단어 제외)
smsDtmTrain <- smsDtm[1:4169,]
smsDtmTest <- smsDtm[4170:5559,]
smsFreqWords <- findFreqTerms(smsDtmTrain, 5)
removeSparseTerms(smsDtmTrain, 0.9)

# 나이브베이즈 분류기
smsDtmFreqTrain <- smsDtmTrain[,smsFreqWords]
smsDtmFreqTest <- smsDtmTest[,smsFreqWords]
convertCounts <- function(x){
  x <- ifelse(x>0,"Yes","No")
}
smsTrain <- apply(smsDtmFreqTrain, MARGIN = 2, convertCounts)
smsTest <- apply(smsDtmFreqTest, MARGIN = 2, convertCounts)
smsTrain

# 나이브베이즈 분류기
library(e1071)
smsClassifier<-naiveBayes(smsTrain, smsTrainLabels)
smsTestPred<-predict(smsClassifier, smsTest)
smsTestPred

#############
## 연습문제 ##
#############

data <- c("You have 3 new messages. Call 119",
          "Sent me ur email id soon.",
          "We are trying to contack U.",
          "Sorry. I'll call later")

# corpus에 있는 단어를 소문자로 변환
dataClean <- tm_map(VCorpus(VectorSource(data)), content_transformer(tolower))
# 모든 숫자를 제거
dataClean <- tm_map(dataClean, content_transformer(removeNumbers))
# 불용어 제거
dataClean <- tm_map(dataClean, removeWords, stopwords())
# 특수문자 제거
dataClean <- tm_map(dataClean, removePunctuation)
# 어근 형태로 변환
dataClean <- tm_map(dataClean, stemDocument)

dataTest <- apply(DocumentTermMatrix(dataClean), MARGIN=2, convertCounts)
dataPred <- predict(smsClassifier, dataTest)
dataPred