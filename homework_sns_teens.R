#데이터 불러오기
teens <- read.csv("snsdata.csv")
View(teens)
# 데이터 확인
summary(teens)

# 컬럼별로 결측값 갯수 확인
colSums(is.na(teens))

## age 이상치 해결
# 이상치 NA처리
teens$age <- ifelse( teens$age >= 13 & teens$age < 20 , teens$age, NA)

## age 결측값먼저 해결
# NA값 채워주기( 동일 졸업년도 졸업생의 나이평균 입력 )

avg_age<-ave(teens$age,teens$gradyear,
             FUN=function(x) mean(x,na.rm=TRUE))
View(avg_age)
teens$age <- ifelse(is.na(teens$age), avg_age, teens$age)

## gender 결측값 해결 - knn

gender_exist <- teens[!is.na(teens$gender),] # gender_exist <- na.omit(teens)
gender_na <- teens[is.na(teens$gender),]
View(gender_na$gender)

# 표준화
teens_train<-as.data.frame(scale(gender_exist[,4:40]))
teens_test<-as.data.frame(scale(gender_na[,4:40]))
View(teens_test)

library(class)
# library(caret)
# library(e1071)

genderknn <- knn(train = teens_train,
              test = teens_test,
              cl = gender_exist[,2],
              k = 11)

gender_na$gender <- genderknn
teens_fulldata <- rbind(gender_exist,gender_na)

summary(teens_fulldata)
