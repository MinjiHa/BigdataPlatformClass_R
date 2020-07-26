teens<-read.csv("snsdata.csv")
str(teens)

table(teens$gender) #table함수는 종류별로 몇개씩 있는지 요약해주는 함수이다.
table(teens$gender, useNA = "ifany") #NA값의 갯수도 함께 count한다.

table(teens$age)
#범주형데이터에 각각 몇개씩 데이터가 있는지 갯수를 센다.
#나이는 float으로 되어있어서 범주가 너무 많음.
summary(teens$age)

##이상치 처리
teens$age <- ifelse( teens$age >= 13 & teens$age < 20, teens$age , NA)
summary(teens$age)

teens$female <- ifelse(teens$gender=="F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)

table(ifelse(teens$gender=="F",1,0))# 이렇게 하면 na가 무시되어버림.

#여성 22000명, 남성&NA 8000명

summary(teens$age) #5523개 na
mean(teens$age) #결과가 na가 나와버림
mean(teens$age, na.rm=TRUE) #na가 제외된 평균값



#평균 연령을 출력하자
#집계 함수: sum, count... / na.rm=TRUE(default로 들어가 있음)
#데이터를 그룹화한 다음 그룹단위로 특정 통계함수 적용 : #aggregate
# aggregate(data=데이터,함수적용원하는컬럼 ~ 그룹화기준컬럼, 사용할 함수)
aggregate(data=teens,age ~ gradyear, mean)
agg<-aggregate(data=teens, age ~ gradyear, mean, na.rm=TRUE)

#teens데이터에서 gradyear를 기준으로 그룹화한다음에, 그룹별로 age의 평균을 구한다.
typeof(agg)

#teens 데이터에서 gradyear열을 기준으로 그룹화한 다음
#그룹별로 age 컬럼값의 평균을 구해라
aggregate(data=teens, age~gradyear,mean,na.rm=TRUE)

#avg_age함수 정의
#(teens$age = x ,teens$gradyear = 그룹바이기준, FUN은 람다와 같은 개념)
avg_age<-ave(teens$age,teens$gradyear,
             FUN=function(x) mean(x,na.rm=TRUE))

#ifelse(조건, 참이면 실행할 함수명, 거짓이면 넣을 값)
teens$age<-ifelse(is.na(teens$age),avg_age,teens$age)

summary(teens$age)
interests<-teens[5:40]

#표준화작업
#lapply(함수 적용할데이터,적용하고자하는 함수명)
lapply(interests,scale) #표준화
lapply(interests,max) #최댓값
lapply(interests,var) #분산

interests_z<-as.data.frame(lapply(interests,scale))
set.seed(2345) #랜덤하게 centoid point 생성하기 위함

#kmeans
#kmeans(numeric vector or a data frame with all numeric columns,k값)
teen_clusters<-kmeans(interests_z,5)
teen_clusters

teen_clusters$size
teen_clusters$centers
teens$cluster<-teen_clusters$cluster

teens[1:10,c('cluster','gender','age','friends')]

#클러스터별로 나이의 평균
aggregate(data=teens,age~cluster,mean)
#클러스터별로 여학생수의 평균
aggregate(data=teens,female~cluster,mean)
#각 클러스터별로 친구수의 평균
aggregate(data=teens,friends~cluster,mean)
