wbcd<-read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1] #1번컬럼 제외
str(wbcd)
wbcd2<-wbcd[c(-1,-5,-7)] #여러개컬럼 제외
str(wbcd2)

table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"))
str(wbcd)
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"), lables = c("Benign","Malignant"))

typeof(table(wbcd$diagnosis)) #정수로 나온다 : 정수값들이 저장되어있는 벡터
table(wbcd$diagnosis)[1]
table(wbcd$diagnosis)[2]

round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c('radius_mean','area_mean','smoothness_mean')])
wbcd$radius_mean

#정규화작업을 하는 함수작성
normalize <- function(x){
  #print(min(x))
  #print(max(x))
  #print(x-min(x)) #벡터화연산 (벡터단위로 연산:for문없이도 연산된다.)
  #print(max(x)-min(x))
  return ( (x-min(x)) / (max(x)-min(x)) )
}

print(normalize(c(1,2,3,4,5))) #c함수로 묶으면 벡터다. x에 벡터가 전달되었다.

#lapply():리스트에 저장된 자료에 대해 특정 함수(nomalize)를 적용
#데이터프레임은 동일한 길이의 벡터들로 구성된 리스트

typeof(lapply(wbcd[2:31],normalize))
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))
wbcd_n # 정규화한 wbcd데이터

summary(wbcd_n$area_mean)
range(wbcd$area_mean)

#일반적으로 전체 데이터셋 => 트레이닝(70%),테스트(30%)
#트레이닝 데이터 => 알고리즘 => 모델링 => knn분류모델 => 테스트 => 결과(정확도) => 모델개선

#트레이닝 / 테스트데이터 분리

wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]
wbcd_train_lables<-wbcd[1:469,1]
wbcd_test_lables<-wbcd[470:569,1]#정답은 보통 레이블이라고 명명한다.

install.packages("class")
library(class)


#모델링
#knn함수
wbcd_test_pred <- knn(train = wbcd_train, 
                      test = wbcd_test, 
                      cl = wbcd_train_lables, 
                      k= 5)
wbcd_test_pred

#교차분석(CrossTable) : 변수간 관련성 여부를 분석
install.packages("gmodels")
library(gmodels)

CrossTable(x = wbcd_test_lables, y = wbcd_test_pred)


# 첫번째사람 악성분별
x = c(12.32,12.39,78.85,464.1,0.1028,0.06981,0.03987,0.037,0.1959,0.05955,0.236,0.6656,1.67,17.43,0.008045,0.0118,0.01683,0.01241,0.01924,0.002248,13.5,15.64,86.97,549.1,0.1385,0.1266,0.1242,0.09391,0.2827,0.06771)
wbcd_test_ex<-knn(train = wbcd_train, 
                  test = x, 
                  cl = wbcd_train_lables, 
                  k= 5)
wbcd_test_ex
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

#knn모델 ->교차분석(정확도)확인
#k=1,5,11,15,21,27 정확도 비교
wbcd_z

#트레이닝 / 테스트데이터 분리

wbcdz_train<-wbcd_z[1:469,]
wbcdz_test<-wbcd_z[470:569,]
wbcd_train_lables<-wbcd[1:469,1]
wbcd_test_lables<-wbcd[470:569,1]

k1train<- knn(train = wbcdz_train,
              test = wbcdz_test,
              cl = wbcd_train_lables,
              k=1)
CrossTable(x = wbcd_test_lables,
           y = k1train)

k5train<- knn(train = wbcdz_train,
              test = wbcdz_test,
              cl = wbcd_train_lables,
              k=5)
CrossTable(x = wbcd_test_lables,
           y = k5train)

k5train<- knn(train = wbcdz_train,
              test = wbcdz_test,
              cl = wbcd_train_lables,
              k=5)
CrossTable(x = wbcd_test_lables,
           y = k5train)

k7train<- knn(train = wbcdz_train,
              test = wbcdz_test,
              cl = wbcd_train_lables,
              k=7)
CrossTable(x = wbcd_test_lables,
           y = k7train)