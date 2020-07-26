#문자데이터가 Factor형태로 읽어짐
usedcars<-read.csv("usedcars.csv", stringsAsFactors = TRUE)
usedcars
str(usedcars) #info와 유사한 함수

summary(usedcars) #컬럼단위로 기술통계치 확인
summary(usedcars$price) #한개컬럼 기술통계치 확인
summary(usedcars[c('price','mileage')]) #여러컬럼 기술통계치 확인

(36+44+56)/3
mean(c(36,44,56)) #평균
median(c(35,44,56)) #중위수(오름차순정렬시 중앙값)
#평균이 어느방향으로 치우쳐있는지 확인가능하다.

range(usedcars$price) #최소/최대값확인
diff(range(usedcars$price)) #최대-최소 차이확인
summary(usedcars$price)
IQR(usedcars$price) #3사분위수 - 1사분위수

quantile(usedcars$price) #five-number
boxplot(usedcars$price)
boxplot(usedcars$price, ylab='Price', main="Boxplot")
hist(usedcars$price, ylab='Price', main="histogram")
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)
plot(x = usedcars$mileage,
     y = usedcars$price)

#수치변수 => summary
#범주변수 => table
table(usedcars$color)
round(prop.table(table(usedcars$model))*100, digits = 1)


install.packages("gmodels")
library(gmodels)

usedcars$conservative<-usedcars$color %in% c("Black","Gray","Silver","White")
table(usedcars$conservative)
CrossTable(x=usedcars$model,y=usedcars$conservative)
#모델(독립)이 색(종속)선택에 영향을 줄것인가?

cars
str(cars)
head(cars)
summary(cars)
plot(cars$speed, cars$dist,
     xlab='speed(MPH)',
     ylab = 'distance(feet)',
     main = 'stopping distance of cars based on speed')
cor(cars$speed,cars$dist) #상관계수

#lm(종속변수~독립변수들,data) #선형회귀모델이 만들어진다.
myModel <- lm( dist~speed ,cars) # 선형회귀모델을 만듬
myModel
# myModel2 <- lm( dist~speed+wind ,cars) # 독립변수가 여러개인 선형회귀모델을 만듬(예시)

#h(x) = 3.932*x - 17.579

#predict

newd<-data.frame(speed=c(10,30,50,100))
newd

predict(myModel,newd)
predict(myModel,newd, interval = 'confidence') #신뢰구간과 함께 출력


iris
wbcd<-read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1] #1번컬럼 제외
str(wbcd)
wbcd2<-wbcd[c(-1,-5,-7)]
str(wbcd2)

table(wbcd$diagnosis)



wbcd[1:2,1:10]

df = data.frame(Hours=c(3,7,6,9,4,2),Score=c(50,80,60,75,70,55))

Mmodel = lm(Score~Hours,df)

x = data.frame(Hours=c(2,5,10,15))
predict(Mmodel,x,interval = 'confidence')
