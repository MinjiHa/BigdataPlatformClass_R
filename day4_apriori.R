install.packages("arules")
library(arules)

groceries<-read.transactions("groceries.csv",sep=",")
View(groceries)
summary(groceries)
inspect(groceries)
itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries,support=0.1) #지지도 0.1이상인 상품 시각화
itemFrequencyPlot(groceries,topN=20)
groceries[1:5]
image(groceries[1:100]) #이게 뭘의미한다고?

#169개 종류의 상품
#2의 169승 -1 => 아이템 항목 집합
#연관규칙:아이템 항목 집합(X) -> 아이템 항목집합(Y)
#연관규칙개수:아이템 항목 집합(X)*아이템 항목집합(Y)
#전체 연관규칙 중에서 향상도가 가장높은 연관규칙 10개를 뽑아보자
#Pruning! (Apriori Prune Algorithm)
#A(설탕) 구매횟수가 0이라면 가지치기(pruning)를 해주자
#ex) A->B, AB->C ...A가 들어가는 모든 규칙 제거
#Apriori 는 transaction type 이어야 한다.
#apriori(data, parameter = NULL, appearance = NULL, control = NULL)

grocery_rules<-apriori(groceries, parameter=list(
  support=0.005, confidence=0.2, minlen=2
)) 

#[872 rule(s)] 872개의 규칙 형성. minlen= 최소 길이가 2개 이상이어야한다.(조건)

summary(grocery_rules)
#X(lhs) ->Y(rhs)

inspect(grocery_rules[1:3])
inspect(sort(grocery_rules,by='lift')[1:15]) #1등-15등

subset()#부분집합을 뜻함
herb_rules <- subset(grocery_rules,items %in% "herbs") #허브와 관련된 rule들의 모임
inspect(herb_rules)

write(grocery_rules, file="myRules.csv", sep=",",row.names=FALSE)
df<- as(grocery_rules,"data.frame")
str(df)