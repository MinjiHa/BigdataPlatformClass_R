print("hello world")
#R = Python + Numpy + Pandas + Matplotlib
#벡터

f = c('apple','grape','orange')
f
n = c(3,2,5)
n
s = c(TRUE,FALSE,FALSE)
f[0]
f[1]
f[1:3]
f[1:4]
f[-2] # R에서 -는 index번호가 아닌, 제외하는 method
f[-3]
f[-1]
f[c(TRUE,TRUE,FALSE)] #불린참조 c(일차원벡터)로 묶어야함
gender = factor(c("F","M","M"))
gender
blood<-factor(c("O","AB","A"),levels = c("O","AB","A","B"))
blood

hakjum<-factor(c("C","A","B","F"),levels = c("A","B","C","D","F"), ordered = TRUE)
#오름차순으로 나열 : ordered = TRUE

# 리스트 : 
# 파이썬 리스트와 공통점 : 순서를 갖는 데이터의 집합
# 데이터간에 타입이 달라도 저장됨.(벡터는 타입이 같아야함)

f[1]
n[1]
s[1]

mylist = list(myf=f[1],myn=n[1],mys=s[1])
mylist
typeof(mylist)
mylist$myn
mylist$mys
mylist[1]
mylist[[2]]
mylist[[1]]

data.frame() #{패키지명} 패키지:함수들의 묶음
df <- data.frame(f,n,s)
typeof(df)
str(df)
dd = data.frame(f,n,s)
dd
typeof(dd)

matrix() #행렬함수
matrix(c(1,2,3,4,5,6),nrow=2) #row의 갯수를 지정
m<-matrix(c(1,2,3,4,5,6),ncol=2) #col의 갯수를 지정
m[1,2]
m[1,]
m[2,]
m[,2]
m[,1]

#문자데이터가 Factor형태로 읽어짐
usedcars<-read.csv("usedcars.csv", stringsAsFactors = TRUE)
str(usedcars) #info와 유사한 함수
