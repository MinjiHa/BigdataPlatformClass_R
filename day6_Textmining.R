#텍스트 분석

c(1:6,'a') #벡터
list(1:6,'a') #리스트

o1<-1:4
o2<-6:10
o3<-list(o1,o2)
myo <- list(o1,o2,o3)
myo

# 리스트 : [[]], 벡터:[]
myo[[3]][1] # []: 리스트내의 리스트를 추출
myo[[3]][[1]] #[[]]:리스트 내의 리스트에 대한 벡터를 추출
myo[3]
myo[[3]]
myo[[3]][[2]][4]

mylist<-list(1:6,'a')
mylist
myvector<-c(1:6,'a')
myvector
unlist(mylist)
unlist(mylist) == myvector
mean(unlist(mylist)[1:6])

mylist[[1]]
mean(mylist[[1]][1:6])
n1<-"Donald"
ms<-" "
n2<-"Trump"
list(n1,ms,n2)
unlist(list(n1,ms,n2)) # unlist 함수를 쓰면 리스트가 해제되면서 벡터로 바뀐다.

n<-c("갑","을","병","정")
gen<-c(2,1,1,2)
df<-data.frame(n,gen)
df

# attribute : 속성에 대한 이름을 주겠다.
attr(df$n, "means") <- "이름"
attr(df$gen, "means") <- "성별"
df$n

myvalues <- gen
length(gen)
for (i in 1:length(gen)){
  myvalues[i]<-ifelse(gen[i]==1,"남성","여성")
}
myvalues
attr(df$gen,"means") #means : 속성명, "성별":속성값

mylist <- list(1:4,6:10,list(1:4,6:10))
mean(mylist[[1]]) # 벡터에 대한 평균
lapply(mylist[1], mean) #list에 대한 평균

rep(1,4) #1이 4번 반복됨

s1 <- c("earth","to","earth")
rep(1,length(s1))#1이 3번 반복됨
rep(s1,2)

letters[1:26] #알파벳 1번부터 26번까지 출력
letters #알파벳 소문자
LETTERS #알파벳 대문자
tolower("Eye for eye")
toupper("Eye for eye")
nchar("Korea") #몇글자인지
nchar("Korea", type = 'bytes')
nchar("한국")
nchar("한국", type = 'bytes')
sent <- "Learning R is so interesting"
mywords <- strsplit(sent,split = " ")

mywords[[1]][5]
strsplit(mywords[[1]][5], split = "")

sent2<-"지지자불여호지자 호지자불여락지자"
strsplit(sent2, split=" ")
strsplit(strsplit(sent2, split=" ")[[1]][2], split="")

rep(NA,5) #na가 5개 들어있는 벡터 생성
myletters<-list(rep(NA,5)) #벡터를 리스트로


myletters[1]<-strsplit(mywords[[1]][1],split = "")
myletters

for (i in 1:5){
  myletters[i]<-strsplit(mywords[[1]][i],split = "")
}

#문자를 합쳐 단어로 구성 : paste
paste(myletters[[1]],collapse = "")
paste(myletters[[1]],collapse = "&")

for (i in 1:5){
  print(myletters[[i]])
}

for (i in 1:5){
  print(paste(myletters[[i]],collapse = ""))
}

mywords2<-rep(NA,5)
for (i in 1:5){
  mywords2[i] <- (paste(myletters[[i]],collapse = ""))
}
mywords2

mywords3<-list(rep(NA,5))
for (i in 1:5){
  mywords3[i] <- (paste(myletters[[i]],collapse = ""))
}
mywords3

paste(mywords2,collapse=" ")
paste(mywords3,collapse=" ")

rwiki <- "R is a programming language and free software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing.
The R language is widely used among statisticians and data miners for developing statistical software and data analysis.
Polls, data mining surveys, and studies of scholarly literature databases show substantial increases in popularity; as of June 2020, R ranks 9th in the TIOBE index, a measure of popularity of programming languages."
rwikiPara = strsplit(rwiki,split = "\n") #\n : 문단단위로 나눈다.

for (i in 1:3){
  print(strsplit(rwikiPara[[1]][i],split = " "))
}

#정규표현식 : regexpr() 1개만 검색 / gregexpr() 전부검색
sent
regexpr("ing",sent) #정규표현식 : ing 패턴을 찾는다.
loc.begin <- as.vector(regexpr("ing",sent))
loc.length<-attr(regexpr("ing",sent),'match.length') #이 패턴을 만족하는 문자열의 길이가 얼마냐
loc.end <- loc.begin + loc.length -1
loc.end

gregexpr("ing",sent)

length(gregexpr("ing",sent)[[1]]) #ing 패턴이 몇번 매칭되었는지
regexpr("interestin(g)",sent)
regexpr("so (interestin(g))",sent)
regexec("interestin(g)",sent)
regexec("so (interestin(g))",sent)

#소문자 ing를 ING로 바꾸기
sub("ing",'ING',sent) #하나만 바꿈
gsub("ing",'ING',sent) #전부 바꿈

# 고유명사 처리 : 여러 단어로 기관명 구성 -> _와 같은 기호로 연결 : 자연어 전처리 필수작업
# singer = "가수들 이름" 이라고 했을 때
gsub("Micheal Jackon", "Micheal_Jackon", singer) #고유명사 처리 예시

rwikiPsplit = list(rep(NA,3))
for (i in 1:3){
  rwikiPsplit <- strsplit(rwikiPara[[1]][i],split = " ")
}

sent1<-rwikiPsplit[[1]]

sum(table(sent1))
gsub("of","",sent1)
dropSent<-gsub("as|in|the","",sent1)
strsplit(dropSent,split="")

sent <- c("Learning R is so interesting",
          "He is a fascinating singer")

#ing로 끝나는 모든 단어
mypat0 <- gregexpr("ing",sent)
regmatches(sent,mypat0)

mypat1 <- gregexpr("[[:alpha:]]ing",sent)
regmatches(sent,mypat1)

mypat1 <- gregexpr("[[:alpha:]](ing)",sent)
regmatches(sent,mypat1)

mypat1 <- gregexpr("[[:upper:]]",sent)
regmatches(sent,mypat1) #대문자인것들만 나온다

mypat1 <- gregexpr("[[:lower:]]",sent)
regmatches(sent,mypat1) #소문자인것들만 나온다

unlist(regmatches(sent,mypat1)) #벡터화
mytable <- table(unlist(regmatches(sent,mypat1))) #각각 몇번나왔는가

max(mytable)
length(mytable)
sum(mytable)

#코퍼스(corpus,말뭉치)
# 분석 대상 분야에서 사용되는 용어 집합
# ex) 인공지능분야 코퍼스 : 머신러닝, 파이썬, 알...

# 자연어처리 : 코퍼스구성
# 법룰분야 코퍼스 : 범죄, 법, 헌법, ... , 날씨(x)

mytext <- c("software environment",
            "software  environment",
            "software\tenvironment")
# install.packages("stringr")
# library(stringr)
# strsplit {base}
# str_split {stringr}

str_split(mytext," ")

#어떤 문자열을 다른 문자열로 치환
str_replace_all(mytext, "[[:space:]]{1,}"," ")#앞에 있는 문자가 한개이상 존재한다면 치환해라.
mytext.nowhitespace <- str_replace_all(mytext, "[[:space:]]{1,}"," ")
mytext2<-"The 45th President of the United States, 
Donald Trump, states that he knows how to play trump with the former president"

str_extract_all(mytext2, boundary("word")) # boundary("word") : 단어단위로 추출하게 해줌
myword <- unlist(str_extract_all(mytext2, boundary("word")))
table(myword)

#고유명사처리 (철자만 같은 단어와 합쳐지지않게)
myword <- str_replace(myword, "Trump","Trump_unique_") 
myword <- str_replace(myword, "States","States_unique_")
table(tolower(myword))

#숫자가 한자리 이상+공백이 한자리이상일때 "_number_" 로 치환
mytext <- c("R is the No. 1 stat sw")
str_replace_all(mytext,"[[:digit:]]{1,}[[:space:]]{1,}","_number_")

####################################################################

# 텍스트마이닝 패키지
# install.packages("tm")
# library(tm)

stopwords("en")
stopwords("SMART")

#am, are, is, was were => be로 치환 : 숙제
mytext <- c("I am a boy. You are a boy. The person might be a boy. Is Jane a boy?")
str_replace_all(mytext, "am|are|is|was|were" ,replacement = "be")


# VCorpus : 폴더에 있는 모든 텍스트 데이터들을 말뭉치로 구성
my.text.location <- "d:/Rwork/dataR/papers"
mypaper <- VCorpus(DirSource(my.text.location))
summary(mypaper)

mypaper[[2]] # 2번째 문서
mypaper[[2]]$content # 문서내의 내용 추출
mypaper[[2]]$meta # 메타데이터 출력

# 전화번호 : 데이터, 색인('김'입력):메타데이터
# [[:alnum:]] : 영문+숫자
# [[:punct:]] : 특수기호
# [[:space:]] : 공백
meta(mypaper[[2]],tag="author") <- "G. D. Hong"
myfunc <- function(x){
  str_extract_all(x$content,"[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
}
lapply(mypaper, myfunc)

# 대문자 [[:upper:]]로 시작하는 단어(영문+숫자)를 추출
myfunc <- function(x){
  str_extract_all(x$content,"[[:upper:]]{1,}[[:alnum:]]{1,}")
}
lapply(mypaper, myfunc)

# removeNumbers : 숫자를 모두 제거
mycorpus <- tm_map(mypaper,removeNumbers)
mypaper[[1]]$content #제거전
mycorpus[[1]]$content #제거후

# removePunctuation : 특수문자를 모두 제거
mycorpus <- tm_map(mypaper,removePunctuation)
mypaper[[1]]$content #제거전
mycorpus[[1]]$content #제거후

# stripWhitespace : 불필요한 공란을 모두 제거
mycorpus <- tm_map(mycorpus,stripWhitespace)
mypaper[[1]]$content #제거전
mycorpus[[1]]$content #제거후

# 토큰화 : 코퍼스를 토큰 단위로 나누는 작업
# 정제와 정규화란?
#   정제 : 불필요한 부분 제거
#   정규화 :  의미가 같은 다양한 단어들을 하나의 단어로 통일


