
############################## 미니프로젝트 ####################################



#감성어휘사전 기반, 감성분석 프로젝트
#스크래핑(파이썬, 해외언론사(통신사), 넷플릭스...영화줄거리) -> txt형식으로 저장
#데이터전처리 -> 감성분석 -> 결과 출력 



############# 미니프로젝트 : 구글영문news Top stories 감정분석 #################


install.packages("rvest")
install.packages("dplyr")
library(rvest)
library(dplyr)
library(tidytext)
library(textdata)
library(tidyverse)
library(tm)
library(stringr)
library(dplyr)
library(tidyr)
library(textstem)
url<-"https://news.google.com/topstories?hl=en-US&gl=US&ceid=US:en"
html<-read_html(url)
html2<-html_nodes(html, '.DY5T1d') %>% html_text()
html2
typeof(html2)
text_news<-data_frame(id=1:55, text=html2)
text_news.df<- text_news %>% unnest_tokens(word, text)
text_news.df
result_news<-text_news.df %>% inner_join((get_sentiments("bing"))) %>% count(word, id, sentiment) %>% spread(sentiment, n, fill=0)
result_news
myagg_news<-summarise(group_by(result_news,id),pos.sum=sum(positive),neg.sum=sum(negative),pos.sent=pos.sum-neg.sum)
myagg_news
plot(myagg_news$pos.sum, myagg_news$neg.sum)
myagg_news$color[myagg_news$pos.sent>=1]='blue'
myagg_news$color[myagg_news$pos.sent<=-1]='red'
myagg_news$color[myagg_news$pos.sent==0]='yellow'
plot(myagg_news$pos.sent, col=myagg_news$color)
pie(table(myagg_news$color))


