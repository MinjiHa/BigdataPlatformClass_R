data(Epub)
help(Epub) #전자책 다운로드 데이터

summary(Epub) #이미 읽어져 있는 상태라 read.transaction 할필요없음

#연관규칙 연습

inspect(head(Epub))
itemFrequencyPlot(Epub,topN=20)
image(Epub[1:300])

Epub_rules <- apriori(Epub, parameter=list(
  support=0.002, confidence=0.002, minlen=2
))
summary(Epub_rules)
inspect(Epub_rules[1:3])
inspect(sort(Epub_rules, by='lift'))
write(Epub_rules, file="myEpub_Rules1.csv", sep=",", row.names=FALSE)
