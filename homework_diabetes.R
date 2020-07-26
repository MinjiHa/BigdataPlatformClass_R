dia<-read.csv("diabetes.csv", stringsAsFactors = FALSE)
str(dia)
# wbcd<-wbcd[-1] #1번컬럼 제외
# str(wbcd)
# wbcd2<-wbcd[c(-1,-5,-7)] #여러개컬럼 제외
# str(wbcd2)

dia_train_lables <- dia[1:532,9]
dia_train_lables
dia_test_lables <- dia[533:758,9]
dia_test_lables

diax<-dia[-9]

dia_train <- diax[1:532,]
dia_test <- diax[533:758,]


diaknn1 <- knn(train = dia_train,
              test = dia_test,
              cl = dia_train_lables,
              k = 1)
ct<-CrossTable(x=dia_test_lables,
           y=diaknn1)
ct$t[1,2]

for (i in 1:50){
  if(i%%2 != 0){

    diaknn <- knn(train = dia_train,
                   test = dia_test,
                   cl = dia_train_lables,
                   k = i)
    ct <- CrossTable(x=dia_test_lables,
               y=diaknn)
    tp<-ct$t[1,1]
    fp<-ct$t[1,2]
    fn<-ct$t[2,1]
    Precision<-tp/(tp+fp)
    Recall<-tp/(tp+fn)
    f<-2*Precision*Recall/(Precision+Recall)
    print("K값")
    print(i)
    print("정확도:")
    print(f)
  }
}
