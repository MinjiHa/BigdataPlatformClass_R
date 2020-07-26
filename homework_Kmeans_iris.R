##############################################################################
# 2. iris 또는 iris3 데이터에 대해 3개의 클러스터으로 나누세요. (시각화, ggplot)
#    + 3개 클러스터의 중심좌표


# 데이터 정규화
iris_z<-as.data.frame(scale(iris[-5]))

# K-means 군집화
iris_kkmeans <- kmeans(iris_z,3,nstart=2000)
iris_z$cluster <- as.factor(iris_kkmeans$cluster)

# ggplot 시각화
# install.packages("ggplot2")
# library(ggplot2)
ggplot() +
  geom_point(data = iris_z, 
             mapping = aes(x = Sepal.Length, 
                           y = Petal.Length, 
                           colour = cluster)) +
# add centroid (plot에 3개 클러스터의 중심좌표 표시)
  geom_point(mapping = aes_string(x = iris_kkmeans$centers[, "Sepal.Length"], 
                                  y = iris_kkmeans$centers[, "Petal.Length"]),
             color = "black", size = 2)

# centroids 수치확인
View(iris_kkmeans$centers)

# 분류가 얼마나 잘 되었는지 테이블로 확인
iris_z$Species <- iris$Species
table(iris_z$Species, iris_z$cluster)

##############################################################################
# 3.2번에서 만든 결과에 대해, 임의의 iris 데이터가 입력되었을 때,
# 어느 클러스터에 속하는지 출력하는 프로그램을 작성하세요.
# -[1.2, 2.1, 1.0, 1.5]-> 입력 -> 3번 클러스터

newRow <- data.frame('Sepal.Length'=1.2, 'Sepal.Width'=2.1, 'Petal.Length'=1.0, 'Petal.Width'=1.5)
iris_n <- rbind(iris[-5],newRow)
iris_nz <- as.data.frame(scale(iris_n))

c2 <- dist(rbind(iris_kkmeans$centers[1,],iris_nz[151,]))
c1 <- dist(rbind(iris_kkmeans$centers[2,],iris_nz[151,]))
c3 <- dist(rbind(iris_kkmeans$centers[3,],iris_nz[151,]))
newRow$cluster <- which.min(c(c1,c2,c3))