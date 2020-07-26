##############################################################################
# 2. iris �Ǵ� iris3 �����Ϳ� ���� 3���� Ŭ���������� ��������. (�ð�ȭ, ggplot)
#    + 3�� Ŭ�������� �߽���ǥ


# ������ ����ȭ
iris_z<-as.data.frame(scale(iris[-5]))

# K-means ����ȭ
iris_kkmeans <- kmeans(iris_z,3,nstart=2000)
iris_z$cluster <- as.factor(iris_kkmeans$cluster)

# ggplot �ð�ȭ
# install.packages("ggplot2")
# library(ggplot2)
ggplot() +
  geom_point(data = iris_z, 
             mapping = aes(x = Sepal.Length, 
                           y = Petal.Length, 
                           colour = cluster)) +
# add centroid (plot�� 3�� Ŭ�������� �߽���ǥ ǥ��)
  geom_point(mapping = aes_string(x = iris_kkmeans$centers[, "Sepal.Length"], 
                                  y = iris_kkmeans$centers[, "Petal.Length"]),
             color = "black", size = 2)

# centroids ��ġȮ��
View(iris_kkmeans$centers)

# �з��� �󸶳� �� �Ǿ����� ���̺��� Ȯ��
iris_z$Species <- iris$Species
table(iris_z$Species, iris_z$cluster)

##############################################################################
# 3.2������ ���� ����� ����, ������ iris �����Ͱ� �ԷµǾ��� ��,
# ��� Ŭ�����Ϳ� ���ϴ��� ����ϴ� ���α׷��� �ۼ��ϼ���.
# -[1.2, 2.1, 1.0, 1.5]-> �Է� -> 3�� Ŭ������

newRow <- data.frame('Sepal.Length'=1.2, 'Sepal.Width'=2.1, 'Petal.Length'=1.0, 'Petal.Width'=1.5)
iris_n <- rbind(iris[-5],newRow)
iris_nz <- as.data.frame(scale(iris_n))

c2 <- dist(rbind(iris_kkmeans$centers[1,],iris_nz[151,]))
c1 <- dist(rbind(iris_kkmeans$centers[2,],iris_nz[151,]))
c3 <- dist(rbind(iris_kkmeans$centers[3,],iris_nz[151,]))
newRow$cluster <- which.min(c(c1,c2,c3))