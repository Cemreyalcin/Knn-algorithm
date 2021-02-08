#**Gerekli Kütüphaneler**

library("class")
library("ggplot2")
library("png")
library("grid")

attach(iris)

# Veri Seti 

#Veri kümesi, her biri bir tür iris bitkisine atýfta bulunan 50 örneklik 3 sýnýf içerir.
#Yanýt deðiþkeni: iris bitkisinin sýnýfý.
#1. sepal length (çanak yaprak uzunluðu) in cm 
#2. sepal width (çanak yaprak geniþliði) in cm 
#3. petal length (taç yaprak uzunluðu) in cm 
#4. petal width (taç yaprak geniþliði) in cm 
#5. class: Iris Setosa- Iris Versicolour- Iris Virginica

summary(iris)
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

# Veri Seti Görselleþtirmesi

ggplot(data = iris,aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()

# Train-Test Ayrýmý

smp_size <- floor(0.6 * nrow(iris))
set.seed(123) 
train_ind <- sample(nrow(iris), size = smp_size, replace = FALSE)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

# Optimum k deðerini bulmak için train verisindeki gözlem sayýsýnýn karekökü hesaplanýr.

sqrt(90)

iris_pred2 <- knn(train = train[,1:4], test = test[,1:4], cl = train[,5], k=9)
iris_pred3 <- knn(train = train[,1:4], test = test[,1:4], cl = train[,5], k=15)
iris_pred4 <- knn(train = train[,1:4], test = test[,1:4], cl = train[,5], k=25)

#Algoritma kurulurken optimum k deðerine yakýn olan deðerden baþlayarak k arttýrýlmýþtýr.

# Confusion Matrix 

cm2 <- table(test$Species, iris_pred2) 
cm2 

#Algoritmada k=9 alýndýðýnda, 

cm3 <- table(test$Species, iris_pred3) 
cm3 

#Algoritmada k=15 alýndýðýnda, 

cm4 <- table(test$Species, iris_pred4) 
cm4 

#Algoritmada k=25 alýndýðýnda, 

misClassError2 <- mean(iris_pred2 != test$Species) 
misClassError3 <- mean(iris_pred3 != test$Species) 
misClassError4 <- mean(iris_pred4 != test$Species) 

Accuracy2=1-misClassError2
Accuracy3=1-misClassError3
Accuracy4=1-misClassError4

cbind(Accuracy2,Accuracy3,Accuracy4)

#Algoritmada k=9 alýndýðýnda, %95 doðruluk elde edilmiþtir.
#Algoritmada k=9 alýndýðýnda, %93.3 doðruluk elde edilmiþtir.
#Algoritmada k=9 alýndýðýnda, %91.7 doðruluk elde edilmiþtir.
#Optimum k deðerinden uzaklaþtýkça accuracy deðerinin düþtüðü gözlemlenmiþtir.


