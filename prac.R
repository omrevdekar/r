data("iris")	

ni<-iris

ni$Species <- NULL

kc <- kmeans(ni,centers = 3)

plot(ni[c("Sepal.Length","Sepal.Width")],col = kc$cluster,main = "K-means Clustering of IRIS ")

points(kc$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 2)

----

library(party)

print(head(readingSkills))

input.dat <- readingSkills[1:105,]

output.tree <- ctree(nativeSpeaker~ age +shoeSize+score,data = input.dat)

plot(output.tree)

----

x<- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

model<- lm(y~x)

print(model)
print(summary(model))

newh <- data.frame(x=170)
pw <- predict(model,newh)

print(pw)

plot(x,y,col = "blue",main ="HEIGHT AND WEIGHT REGRESSION",
xlab= "height in cm",ylab="Weight in kg ",pch=16,cex=1.3)

abline(model,lwd=2,col="red")

----

input <- mtcars[,c("am", "cyl", "hp", "wt")]
print(head(input))
am.data <- glm(am ~ cyl + hp + wt,data =input ,family =binomial)
print(summary(am.data))
