#######################################################################
## Assignment 2: Visualization in R

install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
install.packages("progress")
library("GGally")
## 1. load the iris data package and display summary statistics
data("iris")
summary(iris)

## 2. plot the distribution of Sepal Length
#First a scatter plot
plot(iris$Sepal.Length, ylab = "Sepal Length (cm.)", main = "Sepal Length: Distribution")

#Then a histrogram
hist(iris$Sepal.Length, xlab = "Sepal Length (cm.)", main = "Sepal Length: Histogram")

#Then a boxplot
boxplot(iris$Sepal.Length, main="Sepal Length: BoxPlot", ylab="Sepal Length (cm.)", col="lightgray")


## 3. Visualize the distribution of sepal width against grouping based on species
boxplot(iris$Sepal.Width ~ iris$Species, xlab = "Species", ylab = "Sepal Width (cm.)", main = "Sepal Width by Species", col="lightblue")

#Generate the plot
plot(x=iris$Sepal.Length, y=iris$Petal.Length, main="Sepal Length vs. Petal Length", xlab = "Sepal Length (cm.)", ylab = "Petal Length (cm.)")
#Add a horizontal line for mean Petal Length
abline(h=mean(iris$Petal.Length), lty=2, col="blue")
#Add a text label for the line
text(7.5, 3.9, paste0("Mean Petal Length: ", mean(iris$Petal.Length), "cm.") , col="red", cex = 0.5, font=2)
#Add a vertical line for mean Sepal Length
abline(v=mean(iris$Sepal.Length), lty=2, col="blue")
#Add a text label for the line
text(5.8, 2.5, paste0("Mean Sepal Length: ", round(mean(iris$Sepal.Length), 2), "cm.") , col="red", cex = 0.5, font=2)

## 5. Present the pairwise relationship of all four continuous variables
pairs(iris[, c(1:4)], col=iris$Species)

## 6. Graph the frequency of occurrences for the three groups
barplot(table(iris$Species), xlab = "Species", ylab = "Frequency", main = "Iris: Species & their frequencies")

table(iris$Species)/length(iris$Species)

#################################
##Part 2: plot all plots above using ggplot

## 2. plot the distribution of Sepal Length
#First a scatter plot
ggplot(data = iris)+
  geom_point(aes(x = seq(1, length(iris$Sepal.Length)), y=Sepal.Length))+
  ggtitle("Sepal Length: Distribution")+
  labs(x = "Index")+
  labs(y = "Sepal Length (cm.)")+
  theme_bw()

#Then a histrogram
ggplot(data = iris)+
  geom_histogram(aes(Sepal.Length), binwidth = 0.25)+
  ggtitle("Sepal Length: Histogram")+
  labs(y = "Count")+
  labs(x = "Sepal Length (cm.)")+
  theme_bw()
#Then a boxplot
boxplot(iris$Sepal.Length, main="Sepal Length: BoxPlot", ylab="Sepal Length (cm.)", col="lightgray")
ggplot(data = iris)+
  geom_boxplot(aes(x = "", y=Sepal.Length))+
  ggtitle("Sepal Length: Boxplot")+
  #labs(y = "Count")+
  labs(y = "Sepal Length (cm.)")+
  theme_bw()

## 3. Visualize the distribution of sepal width against grouping based on species
ggplot(data = iris)+
  geom_boxplot(aes(x = Species, y=Sepal.Width), outlier.color = "red", fill = "lightblue")+
  ggtitle("Sepal Width by Species")+
  labs(x = "Species")+
  labs(y = "Sepal Width (cm.)")+
  theme_bw()

## 4. plot the relationship between sepal length and petal length
#Generate the plot
plot(x=iris$Sepal.Length, y=iris$Petal.Length, main="Sepal Length vs. Petal Length", xlab = "Sepal Length (cm.)", ylab = "Petal Length (cm.)")
#Add a horizontal line for mean Petal Length
abline(h=mean(iris$Petal.Length), lty=2, col="blue")
#Add a text label for the line
text(7.5, 3.9, paste0("Mean Petal Length: ", mean(iris$Petal.Length), "cm.") , col="red", cex = 0.5, font=2)
#Add a vertical line for mean Sepal Length
abline(v=mean(iris$Sepal.Length), lty=2, col="blue")
#Add a text label for the line
text(5.8, 2.5, paste0("Mean Sepal Length: ", round(mean(iris$Sepal.Length), 2), "cm.") , col="red", cex = 0.5, font=2)

ggplot(data = iris)+
  geom_point(aes(x = Sepal.Length, y=Petal.Length, color = Species))+
  ggtitle("Sepal Length vs. Petal Length")+
  labs(x = "Sepal Length (cm.)")+
  labs(y = "Petal Length (cm.)")+
  theme_bw()+
  geom_hline(linetype = "dashed", aes(yintercept = mean(Petal.Length)))+
  geom_text(size = 2, aes(7, mean(Petal.Length), label = paste0("Mean Petal Length: ", mean(Petal.Length), "cm."), vjust = -1))+
  geom_vline(linetype = "dashed", aes(xintercept = mean(Sepal.Length)))+
  geom_text(size = 2, aes(6, 2, label = paste0("Mean Sepal Length: ", round(mean(Sepal.Length), 2), "cm.")), angle = 90)
  
## 5. Present the pairwise relationship of all four continuous variables
plotmatrix(iris[, c(1:4)])
ggpairs(iris[, c(1:4)])

## 6. Graph the frequency of occurrences for the three groups
barplot(table(iris$Species), xlab = "Species", ylab = "Frequency", main = "Iris: Species & their frequencies")

ggplot(iris)+
  labs(y = "Count")+
  geom_bar(aes(x = Species))+
    ggtitle("Iris: Species & frequencies of occurence")+
    theme_bw()
    
  

table(iris$Species)/length(iris$Species)