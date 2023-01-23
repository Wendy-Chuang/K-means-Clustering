#Set working directory
setwd("C:/Users/wendy/Desktop")
library("readxl")

#Import data
df <- read.csv("segmented_customers.csv")
head(df)
df <- as.data.frame(df)

#Dataset dimension
paste(dim(df)[1],'rows','x',dim(df)[2],'columns')

# Check if there is any NA or blank in df
lapply(df,function(x) { length(which(is.na(x)))})
lapply(df,function(x) { length(which(x==""))})

#Boxplots
boxplot(df$Age, horizontal = TRUE,col = 'Light Blue',main="Age")
boxplot(df$Annual_Income, horizontal = TRUE,col = 'Light Blue',main="Annual Income")
boxplot(df$Spending_Score, horizontal = TRUE,col = 'Light Blue',main="Spending Score")


hist(df$Age,
    col="lightblue",
    main="Distribution of Age Class",
    xlab="Age Class",
    ylab="Frequency",
    labels=TRUE)

hist(df$Annual_Income,
    col="lightblue",
    main="Distribution of Annual Income",
    xlab="Annual Income",
    ylab="Frequency",
    labels=TRUE)

hist(df$Spending_Score,
    col="lightblue",
    main="Distribution of Spending Score",
    xlab="Spending Score",
    ylab="Frequency",
    labels=TRUE)
 
#Use Elbow Method to determine k
library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(df[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
    type="b", pch = 19, frame = FALSE, 
    xlab="Number of K Clusters",
    ylab="Total intra-clusters sum of squares")

k4 <- kmeans(df[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
df[,6] <- data.frame(k4[1])
k4

pcclust=prcomp(df[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]

set.seed(123)
ggplot(df, aes(x =Annual_Income, y = Spending_Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k4$cluster))) +
  scale_color_discrete(name=" ",
              breaks=c("1", "2", "3", "4"),
              labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Mall Customers Segmentations")
