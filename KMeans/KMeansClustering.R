#KMeans
#Unsupervised Learning algorithm.

#Get the data
df_red <- read.csv('winequality-red.csv', sep = ';')
df_white <- read.csv('winequality-white.csv', sep = ';')

#Now, add a label col to both df_red,df_white indicating a label 'red' & 'white'
df_red$label <- 'red'
df_white$label <- 'white'


#Head of df
head(df_red)
head(df_white)

#Combine df_red and df_white to a single data frame as wine
wine <- rbind(df_red, df_white)

#Structure of the wine df
str(wine)

#-------------------------@EDA@-------------------------------
library(ggplot2)
#Histogram of residual sugar from wine
pl1 <- ggplot(wine, aes(x=residual.sugar)) + geom_histogram(aes(fill=label),
                                                     color='black', bins=40)
pl1 + scale_fill_manual(values=c('#ae4554', '#faf7ea')) + theme_bw()

#Histogram of citric.acid
pl2 <- ggplot(wine, aes(x=citric.acid)) + geom_histogram(aes(fill=label), color='black', bins=40)
pl2 + scale_fill_manual(values= c('#ae4554', '#faf7ea')) + theme_bw()

#Histogram of alcohol from the wine
pl3 <- ggplot(wine, aes(x=alcohol)) + geom_histogram(aes(fill=label),
                                                     color='black', bins=40)
pl3 + scale_fill_manual(values= c('#ae4554', '#faf7ea')) + theme_bw()

#Scatterplot of residual.sugar versus citric.acid
pl4 <- ggplot(wine, aes(x=citric.acid, y=residual.sugar)) + geom_point(aes(color=label), alpha=0.2)
pl4 + scale_color_manual(values = c('#ae4554', '#faf7ea')) + theme_bw()

#Scatterplot of volatile.acidity vs residual.sugar
pl5 <- ggplot(wine, aes(x=volatile.acidity, y=residual.sugar)) + geom_point(aes(color=label), alpha=0.2)

#Grab the wine data without label
clustering.data <- wine[, 1:12]

#Checking the head of the df
head(clustering.data)

#Building the Clusters
wine.cluster <- kmeans(wine[1:12], 2)
print(wine.cluster)

#Evaluating the clusters
#Generally, as it is a Unsupervised algorithm, we don't have any label to check
#how well the model is performed in the real world problems.
#Bt, for simplicity, we have included labels to check how well the model performed
table(wine$label, wine.cluster$cluster)

#It's important to note here, that K-Means can only give you the clusters, it can't directly tell you what 
#the labels should be, or even how many clusters you should have, we are just lucky to know we expected two 
#types of wine.

print(wine.cluster$cluster)












