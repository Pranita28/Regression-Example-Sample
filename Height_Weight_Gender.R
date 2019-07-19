#Set the working directory
setwd("C:/Users/pranita.patil/Documents/Hackathon/Height_Weight_Gender")

#Read the data
data <- read.csv("weight-height.csv")

#Load the required libraries
library(ggplot2)
library(reshape2)
library(plyr)

#Find the structure of the data
str(data)

#Check if we have any null values
colSums(is.na(data)) #We do not have any NA values in the dataset

#Plot the graph of height against weight
#Scatterplot
plot(x = data$'Height', y = data$'Weight',main = "Height vs Weight")

#Draw a scatterplot with the scatterplot function
library(car)
scatterplot(Height ~ Weight,data = data,main = "Height_vs_Weight")

#Scatterplot shows a strong relationship between height and weight
model <- lm(Weight ~ Height, data = data)
summary(model)

#We see an extreme strong linear relationship between Height and Weight
#Now we subset the data between male and female
male <- subset(data, Gender == "Male" )
female <- subset(data,Gender == "Female")

#Find the summary of both the gender datasets
summary(male)
summary(female)

#A another way to find the sumarry of both the genders
ddply(data, .(Gender), function(df) summary(df$Height))

#Plot the density by the gender 
plot(density(male$Height))
plot(density(female$Height))

#Overlay one plot over the another
density_gender <- ggplot(data = data,aes(x = Height,color = Gender)) +
  geom_density()

density_gender

#Q-Q Plot
qq_gender <- ggplot(data = data, aes(sample = Height)) + geom_point(stat = "qq") + 
  facet_wrap(~Gender)
qq_gender

#Relationship between height, weight and gender by distribution
ht_wt_pt_gender <- ggplot(data = data, aes(x = Height, y = Weight, color = Gender)) + 
  geom_point(alpha = 0.2)
ht_wt_pt_gender

#Model with Gender in it
model_all <- lm(Weight ~ Height + Gender,data = data)
summary(model_all)
