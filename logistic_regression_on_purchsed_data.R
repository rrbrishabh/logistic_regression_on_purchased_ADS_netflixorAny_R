
# logistic regression on purchaesd data

library(readr)
load_data <- read_csv("Social_Network_Ads.csv")

head(load_data)

str(load_data)

summary(load_data)


load_data$`User ID` = NULL

head(load_data)
View(load_data)

# table function is most important

barplot(table(load_data$Gender))

# scaling of the data

# converting the purchaes column in factor

load_data$Purchased = factor(load_data$Purchased, levels = c(0,1))
load_data


load_data[,2:3] = scale(load_data[,2:3])
load_data

# create train and test dataset

library(caTools)

set.seed(123)


split = sample.split(load_data$Purchased, SplitRatio = 0.75)
train = subset(load_data, split == T)
test = subset(load_data, split == F)

# creat a modal
# glm stands for generalise linear modal 

classifier <- glm(formula = Purchased~., family = "binomial", data = train)
summary(classifier)


# prediction

predict = predict(classifier, test, type = "response")
predict

# response means is the way of telling R to generate output in the form of probability   

test$prediction <- predict
View(test)



test$probabilityAnswer <- ifelse(test$prediction <= 0.5,  0 , 1) 
test

table(test$Purchased, test$probabilityAnswer)
table(test$probabilityAnswer)

# total accuracy is 83 
# and this is called confussion matrix





