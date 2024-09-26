# Oscar Misiewicz 
# 11/7/23

library(ggplot2)
# Females were more likely to survive than males, shown in the bar graph.
Data <- W13759_XLS_ENG 

Data$sex <- factor(Data$sex)
Data$survived <- factor(Data$survived)

ggplot(Data, aes(x = sex, fill = survived)) +
  geom_bar()

# Children are more likely to survive, their survival rate was 49% compared to
# adults, which had a survival rate of 36%
Data$adult <- factor(Data$adult)

ggplot(Data, aes(x = adult, fill = survived)) +
  geom_bar()

children_survival <- table(Data$adult, Data$survived)
prob_child <- children_survival[1,2] / (children_survival[1,1] + children_survival[1,2])
prob_adult <- children_survival[2,2] / (children_survival[2,1] + children_survival[2,2])
prob_child
prob_adult

# Age was a slight factor in survival. The older a person was, the older a 
# person was, the more likely they were to survive. 
Adult_Data <- subset(Data, adult == 1)
ggplot(Adult_Data, aes(x = age, y = survived)) +
  geom_boxplot()

# Passenger class was a factor in survival, people in the first passenger
# class were more likely to survive, while people in the third passenger class
# weren't as likely to survive
Data$pclass <- factor(Data$pclass)
ggplot(Data, aes(x = pclass, fill = survived)) +
  geom_bar()

# Port of embarkation was a factor in survival, the probability of survival
# of embarked C was the highest (55.8%) compared to the port s and q which had
# the probability of survival to be 33.2% and 35.7%. Port c has the highest survival
# rate. 
Data$embarked <- factor(Data$embarked)
ggplot(Data, aes(x=embarked, fill = survived)) +
  geom_bar()

embarked_table <- table(Data$embarked, Data$survived)
embarked_table

prob_embarked_c <- embarked_table[1,2] / (embarked_table[1,1] + embarked_table[1,2])
prob_embarked_c

prob_embarked_q <- embarked_table[2,2] / (embarked_table[2,1] + embarked_table[2,2])
prob_embarked_q

prob_embarked_s <- embarked_table[3,2] / (embarked_table[3,1] + embarked_table[3,2])
prob_embarked_s

# We can see from the step-wise function that having sex being 1 (someone being female)
# has a much higher chance to survive. Also, we can see that a combination of 
# sex, passenger class, adult, embarked all contribute to survival. 
indx <- sample(2,nrow(Data), replace = "True", prob = c(.8,.2))
train <- Data[indx == 1, ]
test <- Data[indx == 2, ]


null <- glm(survived ~ 1, train, family = "binomial", maxit = 1000)
full <- glm(survived ~ pclass + sex + adult + embarked, train, family = "binomial", maxit = 1000)

step(null, scope = list(lower = null, upper = full), direction = "forward")

Data$age_categories <- factor(Data$age_categories)
Data$boat <- factor(Data$boat)
log_reg <- glm(survived ~ pclass + sex + adult + embarked + age + fare, train, family = "binomial")
log_reg

# The logistic regression model works pretty well, after running the
# confusion matrix we can see that we have an accuracy of 75% - 80% (depending on training data).
library(caret)
pred <- predict(log_reg, test, type = "response")
pred_glm <- as.factor(ifelse(pred > .5, "1", "0"))

conf_matrix <- confusionMatrix(pred_glm, test$survived)
conf_matrix

