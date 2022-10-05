# Loading package
library(caTools)
library(ROCR) 
library(sjPlot)

# Splitting dataset
split <- sample.split(mtcars, SplitRatio = 0.8)
split

train_reg <- subset(mtcars, split == "TRUE")
test_reg <- subset(mtcars, split == "FALSE")

x_weight <- seq(0, 6, 0.1)



# Training model
logistic_model <- glm(vs ~ wt + disp, 
                      data = train_reg, 
                      family = "binomial")
logistic_model

y_weight <- predict(model, list(wt = x_weight), type="response")

plot(mtcars$wt, mtcars$vs, pch=16, xlab="WEIGHT (g)", ylab="VS")
# plot_model(logistic_model, vline.color = "red")   
# plot_model(logistic_model, show.values = TRUE, value.offset = .3) 
lines(x_weight, y_weight)

# Summary
summary(logistic_model)

# Predict test data based on model
predict_reg <- predict(logistic_model, 
                       test_reg, type = "response")
predict_reg  

# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(test_reg$vs, predict_reg)

missing_classerr <- mean(predict_reg != test_reg$vs)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(predict_reg, test_reg$vs) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)