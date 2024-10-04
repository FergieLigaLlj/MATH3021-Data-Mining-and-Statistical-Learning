data<-read.csv("Smarket.csv",header=TRUE,stringsAsFactors=T)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train  <- data[sample, ]
test   <- data[!sample, ]
### Logistic Regression
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=train,family=binomial)
summary(glm.fits)
fit.predict = predict(glm.fits,test,type = "response")
pre_lab = rep("Down",247)
pre_lab[fit.predict>.5]="Up"
as.factor(pre_lab)
num_correct = 0
for (x in 1:247) {
  if (pre_lab[x]==test$Direction[x]){
    num_correct = num_correct + 1
  }
}
num_correct
correct_rate = num_correct / 247
message("The correct rate of logistic regression is:",correct_rate)
### Linear discriminant analysis
library(MASS)
lda.fits = lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=train)
lda.fits
fit2.predict = predict(lda.fits,test,type="response")
fit2.predict = fit2.predict$class
as.factor(fit2.predict)
num_correct2 = 0
for (x in 1:247) {
  if (fit2.predict[x]==test$Direction[x]){
    num_correct2 = num_correct2 + 1
  }
}
num_correct2
correct_rate2 = num_correct2 / 247
message("The correct rate of linear discriminant analysis is:",correct_rate2)