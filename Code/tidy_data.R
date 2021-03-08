### Libraries
library(data.table)
library(tidyverse)
library(R.utils)
library(rpart) 
library(rpart.plot)
library(e1071)
library(lattice)
library(plyr)
library(caret)
library(ROCR)

### Read Data
setwd(".\\Code")

column_names <- read.csv("..\\Data\\names.csv", header=FALSE) # Column names!
dt <- fread("..\\Data\\census-income.data.gz", col.names=column_names[,2],
            stringsAsFactors=TRUE)
training_df <- tibble(dt)
dt <- fread("..\\Data\\census-income.test.gz", col.names=column_names[,2],
            stringsAsFactors=TRUE)
test_df <- tibble(dt)

### Tidying data


training_df <- training_df %>% 
    unique() %>% # Dropping duplicates
    mutate(AHRSPAY = AHRSPAY/100) # wage per hour in unit of dollars

training_df[training_df == '?'] = NA # Replace "?" by NA


test_df <- test_df %>% 
  unique() %>% # Dropping duplicates
  mutate(AHRSPAY = AHRSPAY/100) # wage per hour in unit of dollars

test_df[test_df == '?'] = NA # Replace "?" by NA


ag_df <- rbind(training_df, test_df) # One df for statistical description


# The instance weight (MARSUPWT) indicates the number of people in the population
# that each record represents due to stratified sampling.
# To do real analysis and derive conclusions, this field must be used.
# This attribute should *not* be used in the classifiers, so it is
# set to "ignore" in this file.


### Statistical description

summary(ag_df)

# NA's - Drop all because there is more NA's than not NA
# MIGMTR1 - 147485
# MIGMTR3 - 147485
# MIGMTR4 - 147485
# MIGSUN - 147485

ag_df <- ag_df %>%
  select(-c(MIGMTR1, MIGMTR3, MIGMTR4, MIGSUN, MARSUPWT, year))

training_df <- training_df %>%
  select(-c(MIGMTR1, MIGMTR3, MIGMTR4, MIGSUN, MARSUPWT, year))

test_df <- test_df %>%
  select(-c(MIGMTR1, MIGMTR3, MIGMTR4, MIGSUN, MARSUPWT, year))

# Num but Factor ADTIND, ADTOCC, SEOTR, VETYN + Label change 0, 1 for income

ag_df <- ag_df %>%
  mutate(ADTIND = as.factor(ADTIND)) %>%
  mutate(ADTOCC = as.factor(ADTOCC)) %>%
  mutate(SEOTR = as.factor(SEOTR)) %>%
  mutate(VETYN = as.factor(VETYN)) %>%
  mutate(TOTINC = fct_recode(TOTINC, "0" = "- 50000.", "1" = "50000+." ))

training_df <- training_df %>%
  mutate(ADTIND = as.factor(ADTIND)) %>%
  mutate(ADTOCC = as.factor(ADTOCC)) %>%
  mutate(SEOTR = as.factor(SEOTR)) %>%
  mutate(VETYN = as.factor(VETYN)) %>%
  mutate(TOTINC = fct_recode(TOTINC, "0" = "- 50000.", "1" = "50000+." ))

test_df <- test_df %>%
  mutate(ADTIND = as.factor(ADTIND)) %>%
  mutate(ADTOCC = as.factor(ADTOCC)) %>%
  mutate(SEOTR = as.factor(SEOTR)) %>%
  mutate(VETYN = as.factor(VETYN)) %>%
  mutate(TOTINC = fct_recode(TOTINC, "0" = "- 50000.", "1" = "50000+." ))


# Histograms

nums <- unlist(lapply(ag_df, is.numeric))

ggplot(gather(ag_df[ , nums]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# 3 Capital Variables. They have basically the same behavior, so we group it
# in only one variable CAP.CHANGE

ggplot(gather(ag_df[ , c("CAPGAIN", "CAPLOSS", "DIVVAL")]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

ag_df <- ag_df %>%
  mutate(CAP.CHANGE = CAPGAIN - CAPLOSS + DIVVAL) %>%
  select(-c(CAPGAIN, CAPLOSS, DIVVAL))

training_df <- training_df %>%
  mutate(CAP.CHANGE = CAPGAIN - CAPLOSS + DIVVAL) %>%
  select(-c(CAPGAIN, CAPLOSS, DIVVAL))

test_df <- test_df %>%
  mutate(CAP.CHANGE = CAPGAIN - CAPLOSS + DIVVAL) %>%
  select(-c(CAPGAIN, CAPLOSS, DIVVAL))


ggplot(gather(ag_df[ , "CAP.CHANGE"]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# distribution factors

not_nums <- unlist(lapply(ag_df, is.factor))

ggplot(gather(ag_df[ , not_nums]), aes(value)) + 
  geom_histogram(stat="count") + 
  facet_wrap(~key, scales = 'free_x')

str(ag_df[ , not_nums])

# PEFNTVTY, PEMNTVTY, PENATVTY, GRINST - Lot of Factor All US much bigger
# VETQVA - Not in universe 292162!
# DROP NA

summary(ag_df[ , c("PEFNTVTY", "PEMNTVTY", "PENATVTY", "GRINST")])

ag_df <- ag_df %>%
  select(-c(PEFNTVTY, PEMNTVTY, PENATVTY, GRINST, VETQVA)) %>%
  drop_na()

training_df <- training_df %>%
  select(-c(PEFNTVTY, PEMNTVTY, PENATVTY, GRINST, VETQVA)) %>%
  drop_na()

test_df <- test_df %>%
  select(-c(PEFNTVTY, PEMNTVTY, PENATVTY, GRINST, VETQVA)) %>%
  drop_na()


# Correlation Matrix between numerical variables

nums <- unlist(lapply(ag_df, is.numeric))
res <- cor(ag_df[ , nums])
round(res, 2)

# WKSWORK with NOEMP highly correlated

boxplot(WKSWORK~TOTINC, data=ag_df) #WKSWORK is highly correlated with Income

boxplot(NOEMP~TOTINC, data=ag_df) #NOEMP is highly correlated with Income

# Correlation between Categorical and Income

qplot(TOTINC, data = ag_df, fill=ACLSWKR) +
  facet_grid(.~ACLSWKR)

qplot(TOTINC, data = ag_df, fill=GRINREG) +
  facet_grid(.~GRINREG)


#### Logistic Regression


logit_model <- glm(TOTINC~., data=training_df, binomial(link="logit"))

summary(logit_model)

pseudo_r2 = 1 - (summary_model$null/summary_model$deviance)

varImp(logit_model)

predict_logit_model <- predict(logit_model, newdata = test_df, 
                                 type = "response")

predict_logit_model_factor <- as.factor(if_else(prediction_test > 0.5, 1, 0))


### Results
confusionMatrix(predict_logit_model_factor, test_df$TOTINC)

prop.table(table(test_df$TOTINC, predict_logit_model > 0.5))

# 92.85% True < 50,000; 2.4% True > 50,000
# Type II Error 3.9% < 50,000 but in reality >50,000
# Type I Error 0,88% > 50,000 but in reality < 50,000

# ROC and AUC


pred_logit <- prediction(predict_logit_model, test_df$TOTINC)

performance(pred_logit, "tpr", "fpr")

auc <- performance(pred_logit, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

perf <- performance(pred_logit, "tpr", "fpr")

plot(perf, lwd=2, xlab="False Positive Rate (FPR)", 
    ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)

### Decision Tree

decision_tree <- rpart(TOTINC  ~., 
                      method="class", data=training_df, 
                      control=rpart.control(minsplit=1), 
                      parms=list(split="information"))

predict_tree <- predict(decision_tree, newdata=test_df, type='class')

summary(decision_tree)

rpart.plot(decision_tree, type=2, extra=1) 

# Results
confusionMatrix(predict_tree, test_df$TOTINC)

prop.table(table(test_df$TOTINC, predict_tree))

# 92.98% True < 50,000; 1.83% True > 50,000
# Type II Error 4.44% < 50,000 but in reality >50,000
# Type I Error 0,76% > 50,000 but in reality < 50,000

# ROC and AUC

pred_tree <- prediction(c(predict_tree), c(test_df$TOTINC))

performance(pred_tree, "tpr", "fpr")

auc <- performance(pred_tree, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

perf <- performance(pred_tree, "tpr", "fpr")

plot(perf, lwd=2, xlab="False Positive Rate (FPR)", 
     ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)



####	Naïve Bayes

model_naive <- naiveBayes(TOTINC  ~., training_df)

predict_naive <- predict(model_naive, newdata=test_df)

# Results
confusionMatrix(predict_naive, test_df$TOTINC)

prop.table(table(test_df$TOTINC, predict_naive))

# 74.64% True < 50,000; 5.45% True > 50,000
# Type II Error 0.81% < 50,000 but in reality >50,000
# Type I Error 19,09% > 50,000 but in reality < 50,000

# ROC and AUC

pred_naive <- prediction(c(predict_naive), c(test_df$TOTINC))

performance(pred_naive, "tpr", "fpr")

auc <- performance(pred_naive, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

perf <- performance(pred_naive, "tpr", "fpr")

plot(perf, lwd=2, xlab="False Positive Rate (FPR)", 
     ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)

