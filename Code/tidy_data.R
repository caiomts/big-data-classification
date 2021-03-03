install.packages("rpart.plot")
install.packages("e1071")


### Libraries
library(data.table)
library(tidyverse)
library(R.utils)
library("rpart") 
library("rpart.plot")
library("e1071") 

### Read Data
column_names = read.csv("C:\\Users\\Ziko Nguyen\\Dropbox\\20.Ziko\\Ziko\\Master 2\\UE07-Big Data\\Project\\big_data\\Data\\names.csv", header=FALSE) # Column names!
dt = fread("C:\\Users\\Ziko Nguyen\\Dropbox\\20.Ziko\\Ziko\\Master 2\\UE07-Big Data\\Project\\big_data\\Data\\census-income.data.gz", col.names=column_names[,2])
df = as_tibble(dt)

### Tidying data
df = df %>% 
  unique() %>% # Dropping duplicates
  mutate(AHRSPAY = AHRSPAY/100) # wage per hour in unit of dollars

df[df == '?'] = NA # Replace "?" by NA


decision_tree <- rpart(TOTINC  ~ AAGE+ACLSWKR+AHGA+AHRSPAY+AMARITL+
                         ARACE+AWKSTAT+PARENT+PRCITSHP+	
                         SEOTR , method="class", data=dt, control=rpart.control(minsplit=1), parms=list(split="information")) 


summary(decision_tree)
rpart.plot(decision_tree, type=2, extra=1) 


###PREDICT
new_customer<-data.frame(AAGE=45, ACLSWKR="Private",ADTIND=30 ,ADTOCC=33, AHGA="Bachelors degree(BA AB BS)", AHRSPAY =0,
                         AHSCOL="Not in universe", AMARITL= "Never married",AMJIND="Hospital services", AMJOCC="Professional specialty", 
                         ARACE="White", AREORGN="All other", ASEX="Female", AUNMEM="Not in universe",AUNTYPE="Not in universe", 
                         AWKSTAT="Full-time schedules", CAPGAIN=0, CAPLOSS=0, DIVVAL=0, FILESTAT="Single",GRINREG="Not in universe",
                         GRINST="Not in universe", HHDFMX="Nonfamily householder",HHDREL="Householder",MARSUPWT="1095.21",MIGMTR1="Nonmover", 
                         MIGMTR3="Nonmover", MIGMTR4=" Nonmover", MIGSAME="Yes", MIGSUN="Not in universe", NOEMP=6, PARENT="Not in universe",
                         PEFNTVTY="Germany",PEMNTVTY="El-Salvador", PENATVTY="Mexico", PRCITSHP="Native- Born in the United States", SEOTR=0,
                         VETQVA="Not in universe", VETYN=0, WKSWORK=52, year=95)

new_customer


predict(decision_tree,newdata=new_customer,type="class")
####PREDICT observation 123####
new_customer2<-data.frame(AAGE=45, ACLSWKR="Self-employed-incorporated",ADTIND=30 ,ADTOCC=33, AHGA="Prof school degree (MD DDS DVM LLB JD)", AHRSPAY =0,
                         AHSCOL="Not in universe", AMARITL= "Married-civilian spouse present",AMJIND="Hospital services", AMJOCC="Professional specialty", 
                         ARACE="White", AREORGN="All other", ASEX="Female", AUNMEM="Not in universe",AUNTYPE="Not in universe", 
                         AWKSTAT="Full-time schedules", CAPGAIN=0, CAPLOSS=0, DIVVAL=0, FILESTAT="Single",GRINREG="Not in universe",
                         GRINST="Not in universe", HHDFMX="Nonfamily householder",HHDREL="Householder",MARSUPWT="1095.21",MIGMTR1="Nonmover", 
                         MIGMTR3="Nonmover", MIGMTR4=" Nonmover", MIGSAME="Yes", MIGSUN="Not in universe", NOEMP=6, PARENT="Not in universe",
                         PEFNTVTY="Germany",PEMNTVTY="El-Salvador", PENATVTY="Mexico", PRCITSHP="Foreign born- U S citizen by naturalization", SEOTR=0,
                         VETQVA="Not in universe", VETYN=0, WKSWORK=52, year=95)

new_customer2


predict(decision_tree,newdata=new_customer2,type="class")

####	Naïve Bayes with R###
dt$AHGA2<-ifelse(dt$AHGA=="10th grade, 11th grade, 12th grade no diploma, 1st 2nd 3rd or 4th grade, 5th or 6th grade, 7th and 8th grade
9th grade, Associates degree-academic program, Associates degree-occup /vocational,Children, High school graduate, Less than 1st grade,Some college but no degree","10th grade, 11th grade, 12th grade no diploma, 1st 2nd 3rd or 4th grade, 5th or 6th grade, 7th and 8th grade
9th grade, Associates degree-academic program, Associates degree-occup /vocational,Children, High school graduate, Less than 1st grade,Some college but no degree","Bachelors degree(BA AB BS),Doctorate degree(PhD EdD), Masters degree(MA MS MEng MEd MSW MBA), Prof school degree (MD DDS DVM LLB JD)") 

training_data <- as.data.frame(dt[1:dim(dt)[1]-1,]) 
test_data <- as.data.frame(dt[dim(dt)[1],])

test_data

model <- naiveBayes(TOTINC  ~ AAGE+ACLSWKR+AHGA+AHRSPAY+AMARITL+
                         ARACE+AWKSTAT+PARENT+PRCITSHP+SEOTR, training_data)
model
#write.csv(dt,"C:\\Users\\Ziko Nguyen\\Dropbox\\20.Ziko\\Ziko\\Master 2\\UE07-Big Data\\Project\\big_data\\data.csv", row.names=FALSE)

