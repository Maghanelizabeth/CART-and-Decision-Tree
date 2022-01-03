setwd('C:/Users/Magha/OneDrive/Documents/R/Machine Learning HW')
alcohol <- read.csv("student-mat.csv")


alcohol <- alcohol[-c(23,5,13,18,31,32,33,26)]
alcohol

sapply(alcohol, class)
alcohol$school<-as.factor(alcohol$school)
alcohol$sex<-as.factor(alcohol$sex)
alcohol$address<-as.factor(alcohol$address)
alcohol$Pstatus<-as.factor(alcohol$Pstatus)
alcohol$Mjob<-as.factor(alcohol$Mjob)
alcohol$Fjob<-as.factor(alcohol$Fjob)
alcohol$reason<-as.factor(alcohol$reason)
alcohol$guardian<-as.factor(alcohol$guardian)
alcohol$schoolsup<-as.factor(alcohol$schoolsup)
alcohol$famsup<-as.factor(alcohol$famsup)
alcohol$activities<-as.factor(alcohol$activities)
alcohol$nursery<-as.factor(alcohol$nursery)
alcohol$higher<-as.factor(alcohol$higher)
alcohol$internet<-as.factor(alcohol$internet)

col_list<-c('age','Medu','Fedu','studytime','failures','famrel','freetime','Dalc','Walc','health','absences')
for (i in col_list){
  #print(i)
  alcohol[paste0(i,"_n")]<-(alcohol[i]-min(alcohol[i]))/(max(alcohol[i])-min(alcohol[i]))
}

#install.packages("rpart")
library("rpart")

cartfit<-rpart(Dalc~age_n+Medu_n+Fedu_n+absences_n+
              failures_n+address+sex+guardian,data=alcohol,method="class")
print(cartfit)
#Save as pdf to zoom in to see it
#install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(cartfit,main="Classification Tree")

