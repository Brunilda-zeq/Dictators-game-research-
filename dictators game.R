library(readxl)

data=apantiseis
x1=data$filo
x2=data$ilikia
x3=data$ekpedefsi
x4=data$dorea
y1=data$y1
y2=data$y2
y3=data$y3
y4=data$y4
model1=lm(y1 ~ x1+x2+x3+x4+y2+y3+y4)
summary(model1)
model2=lm(y2 ~ x1+x2+x3+x4+y1+y3+y4)
summary(model2)
model3=lm(y3 ~ x1+x2+x3+x4+y1+y2+y4)
summary(model3)
model4=lm(y4 ~ x1+x2+x3+x4+y1+y2+y3)
w=summary(model4)
write.csv(w, "model4")
library(corrplot)
X <- cbind(apantiseis[,1:NCOL(apantiseis)], apantiseis)
Y <- cbind(apantiseis[,5:8], apantiseis)
cort<-cor(data[,c(1:8)])

corrplot(cort,type="lower")
boxplot(y1,main = "boxplot",ylab = "Question 1",col = "plum4",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
boxplot(y2,main = "boxplot",ylab = "Question 2",col = "plum4",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

boxplot(y3,main = "boxplot",ylab = "Question 3",col = "plum4",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
boxplot(y4,main = "boxplot",ylab = "Question 4",col = "plum4",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
hist(y1,
     main="	Histogram",
     xlab="QUESTION1 ",
     col="indianred4",
     freq=FALSE
)
hist(y2,
     main="	Histogram",
     xlab="QUESTION 2",
     col="indianred4",
     freq=FALSE
)
hist(y3,
     main="	Histogram",
     xlab="QUESTION 3",
     col="indianred4",
     freq=FALSE
)
hist(y4,
     main="	Histogram",
     xlab="QUESTION 4",
     col="indianred4",
     freq=FALSE
)


y=as.matrix(data$y1)
y=table(y)

barplot(y,
        main="Question 1",
        xlab="Money donated",
        ylab="Count",
        names.arg = c("10", "20-40", "40-60", "60-80", "100"),
        border="slategrey",
        col="maroon")

y=as.matrix(data$y2)
y=table(y)

barplot(y,
        main="Question 2",
        xlab="Money donated",
        ylab="Count",
        names.arg = c("10", "20-40", "40-60", "60-80", "100"),
        border="slategrey",
        col="maroon")

y=as.matrix(data$y3)
y=table(y)

barplot(y,
        main="Question 3",
        xlab="Money donated",
        ylab="Count",
        names.arg = c("10", "20-40", "40-60", "60-80", "100"),
        border="slategrey",
        col="maroon")
y=as.matrix(data$y4)
y=table(y)

barplot(y,
        main="Question 4",
        xlab="Money donated",
        ylab="Count",
        names.arg = c("10", "20-40", "40-60", "60-80", "100"),
        border="slategrey",
        col="maroon")
yo=data.frame(cbind(y1,y2,y3,y4))
length(yo)
a=0
b=0
c=0
d=0
e=0

for (i in 1:150){
        if(x4[i] ==1)
                a=a+1
        if(x4[i] ==2) 
                b=b+1
        if(x4[i] ==3)
                c=c+1
        if(x4[i] ==4)
                d=d+1
        if (x4[i] ==5)
                e=e+1
}
a
b
c
d
e
a=0
b=0
c=0     
for (i in 1:150){
        if(x1[i] ==1)
                a=a+1
        if(x1[i] ==2) 
                b=b+1
        if(x1[i] ==3)
                c=c+1
}
a
b
c
