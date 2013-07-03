library(editrules)
oldwd <- setwd("R")

iris <- read.csv("dirty_iris.csv")
E <- editfile("edits3.txt")
E

# what percentage of observations are complete?
n <- complete.cases(iris)
sum(n)/nrow(iris)

# What percentage of records has no errors?
violated <- violatedEdits(E, iris)
summary(violated)
plot(violated)
# percentage of edits without any errors
sum(apply(violated, 1, sum)==0, na.rm=T) / nrow(iris)

E