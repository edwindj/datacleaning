library(editrules)

# 3.1a
iris <- read.csv("dirty_iris.csv", stringsAsFactors=FALSE)

# 3.1b
# what percentage of observations are complete?
(n <- complete.cases(iris))
sum(n)/nrow(iris)


# 3.1c
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

sapply(iris, is.special)
for (n in colnames(iris)){
  is.na(iris[[n]]) <- is.special(iris[[n]])
}
summary(iris)

# 3.1d
E <- editfile("edits3.txt")
print(E)


# 3.1e
# What percentage of records has no errors?
violated <- violatedEdits(E, iris)
summary(violated)
plot(violated)

# 3.1f
# percentage of edits without any errors
sum(apply(violated, 1, sum)==0, na.rm=T) / nrow(iris)

# 3.1g
iris[which(violated[,"num6"]),]

# 3.1h
boxplot(iris$Sepal.Length)
outliers <- boxplot.stats(iris$Sepal.Length)$out
outliers_idx <- which(iris$Sepal.Length %in% outliers)
iris[outliers_idx,]
# they all seem to be too big... may they were measured in mm i.o cm?
iris[outliers_idx,1:4] <- iris[outliers_idx,1:4]/10
summary(iris)

# Note that 
boxplot(Sepal.Length ~ Species, data=iris)
# shows an extra outlier!

# 3.3a
library(deducorrect)
cr <- correctionRules(expression(
  if (!is.na(Sepal.Width) && Sepal.Width <=0 ) Sepal.Width = NA
  ))
correctWithRules(cr, iris)
# correctWithRules

#3.3b
iris[localizeErrors(E, iris)$adapt] <- NA
# anything violated?
any(violatedEdits(E,iris), na.rm=TRUE)
# 3.4a
library(VIM)
iris_knn <- kNN(iris)
petal_width_kNN <- iris_knn$Petal.Width

# 3.4b
seqImpute <- function(x, last=max(x, na.rm=TRUE)){
  n <- length(x)
  x <- c(x, last)
  i <- is.na(x)
  while (any(i)){
    x[i] <- x[which(i)+1]
    i <- is.na(x)
  }
  x[1:n]
}

o <- order(iris$Species)
petal_width <- iris$Petal.Width[o]
petal_width_hd <- seqImpute(petal_width)

summary(petal_width_kNN[o] - petal_width_hd)

# 3.4c
o <- order(iris$Species,iris$Sepal.Length)
petal_width <- iris$Petal.Width[o]
petal_width_hd <- seqImpute(petal_width)

summary(petal_width_kNN[o] - petal_width_hd)
