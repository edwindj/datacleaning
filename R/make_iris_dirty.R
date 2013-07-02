
library(stringr)
data('iris')

dirty_iris <- iris

# insert 10% empty values in the numerical columns
set.seed(1976)
nNA = 4*nrow(iris)/10


A <- cbind(
  sample(1:150, nNA, replace=TRUE), 
  sample(1:4, nNA,replace=TRUE)
)
dmp <- apply(A,1,function(ij) dirty_iris[ij[1],ij[2]] <<- NA) 

# A number of records were entered in the wrong unit (factor 10).
i <- sample(1:150,5,replace=FALSE)
dirty_iris[i,1:4] <- dirty_iris[i,1:4]*10

# Some fields were not filled in and were automagically converted to 0
A <- cbind(
  sample(1:150,3,replace=FALSE),
  sample(1:4,3,replace=FALSE)
)
dmp <- apply(A,1,function(ij) dirty_iris[ij[1],ij[2]] <<- 0)

# people make spelling errors and use different cases
dirty_iris[,5] <- str_trim(format(dirty_iris[,5]))
I <- which(dirty_iris[,5] == 'setosa')
dirty_iris$Species[I[1:3]] <- c('Setosa',' setosa','Setaso')
I <- which(dirty_iris$Species == 'versicolor')
dirty_iris$Species[I[1:2]] <- 'Versicolor'
I <- which(dirty_iris$Species == 'virginica')
dirty_iris$Species[I[1:4]] <- c('Virginica','Virginia','virinica','Virginica')

# random ordering
dirty_iris <- dirty_iris[sample(nrow(iris)),]

write.csv(dirty_iris,file="dirty_iris.csv",row.names=FALSE)
















