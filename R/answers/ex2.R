
##### Exercise 2.1
# a) We use a textConnection in stead of a file.

L <- readLines(textConnection(
"// Survey data . Created : 21 May 2013
// Field 1: Gender
// Field 2: Age ( in years )
// Field 3: Weight ( in kg )
M ;28;81.3
male ;45;
Female ;17;57,2
fem .;64;62.8"
))

# b) comments are recognized by a forward slash "/" (not a special character)
I <- grepl("^/",L)
comments <- L[I]
dat <- L[!I]

# c)
library(lubridate)
data_date <- dmy(L[1])

# d)
# d)-(a) this will generate a list of character vecors, possibly of different lenghts/
M <- strsplit(dat,";")

# d)-(b) The maximum number of fields detected
max_fields <- max(sapply(M,length))

#  A very compact solution is:
M1 <- lapply(M,function(x){ 
  length(x) <- max_fields
  x
})

# a more verbose (and perhaps understandable) version is
M2 <- vector(mode='list',length=length(M))
for ( i in 1:length(M) ){
  M2[[i]] <- c(M[[i]], rep(NA, max_fields-length(M[[i]])) )
}

# d)-(c) 'unlist' concatenates all vectors in a list
v <- unlist(M1)
# we need to set by.row=TRUE, since we have row-wise concatenated the records
datamatrix <- matrix(v,nrow=4,byrow=TRUE)

#  separate words
x <- strsplit(L[2:4]," ")
# extract: the ultrashort solution
headings <- sapply(x,`[`,4)
colnames(datamatrix) <- headings


##### Exercise 2.2
# a) use stringsAsFactors!
dat <- as.data.frame(datamatrix,stringsAsFactors=FALSE)

# b) 
library(stringdist)
library(stringr)
# note: the gender column is coded closer to male/female than 'man'/'woman'
searchterms <- c('male','female')
codes <- c('man','woman')
# some normalisation
g <- str_trim(dat$Gender)
g <- tolower(g)

A <- stringdistmatrix(g,searchterms)
i <- apply(A,1,which.min)
newgender <- factor(codes[i])
dat$Gender <- newgender

# c) Sure that you used stringsAsFactors?
dat$Age <- as.integer(dat$Age)

# d) use fixed=TRUE. since the period is a special character
dat$Weight <- gsub(",",".",dat$Weight,fixed=TRUE)
dat$Weight <- as.numeric(dat$Weight)



##### Exercise 2.3

# a)
data(warpbreaks)
names(warpbreaks)[
  sapply(warpbreaks, class) %in% c('numeric','integer')
]

# b) Variable 'breaks' is a count variable. Could be integer
warpbreaks$breaks <- as.integer(warpbreaks$breaks)

# c) The 'type' of an R function is closure: a 
#    function + an enveloping execution environment.







