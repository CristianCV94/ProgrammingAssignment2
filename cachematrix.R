## This is about solving the inverse of a matrix by the result
## with a lexical scope function


## The function "make_Cache_Matrix" created a new environment
## 

make_Cache_Matrix <- function(x= matrix()){
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inver <<- inverse}
  getInverse <- function(){inver}
  list(set= set, get=get, setInverse= setInverse, getInverse=getInverse)
}


## The function "cache_Solve" returns the inverse of the matrix returned 
## by the "make_Cache_Matrix" function

cache_Solve <- function(x, ...){
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cathced data")
    return(inver)
  }
  data<- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
}

