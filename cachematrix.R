## Put comments here that give an overall description of what your
## functions do

##There are mainly two functions makeCacheMatrix, makeCacheSolve
##makeCacheMatrix consists of set,get,setinv,getinv
##in here library(MASS) is used in calculating the inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                          #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x                   #function in getting matrix x
  setinv <- function(inverse)inv <<- inversse
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x        #function in obtaining the inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##This function is used to get the cache data

cacheSolve <- function(x, ...)          #function in getting cache data
{
  inv <- x$getinv()
  if(!is.null(inv)){                    #function in checking whether inverse is NULL
    message("getting cached data")
    return(inv)         #returns inverse value
  }                    
  data <- x$get()
  inv <- solve(data, ...)               #function in calculating inverse value
  x$setinv(inv)
  inv                                   ## Return a matrix that is the inverse of 'x'
}
