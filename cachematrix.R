## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## 1.	set the value of the vector
## 2.	get the value of the vector
## 3.	set the value of the mean
## 4.	get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  # initialize value of inverse matrix
  inverse1 <- NULL
  # set value of matrix from user input y
  set <- function(y) {
    # sets to global x value of matrix  user input y
    x <<- y
    # initialize inverse matrix
    inverse1 <<- NULL
  }
  # get value of matrix if it has been set
  get <- function() x
  # set value of the inverse of matrix
  setinverse <- function(solve) inverse1 <<- solve
  # get value of the inverse matrix
  getinverse <- function() inverse1
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
}


## Write a short comment describing this function
## The following function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in 
## the cache via the setmean function.
  cacheinverse <- function(x, ...) {
    #get value of inverse
    inverse1 <- x$getinverse()
    #check to see if inverse is already computed
    if(!is.null(inverse1)) {
      message("getting cached data")
      #if inverse is cached, get the cached value and return
      return(inverse1)
    }

    #otherwise calculates inverse of data
    data <- x$get()
    inverse1 <- solve(data, ...)
    x$setinverse(inverse1)
    inverse1
  
}
