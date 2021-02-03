## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL ## initialize inverse to none
  
  set <- function(x) { ## set x to input value, and resets inverse to null
    x <<- x
    inverse <<- NULL
  }
  get <- function() x ## getter for x
  
  getinverse <- function() inverse  ## getter for inverse
  
  setinverse <- function(inv) inverse <<- inv ## setter for inverse
  
  list(set = set,
       get = get,
       getinverse = getinverse,
       setinverse = setinverse)  ## returns this named list

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() ## retrieves inverse
  
  if(!is.null(inverse)) { ## check for inverse is null or cache exists.
      message("getting cached data")
      return(inverse) ## return cached inverse
  }
  
  data <- x$get()  ## get input x matrix
  inverse <- solve(data, ...) ## computes inverse using solve
  x$setinverse(inverse)  ## setting computed inverse
  inverse  ## returning inverse
}
## define new matrices for testing
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

##creating matrix object by calling function makeCacheMatrix with matrix, m1
myMatrix_object <- makeCacheMatrix(m1)
## calling function cacheSolve for inverse
cacheSolve(myMatrix_object)
## repeats calling for checking cache is working or not
cacheSolve(myMatrix_object)

## setting new matrix n2
myMatrix_object$set(n2)
## calling function cacheSolve for inverse
cacheSolve(myMatrix_object)
## repeats calling for checking cache is working or not
cacheSolve(myMatrix_object)


