## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse

## define the argument with default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inv as NULL; will hold value of matrix
  inv <- NULL
  
  ## define the set function to assign new
  set <- function(y) {
    
    ## value of matrix in parent environment
    x <<- y
    
    ## if there is a new matrix, reset inv to NULL
    inv <<- NULL
  }
  
  ## define the get fucntion
  get <- function() x
  
  ## assigns value of inv in parent
  setInverse <- function(inverse) inv <<- inverse
  
  ## gets the value of inv where called
  getInverse <- function() inv
  
  ## create storage for resulting objects
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse 
##has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv <- x$getInverse()
  
  ## if the inverse has already been calculated
  if (!is.null(inv)) {
    
    ## get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  
  ## sets the value of the inverse in the cache via the setinv function.
  x$setInverse(inv)
  inv
}
