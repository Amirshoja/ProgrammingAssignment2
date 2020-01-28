## Returning the inversion of a matrix by benefiting from caching
## The functions below sets and stores a matrix and finally caches its inverse

## This function creates a matrix and stores its inverse into the cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    
    x <<- y
    inverse <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(matinv) inverse <<- matinv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## The below function returns the inverse of the matrix created in the makeCacheMatrix function
## if the matrix inverse has already been computed it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    
    message("Getting Cached Data")
    return(inverse)
    
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
