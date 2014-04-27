## The below functions can be used for storing,calculating and caching a matrix and 
## its inverse. The first takes in a matrix and returns a cacheable object
## The second function can be used over this object for retrieving the inverse
## matrix from cache or by calculating (if not available in cache)


## makeCacheMatrix takes in a matrix and returns a cacheable list with set/get
## functions for caching the matrix and its inverserve.
##
## Argument:-  an inversible matrix
## Return Values:- Cacheable object, NULL on error

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## check if the supplied argument is a matrix or not
  ## not checking if its inversible
  if(!is.matrix(x))
  {
    message("Supplied argument is not a matrix, returning NULL")
    return (NULL)
  }
  
  set <- function(y) {

    ## check if matrix is supplied.
    if(!is.matrix(y))
    {
      message("Supplied argument is not a matrix, returning NULL")
      x <<- NULL
      inv <<- NULL
      return
    }
    
    ## update variables only if new matrix is different from original
    if(!identical(x,y))
    {
      x <<- y
      inv <<- NULL
    }
  }
  
  get <- function() x
  setinv <- function(invvar) inv <<- invvar
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function returns a matrix that is the inverse of 'x'
## First cache is checked if inverse is available, otherwise 
## inverse is calcuated and stored in cache for future references.
## 
## Argument: cacheable object returned from makeCacheMatrix function
## Return Values: inverse matrix, NULL on error.

cacheSolve <- function(x, ...) {
        
  
  ## check if the argument supplied is a cacheable matrix
  ## else return NULL
  
  if(!is.list(x) || !("getinv" %in% names(x)))
  {
    message("Supplied argument is not a cacheable matrix object")
    return (NULL)
  }
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
