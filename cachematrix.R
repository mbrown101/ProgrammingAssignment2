## @@ -1,8 +1,8 @@
## This file contains two functions intended to work togather in order to  invert 
## a matrix and store its inverted value.


## Function makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.


makeCacheMatrix <- function(x = matrix()) {     ## pass in a matrix
  inv <- NULL                                   ## assign the variable inv to null
  set <- function(y) {                          ## define the set function to assign values to x and inv in the parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                                          ## get value for the inverted matrix
  if(!is.null(inv)) {                                        ## check to see if a value exists
    message("getting cached data")
    return(inv)                                              ## if value, return the cached inv value
  }
  data <- x$get()                                            
  inv <- solve(data, ...)                                    ## if no value for inv cached, call solve() on the input matrix
  x$setinv(inv)                                              ## cache value of inverted matrix 
  inv                                                        ## reference variable inv to return the inverted matrix
  
}
