## Caching the inverse of a matrix rather than compute it repeatedly has advantages for big data sets. 
## Below is a pair of functions that cache the inverse of a matrix. 

## Function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function that computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## if the inverse has already been calculated
    ## return cached inverse
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise calculate inverse with solve(data)
  ## and set it as inverse
  data <- x$get()
  idata <- solve(data)
  x$setinverse(idata)
  idata
}