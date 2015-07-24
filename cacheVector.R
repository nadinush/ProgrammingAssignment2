makeVector <- function(x = numeric()) {
    m <- NULL
    
    ## set the value of the vector
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the value of the vector
    get <- function() x
    
    ## set the value of the mean
    setmean <- function(mean) m <<- mean
    
    ## get the value of the mean
    getmean <- function() m
    
    ## create a special object that stores a numeric vector and cache's its mean.
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## calculates the mean of the special "vector" created 
## with the above function
cachemean <- function(x, ...) {
    m <- x$getmean()
    
    ## it first checks to see if the mean has 
    ## already been calculated
    if(!is.null(m)) {
      
          ## it gets the mean from the cache 
          ## and skips the computation
          message("getting cached data")
          return(m)
    }
    
    ## it calculates the mean of the data and  
    ## sets the value of the mean in the cache 
    ## via the setmean function
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

x <- makeVector(c(1:20))
x$getmean()
x$get()
cachemean(x)
x$getmean()

    