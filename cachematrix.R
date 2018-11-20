## Put comments here that give an overall description of what your
## functions do

## The first function, makeVector creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## Write a short comment describing this function

## Assignment: Caching the Inverse of a Matrixless

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

makeCacheMatrix <- function(x = matrix()) {            ## provided function
        inv_m <- NULL                                  ## initialize the variable
        set <- function(y) {
                x <<- y                                ## assign the variables in different environment
                inv_m <<- NULL
        }
        get <- function() x                            ## get / return the matrix (Neo :)
        setinv_m <- function(invert) inv_m <<- invert  ## set the inverse in different environment
        getinv_m <- function() inv_m                   ## get inverse
        list(set = set, get = get,                     ## create list for function
             setinv_m = setinv_m,
             getinv_m = getinv_m)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv_m <- x$getinv_m()     ## get the value from the list
        if(!is.null(inv_m)) {     ## if the inverse matrix is not null then use it
                message("using cached data")
                return(inv_m)
        }
        ## Or calculate it
        ## Use the solve function in R to Compute the inverse of a square matrix
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinv_m(inv_m)         ## cache to the list
        inv_m                     ## return the value
}
