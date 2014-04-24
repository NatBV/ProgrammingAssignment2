##  - Programming Assignment 2 - Peer Assessment - 
## This work consists of two functions (makeCacheMatrix and CacheSolve)
## that enable the inverse of a matrix to be cached instead of being
## calculated every time it is required (which is specially useful when
## large matrices are used and/or when having to compute it repeatedly).
## [by Nathalie Vicente - Apr 2014]


## makeCacheMatrix creates a list of four functions (set, get, setinv,
## getinv) that act together like a special matrix, allowing the inverse
## of a matrix to be cached
makeCacheMatrix <- function(x = matrix()) {
    ## The cached inverse of x, which initially is set to NULL
    i <- NULL
    
    ## Function used to store/set a new matrix x
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    
    ## Function used to retrieve the stored matrix x
    get <- function() x
    
    ## Function used to store/set the inverse of x
    setinv <- function(inv) i <<- inv
    
    ## Function used to retrieve the stored inverse of x
    getinv <- function() i
    
    ## Return a list containing the four functions
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)  
}


## cacheSolve calculates/retrieves the cached value of the inverse of
## a matrix using makeCacheMatrix; it will calculate it for the first time
## and whenever a new matrix is set, otherwise it will retrieve the cached
## value
cacheSolve <- function(x, ...) {
    ## Retrieve the cached value of the inverse of x
    invm <- x$getinv()
    
    ## If it is the first time the inverse is required or a new matrix has
    ## been set (inv == NULL), the inverse will be computed
    if(is.null(invm))
    {
        message("MSG: Calculating data")
        data <- x$get()
        invm <- solve(data, ...)
        x$setinv(invm)
    }
    else
    {
        message("MSG: Getting cached data")
    }
    
    ## Return a matrix that is the inverse of x
    invm
}
