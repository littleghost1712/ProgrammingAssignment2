## Coursera R Programming - rprog-032 - Assignment 2
## Modifier: Tran, Trong Tuan

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ##initiate local inv variable that store the inversed matrix
    set <- function (y) { 
            ##set function
        x<<- y
        inv <-- NULL
    }
    get <- function() x ## get function to retrieve the matrix from the output list
    setInverse <- function (inverse) inv <<- inverse ## function to store the 
            ## inverse matrix into local variable inv
    getInverse <- function () inv ##function to retrieve the cached inversed matrix
    
    ##output the special "vector" which are the 4 functions above
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', where x is the special 
        ## vector created by the makeCacheMatrix funciton above
    inv <- x$getInverse() ## get cached inverse matrix if available
    if(!is.null(inv)){
        ## if the inversed matrix already calculated & stored before, just call
        ## it out instead of calculating it again
        message("getting cached data")
        return (inv)
    }
    
    ## if the inverse matrix has not been stored (cached) yet, just calculate it
    ## from the matrix
    data <- x$get() ## retrieve the matrix from the vector list via get funciton
    inv <- solve(data, ...) ## calculate the matrix reverse
    x$setInverse(inv) ## store the matrix reverse back into vector x
    inv ## return the inverse matrix to output
}
