## Matrix inversion is usually a costly computation and there may be some 
## benefitt to caching the inverse of a matrix rather than compute it repeatedly
## so, here there are a pair of functions that cache the inverse of a matrix.


## This first function, makeCacheMatrix, creates a special matrix object and 
## caches its inverse. It's really a list containing a funtion to: set the value
## of the matrix; get the value of the matrix; set the value of the inverse; and
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## The second function, cacheSolve, computes the inverse of the special matrix
## created with the first function (makeCacheMatrix), but it first checks if the
## inverse was already calculated. If so, it gets the inverse from the cache and
## skips its computation. Otherwise it will compute the inverse of the matrix 
## and set its value in the cache.

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
}



### testing the functions
mat <- matrix(rnorm(1:9), 3, 3)
#           [,1]       [,2]       [,3]
#[1,] -0.9240807  1.0550903  0.8961220
#[2,]  0.2791152  0.6103117 -0.1792483
#[3,] -1.5162495 -0.3747707  0.7392000

solve(mat)
#          [,1]      [,2]       [,3]
#[1,] 0.8536851 -2.480719 -1.6364590
#[2,] 0.1455468  1.502230  0.1878307
#[3,] 1.8248732 -4.326836 -1.9086672


cache_mat <- makeCacheMatrix(mat)

cache_mat$get()
#           [,1]       [,2]       [,3]
#[1,] -0.9240807  1.0550903  0.8961220
#[2,]  0.2791152  0.6103117 -0.1792483
#[3,] -1.5162495 -0.3747707  0.7392000


cacheSolve(cache_mat)
#          [,1]      [,2]       [,3]
#[1,] 0.8536851 -2.480719 -1.6364590
#[2,] 0.1455468  1.502230  0.1878307
#[3,] 1.8248732 -4.326836 -1.9086672

