## Provides a matrix class that caches inverses

## class cacheMatrix
##   set(x) sets the base matrix
##     x(matrix) the base matrix
##
##   get() returns the base matrix
##     returns (matrix) the base matrix
##
##   setinverse(x) sets the cached inverse
##     x(matrix) value of the inverse
##
##   getinverse() gets the cached inverse
##     returns(matrix) value of the cached inverse, NULL if not yet calculated
##

## Create a cachable matrix class from a normal matrix
## parameters:
##   x (matrix): a normal matrix as the basis for the cachable matrix
##   returns (cacheMatrix)   : a "cache matrix" class as a list
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Get the inverse (possibly cached) of a cachable matrix
## parameters:
##   x (cacheMatrix): the cache matrix to invert
##   ...             : (optional) parameters for the solve() function
#    returns (matrix): the inverse of the cache matrix x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Tests of functionality
test <- function() {
        # 1,1 identity
        m1 <- matrix(1, nrow = 1, ncol = 1)
        cm1 <- makeCacheMatrix(m1)
        if (!is.null(cm1$getinverse())) {
                stop('test 1a failed')
        }
        inverse1a = cacheSolve(cm1)
        print(inverse1a)
        if (is.null(cm1$getinverse()) || inverse1a[1,1] != 1) {
                stop('test1b failed')
        }
        inverse1b = cacheSolve(cm1)
        print(inverse1b)
        if (cm1$getinverse()[1,1] != 1 || inverse1b[1,1] != 1) {
                stop('test1c failed')
        }

        # 2,2 identity
        m2 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
        cm2 <- makeCacheMatrix(m2)
        if (!is.null(cm2$getinverse())) {
                stop('test 2a failed')
        }
        inverse2a <- cacheSolve(cm2)
        print(inverse2a)
        if (is.null(cm2$getinverse()) || inverse2a[2,2] != 1) {
                stop('test2b failed')
        }
        inverse2b <- cacheSolve(cm2)
        print(inverse2b)
        if (cm2$getinverse()[2,2] != 1 || inverse2b[2,2] != 1) {
                stop('test2c failed')
        }

        # 3,3 non-identity
        m3 <- matrix(c(1, 0, 1, 0, 2, 0, 0, 0, 4), nrow = 3, ncol = 3)
        cm3 <- makeCacheMatrix(m3)
        if (!is.null(cm3$getinverse())) {
                stop('test 3a failed')
        }
        inverse3a <- cacheSolve(cm3)
        print(inverse3a)
        if (is.null(cm3$getinverse()) || inverse3a[3,1] != -0.25) {
                stop('test3b failed')
        }
        inverse3b = cacheSolve(cm3)
        print(inverse3b)
        if (cm3$getinverse()[3,1] != -0.25 || inverse3b[3,1] != -0.25) {
                stop('test2c failed')
        }
}
