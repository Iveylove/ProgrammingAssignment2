## A pair of functions that cache the inverse of a matrix


## Creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" created by ## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!its.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$gat()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv  ## Return a matrix that is the inverse of 'x'
}
