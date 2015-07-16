## Functions that cache the inverse of a matrix


## The first function, makeCacheMatrix creates a special 
## "matrix", which is a list containing a function to
##      -set the value of the matrix
##      -get the value of the matrix
##      -set the value of the inverse matrix
##      -get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) a <<- solve
        getinverse <- function() a
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function calculate the inverse of the special
## "matrix" created with the above function.

## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getinverse()
        if(!is.null(a)) {
                message("getting cached data")
                reture(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}
