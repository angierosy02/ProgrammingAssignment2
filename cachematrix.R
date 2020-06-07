## Description: The following functions are use to cache the inverse of a matrix.

## `makeCacheMatrix` is a function that creates a list containing a function to: 1- set the value of a matrix,
## 2-get the value of the matrix, 3-set the value of the inverse matrix and 4-get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## `cacheSolve` is a function that checks if the inverse of a matrix has been calculated already 
## by the function above. If not, it will proceed to calculate the inverse of a matrix and sets the value
## of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
