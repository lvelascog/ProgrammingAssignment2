## We'll solve the matrix and cache it's inverse
## Whenever we ask for the inverse of a matrix we'll check if it's cached
## and skip the calculation if it is

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) i <<- solve
    getmatrix <- function() {
        if(is.null(i)) {
            i <- solve(x)
        }
        i
    }
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'c
    i <- x$getmatrix()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matriz <- x$get()
    i <- solve(matriz, ...)
    x$setmatrix(i)
    i
}
