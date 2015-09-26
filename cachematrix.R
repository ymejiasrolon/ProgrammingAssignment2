makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set_inverse(inv)
        inv
}