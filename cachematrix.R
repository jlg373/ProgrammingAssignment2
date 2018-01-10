## Second programming assignment for R-Programming Coursera course.
## These functions cache the inverse of a matrix


## Returns "cachematrix" object, which is a list of functions to get/set the 
## matrix x and get/set its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Takes "cachematrix" object x and checks whether the matrix inverse is stored.
## If not, calculates the inverse and stores it.  Returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Getting cached data.")
                return(inv)
        }
        inv <- solve(x$get())  # Here we're assuming x exists and is invertible.
        x$setinv(inv)
        inv
}