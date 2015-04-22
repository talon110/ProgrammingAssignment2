## These functions are designed to reduce processing time on big matrices.
## makeCacheMatrix is called first and builds a list that contains functions 
## to set and retrieve the matrix and its inverse. cacheSolve is called 
## next and determines if a cached solution already exists. If not, it 
## calculates the solution and stores it in the cache.

## makeCacheMatrix creates a list containing functions to set the value 
## of the matrix, get the value of the matrix, set the value of the matrix's 
## inverse, and get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is a special version of solve which first checks to see if 
## a cached version of the inverse exists. If so, returns a message that
## it's getting cached data and returns the cached data. If not, calculates
## the inverse using solve, and stores that inverse in the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
