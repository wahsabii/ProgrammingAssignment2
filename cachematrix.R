# 1.makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse.
# 2.cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

# Purpose: to gain computation efficiencies by caching potential time-consuming
# computations such as inverting a matrix.

# Input:an assumed invertable matrix.
# Ouput: makeCacheMatrix creates a special "matrix", which is really a list 
# containing functions to: 
#         1.set the value of the matrix inverse
#         2.get the value of the matrix inverse
#         3.set the value of the matrix inverse
#         4.get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixx <- function(matrixx) m <<- matrixx
        getmatrixx <- function() m
        list(set = set, get = get,
             setmatrixx = setmatrixx,
             getmatrixx = getmatrixx)
}


# Input: Special matrix x.
# Output: m, the inverse special matrix x.
# The following function calculates the inverse of a matrix of the special 
# "matrix" created with the makeCacheMatrix function above. However, it first 
# checks to see if the inverse has already been calculated. If so, it gets the 
# inverse from the cache and skips the computation. Otherwise, it calculates
# the inverse of the data and sets the value of the inverse in the cache via 
# the setmatrixx function.
cacheSolve <- function(x, ...) {
        m <- x$getmatrixx()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrixx(m)
        m
}
