# Programmer: Wahsabii Neanderthal, wahsabii@gmail.com
#
# Purpose: To gain computational efficiency by caching potential time-consuming
# computations such as inverting a matrix in this case.
#
# Source: Modified Dr. Roger D. Peng, John Hopkins University, functions at
# https://github.com/rdpeng/ProgrammingAssignment2
#
# Two primary functions:
#
# 1.makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse outside the scope of the calling function using "<<-"
# by defining functions for use in the calling environment.
#
# 2.cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
#
#
makeCacheMatrix <- function(x = matrix()) {
        # Input:an assumed invertable matrix.
        # Ouput: makeCacheMatrix creates a special "matrix", which is really a 
        # a list x containing four functions to: 
        #         1.set(); sets the value of the matrix 
        #         2.get(); gets the value of the matrix 
        #         3.setmatrixx(); sets the value of the matrix inverse
        #         4.getmatrixx(); gets the value of the matrix inverse
        m <- NULL # initialize a null matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixx <- function(matrixx) m <<- matrixx  
        getmatrixx <- function() m
        # Output is x a list of functions where some use the "<<-" 
        # operator.
        list(set = set, get = get,
             setmatrixx = setmatrixx,
             getmatrixx = getmatrixx)
}
#
#
cacheSolve <- function(x, ...) {
        # Input: Special matrix x.
        # Output: m, the inverse special matrix x.
        # The following function calculates the inverse of a matrix of the 
        # special "matrix" created with the makeCacheMatrix function above. 
        # However, it first checks to see if the inverse has already been 
        # calculated. If so, it gets the inverse from the cache and skips the 
        # computation. Otherwise, it calculates the inverse of the data and 
        # sets the value of the inverse in the cache via the setmatrixx 
        # function.
        m <- x$getmatrixx()
        if(!is.null(m)) {
                message("Getting cached data ")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                 # compute the matrix inverse
        x$setmatrixx(m)
        # Output is the matrix inverse
        m
}
