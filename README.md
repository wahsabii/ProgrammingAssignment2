### Credits

This R function modifies Dr. Roger D. Pheg's fuctions from GitHub that I forked
into my GitHub repository.

Source: https://github.com/rdpeng/ProgrammingAssignment2

### Purpose

This R function is able to cache potentially time-consuming computations.
For this example, taking the inverse of a matrix. However, for a larger
matrices, it may take too long to compute the matrix inverse, especially if 
it has to be computed repeatedly (e.g. in a loop). If the contents of a 
matrix are not changing, it may make sense to cache the value of the inverse 
so that when we need it again, it can be looked up in the cache rather than
recomputed. tHE function takes advantage of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

### Caching the inverse of a matrix

The `<<-` operator which can be used to assign a value to an object in an 
environment that is different from the current environment. Below are two 
functions that are used to create a special object that stores a numeric 
matrix and caches its inverse.

The first function, `makeCacheMatrix` creates a special "matrix", which is
really a list containing a function to

1.  set(), set the value of the matrix
2.  get(), get the value of the matrix
3.  setmatrixx, the value of the matrix inverse
4.  getmatrixx, get the value of the matrix inverse

<!-- -->

 makeCacheMatrix <- function(x = matrix()) {
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

The following function calculates the inverse of the special "matrix"
created with the above function. However, it first checks to see if the
inverse has already been calculated. If so, it `get`s the matrix from the
cache and skips the computation. Otherwise, it calculates the inverse of
the data and sets the value of the inverse in the cache via the `setmatrixx`
function.

cacheSolve <- function(x, ...) {
        m <- x$getmatrixx()
        if(!is.null(m)) {
                message("Getting cached data ")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                 
        x$setmatrixx(m)
        # Output is the matrix inverse
        m
}


