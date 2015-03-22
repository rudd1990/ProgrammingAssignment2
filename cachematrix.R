# The two functions cache the inverse of a matrix

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# The following function checks whether the inverse of the matrix has already been computed.
# It gets the result if the inverse exists and otherwise it computes the inverse and sets
# the value in the cache through the setinv function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
     return(inv)
}
