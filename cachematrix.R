# makeCacheMatrix: function that creates a special "matrix" object that can cache its inverse.
# cacheSolve:  function that computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), then cachesolve should
# retrieve the inverse from the cache.
# Solve is the function used in R to calculate the inverse of a invertible matrix X.
# A matrix X is invertible if and only if its determinant is NOT zero.
# solve(X) returns the inverse of matrix X.



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL       # initialization of inv
                      # create the matrix in the working environment ( <<- ) 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
                      # get the value of the matrix
    get <- function() x
                      # invert the matrix and store in inv
    setInv <- function(inverse) inv <<- inverse
                       # get the inverted matrix from inv
    getInv <- function() inv
                       # return the created functions to the working env
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}





cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()             # try to get the matrix inverse stored in cache
                                  # if it exists in cache, it is returned
                                  # if not create the matrix in working env
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)               # return matrix
    }
    matrix <- x$get()             # creates matrix 
    inv <- solve(matrix, ...)     # return inverse of matrix
    x$setInv(inv)                 # store inverted matrix in cache
    return (inv)
}

