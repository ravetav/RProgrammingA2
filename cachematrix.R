## This program allows to catch the inverse of the matrix in cache rather than compute it repeatedly
## It's created because calculate the inverse of matrix is usually a costly computation

###################   makeCacheMatrix   #########################33

## The first function: "makeCacheMatrix" contains the following functions:
# 1.set()= set the value of the matrix
# 2.get()= get the value of the matrix
# 3.setinversa()= set the value of inverse of the matrix
# 4.getinversa()= get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y) {
        x <<- y
        inversa <<- NULL
    }
    get <- function() x
    setinversa <- function(inv) inversa <<- inv
    getinversa <- function() inversa
    list(set=set, get=get, setinversa=setinversa, getinversa=getinversa) 
}


###################   cacheSolve   #########################33

## The second function in this program is : cacheSolve()
# This function assumes that the matrix is always invertible.
# "cacheSolve" returns the inverse of the matrix, but first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# "setinversa" function.

cacheSolve <- function(x, ...) {
    inversa <- x$getinversa()
    if(!is.null(inversa)) {
        message("...getting cached data")
        return(inversa)
    }
    data <- x$get()
    inversa <- solve(data)
    x$setinversa(inversa)
    inversa     ## Return a matrix that is the inverse of 'x' 
}

