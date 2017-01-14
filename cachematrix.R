## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
## set: set the value of the matrix, 
## get: get the value of the matrix, 
## setinv: set the value of the inverse matrix, 
## getinv: get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
      
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinv <- function(matrixinv) m <<- matrixinv
                getinv <- function() i
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## cacheSolve intially checks if inverse of the special "matrix" is already in cache and returns it,
## if not it will compute the inverse, caches in makeCacheMatrix and returns the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getinv()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinv(i)
                i
}
        