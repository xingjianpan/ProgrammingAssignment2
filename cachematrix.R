## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # set inverse to NULL 
    i <- NULL
    # setter function for matrix values
    set <- function(y) {
        # set the value of matrix as y
        x <<- y
        # since the values of matrix get udpated, 
        # the mean will change, so set it as NULL
        i <<- NULL
    }
    # getter function
    # return the values of matrix
    get <- function() x
    
    # setter function for matrix inverse
    setinverse <- function(inverse) i <<- inverse
    
    # getter function for matrix inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates 
## the inverse of the special "matrix" created with 
## the above function. However, it first checks
## to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
