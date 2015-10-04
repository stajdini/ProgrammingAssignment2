# makeCacheMatrix creates a list containing a function to
#  sets the value of the matrix
#  gets the value of the matrix
#  sets the value of inverse of the matrix
#  gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# this fucntion calculates and returns the inverse of a given matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
        
}
