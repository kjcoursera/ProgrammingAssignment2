## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        nv <- NULL   # setting as a place holder 
        
        # sets the matrix x, to a new matrix y and resets inverse, inv to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x # returns the stored matrix x
        
        setinverse <- function(inverse) inv <<- inverse # sets the inverse, inv to "inverse"
        getinverse <- function() inv # returns the inverse, inv
        
        # returns the special vector containing all the functions just defined
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## getting the cached inverse
        inv <- x$getinverse()
        
        # if a chached inverse exists, return the inv, inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # otherwise get the matrix, calculate the inverse and store it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        # returns the inverse
        inv
}
