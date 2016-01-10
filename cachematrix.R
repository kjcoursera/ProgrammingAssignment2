## To get Inverse of the Matrix
## Matrix inversion is usually a costly computation. Caching the inverse of a matrix will add some benefit instead 
## off computing it repeatedly. Below are the pair of functions that cache the inverse of a matrix.


## makeCacheMatrix is a function to create & store a special matrix object that can cache its inverse
## It's  a list containing the functions to set and get the value of the matrix and its inverse

## Contains the following sub functions:
## setinverse  - set the value of the matrix
## getinverse  - get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   # setting as a place holder 
        
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


## CacheSolve is a function to compute the inverse of the special "matrix" returned from the above function
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache, i.e skipping the computation. Otherwise
## it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x' / getting the cached inverse
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
