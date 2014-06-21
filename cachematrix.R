## Function creates a special matrix object that can cache its inverse

## makeCacheMatrix creates a list containing a function to set value of vector,
## get value of vector, set value of inverse matrix, and get value
## of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## set inv to NULL
    inv <- NULL
    
    ## function to set value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## function to get value of matrix
    get <- function() x
    
    ## function to solve for inverse of matrix
    setinverse <- function(solve) inv <<- solve
    
    ## function to get inverse of matrix
    getinverse <- function() inv
    
    ## make list of functions
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## cacheSolve: First checks to see if inverse has already been solved.
## If inverse has been solved, it retrieves inverse from cache and skips
## computation. If not, it solves for matrix inverse and stores it in cache
## via setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get inverse matrix, set to variable inv
    inv <- x$getinverse()
    
    ## if inv is not empty, return cached matrix and stop
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## else...
    
    ## retrieve matrix using get function, store in variable named data
    data <- x$get()
    
    ## solve for inverse of matrix, store in variable inv
    inv <- solve(data, ...)
    
    ## cache inv using setinverse function
    x$setinverse(inv)
    
    ## return inverse matrix and stop
    inv
}
