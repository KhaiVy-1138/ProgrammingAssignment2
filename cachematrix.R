## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The following functions are used to create a special object that stores 
##a matrix and caches its inverse. 


## A pair of functions that cache the inverse of a matrix


## The first function is to creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(m = matrix()) {
    #   Initiate inverse property
    i <- NULL
    
    ## Method to set the matrix
    set <- function(y) {
        m <<- y
        i <<- NULL
        }
    ## Method to get the matrix
    get <- function() {
        #Return a matrix
        m
        }
    ## Method to set inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
        }
    
    ## Method to get inverse of the matrix
    getinverse <- function() {
    
    #   Return the inverse property    
    i        
    }
    
    
    ## Return the list of the method
    list(set = set,get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    ## Just return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setinverse(m)
    
    ## Return the matrix
    m
}