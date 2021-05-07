## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The following functions are used to create a special object that stores 
##a matrix and caches its inverse. 


## A pair of functions that cache the inverse of a matrix


## The first function is to creates a special matrix object that can cache its inverse


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = maxtrix()) {
  # Initiate the inverse property
  i = NULL
  
  # Set a matrix 
    set <- function(m) {
      x <<- m
      i <<- NULL
    }
  
  # Get the matrix
    get <- function () {
      # Return a matrix 
      x
    }
  
  # Set inverse of the matrix
    setInverse <- function(Inverse) {
      i <<- Inverse 
    }
  
  # Get inverse of the matrix
    getInverse <- function() {
      # Return the inverse of a matrix
      i
    }
  # Return a list of function
    list (get = get, set = set, setInverse = setInverse, getInverse = getInverse)
    }

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function (x,...) {
  
  # Return a matrix that is inverse of 'x'
  m <- x$getInverse()
  
  # Return the inverse if already set from the cache
  if(!is.null(m)) {
    message("retrieve from the cache")
    return(m)
  }
  
  ## Computing the inverse of a square matrix can be done with the solve function
  ## in R. For example, if X is a square invertible matrix, then solve(X) 
  ## returns its inverse.
  
  # Get the matrix from the object 
  data<- x$get()
  
  # Calculate the inverse using the matrix manipulation
  m<- solve(data)%*%data
  
  # Set inverse of the object 
  x$setInverse(m)
  
  # Return the martrix
  m
  
}
