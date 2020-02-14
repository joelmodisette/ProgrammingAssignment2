## Programmer: Joel D. Modisette for Coursera course R Programming

## These functions provide memoization of matrix inversion using R code.

## The first function makeCacheMatrix
## creates a list containing the following functions (by index)

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
    cacheValue <- NULL
    set <- function(y) {
      x <<- y
      cacheValue <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cacheValue <<- inverse
    getinverse <- function() cacheValue
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function cacheSolve
## returns the inverse of the matrix. First we check if the inverse
## has already been computed. If we have already computed and saved 
## that inverse, we retrieve the value. Otherwise we calculate the
## inverse and store that value for later use as well.

## We will assume the matrix in the argument is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cacheValue <- x$getinverse()
    
  #check if any result is cached
    
  if(!is.null(cacheValue)) {
      message("getting cached data")
      return(cacheValue)
  }
    
  data <- x$get()
   cacheValue <- solve(data, ...)
   x$setinverse(cacheValue)
   cacheValue
}
