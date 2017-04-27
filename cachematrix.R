## This script contains two functions.
## One caches the inverse of a matrix and the other creates the inverse.


## This function checks for the cached inverse of matrix and if it doesn't exist it creates it.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  getMatrix <- function() x                                   
  setInverse <- function(inverse) i <<- inverse  
  getInverse <- function() i                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## This function creates the inverse of the matrix passed from makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  
  if(!is.null(i)) {                       
    message("Getting the Cached Matrix...")    
    return(i)                             
  }
  
  MatrixData <- x$getMatrix()                      
  i <- solve(MatrixData, ...)             
  x$setInverse(i)                         
  return(i)                               
  
}