## Pair of functions that cache the inverse of a matrix.

## Assumption(As per the assignment description): The matrix supplied is always invertible. Can test using det(<your matrix>). If it returns zero, 
## the matrix is not invertible.

## This function creates a special "matrix" object that can cache its inverse.
## makeVector creates a special "vector", which is really a list containing functions.


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## get the inverse of the matrix
  getinverse <- function() m
  
  ## Return the list containing all the above four functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  ## Try to get the inverse of the matrix from cache.
  m <- x$getinverse()
  
  ## If the inverse is already calculated..
  if(!is.null(m)) {
    
    message("getting cached data")
    
  ## Return the already catched inverse of the matrix.
    return(m)
  }
  ## If the inverse is not calculated already, get the matrix.
  data <- x$get()
  
  ## Use solve() function to get the inverse of the matrix.
  m <- solve(data)
  
  ## set the calculated inverse in the cache.
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'.
  m
}



