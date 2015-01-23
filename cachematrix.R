## Put comments here that give an overall description of what your
## functions do

  ## The objective of these functions is to set a square matrix (x) and 
  ## calculate it's inverse. To save on computation time and memory, 
  ## the inverse calculated is stored in the cache (inv). 
  ## If the inverse has already been computed for a matrix, 
  ## then the data is retrieved from the cache.
 .


## Write a short comment describing this function

  ## makeCacheMatrix sets a special matrix returning a list.  
  ## The function sets the value and outputs the matrix through 'get'. 
  ## It also sets the inverse (setinverse) and outputs (getinverse) it.
  ## The function uses the special operator <<- , which sets the value of
  ## inverse 'inv' in a different environment than the current one. 

 makeCacheMatrix <- function(x = matrix()) {
   
      inv <- NULL   ## Inverse 'inv' is initially set to NULL
  
      set <- function(y = matrix()) {
            x <<- y
              inv <<- NULL ## Inverse 'inv' is set to NULL 
        }                  ## when a new matrix is set
    
      get <- function()x
      setinverse <- function(solve) inv <<- solve(x)
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse  = setinverse,
           getinverse  = getinverse)
  
}



## Write a short comment describing this function

  ## The function computes the inverse of the special matrix set above
  ## using solve and caches it. It checks if the inverse has already
  ## been calculated and if yes, it retrieves it from the cache and displays
  ## "getting cached data".

 cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      inv <- x$getinverse()
      if(!is.null(inv)) {
          message("getting cached data")  ## If the inverse is not NULL  
                                          ## it will return the cached inverse
          return(inv)
        }
  
      data <- x$get()
      inv    <- solve(data)             ## Else inverse is computed
      x$setinverse(inv)
      inv
 }

