## These functions cache the inverse of a matrix so that later when that inverse
## is needed it can be pulled from the cache value rather than re-calculated.


makeCacheMatrix <- function(x = matrix()) {
      ## creates a matrix which is a function to get a matrix and its inverse
      
      s <- NULL
      set <- function(y) {  
            x <<- y
            s <<- NULL ## s is initially set to NULL
      }
      get <- function() x
      setinv <- function(solve) s <<- solve 
      getinv <- function() s  
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)  
      
}



cacheSolve <- function(x, ...) {
      ## Returns a matrix that is the inverse of 'x', either by computing it with
      ## solve() if does not exist, or getting it from the cache (created by
      ## makeCacheMatrix)
      s <- x$getinv()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- inv(data, ...)
      x$setinv(s)
      s
      
}
