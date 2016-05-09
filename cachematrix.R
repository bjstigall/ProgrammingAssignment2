## This will cache a matrix and the inversion of it to reduce processing time.

## This will cache the matrix given

makeCacheMatrix <- function(x = matrix()) 
  {

    invmat <- NULL
  
    set = function(y) 
    {
      x <<- y
      invmat <<-NULL
    }
  
    get = function() x
    
    setinv = function(i) 
    {
      invmat <<- i
    }
  
    getinv = function()
    {
      invmat
    }
  
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }


## This solves the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  inv
}


