## Programming Assignment 2

## creates a "matrix" object that caches its inverse computation.
makeCacheMatrix <- function ( x = matrix() )
{
  inv <- NULL
  
  set <- function(x)
  {
    mat <<- x;
    inv <<- NULL;
  }
  
  get <- function()
    return(mat);
  
  setinv <- function(inv1)
    inv <<- inv1;
  
  getinv <- function()
    return(inv);
  
  return ( list ( set = set, 
                  get = get, 
                  setinv = setinv,
                  getinv = getinv))
}

## computes the inverse of the "matrix" returned by %makeCacheMatrix%. 
## If the inverse is cached then %cacheSolve% returns the cached inverse
cacheSolve <- function ( x, ... )
{
  ## Return a matrix that is the inverse of 'x'
  inv <- mat$getinv()
  if( !is.null(inv) )
  {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  return(inv)  
}
