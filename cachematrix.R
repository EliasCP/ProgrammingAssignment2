## The function presented follows the example basically 
## replacing mean for solve and variables meaning different things

## The first function sets the functions to the actual calculation

makeCacheMatrix <- function(x = matrix()) {
  s <<- NULL
    ## First we create the "s" variable and empty it
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## I don't know why we did this since we never call that function 
  ## but the idea is clearing the cache once we move one to a different set
  
  getimtx <- function() s
  ## the getimtx function returns the content of the "s" variable from cache
  ## initially the value is NULL unless we have already set the value

  setimtx <- function(solve) s <<- solve
  ## after calculating the inverse of matrix, this function sets the variable "s" with the result
  ## next time cacheSolve is called, 
  ## it will be populated with a value and that will save computation time
  
  get <- function() x
  ## Returns the inverse of matrix
 
  list(get = get, set = set, setimtx = setimtx, getimtx = getimtx)
  ## lists the functions set in makeCacheMatrix functions to pass on to the next function
}


## Time to obtain inverse of matrix!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getimtx()
  ## gets the value of s from cache
  
  if(!is.null(s)){
    message("Getting Cached Data")
    return(s)
  }
  ## if s was calculated in the past, s will not be NULL, a message will display
  ## and the value os s from cache will be returned without the need to calculate again
  
  imtx <- x$get()
  ## if "s" is NULL then we need to calculate the inverse of matrix "x", so we get this value
  
  s <- solve(imtx, ...)
  ## use function solve to obtain inverse of matrix "x"
  
  x$setimtx(s)
  ## and set s with the value obtained
  
  s
  ## return the result "s" which is also now stored in cache
}

