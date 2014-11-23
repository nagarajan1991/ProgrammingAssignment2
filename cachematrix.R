## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getMtx <- function() return(x)  
  setMtx <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  getInvMtx <- function() return(inverse)
  setInvMtx <- function(y) inverse <<- y
  list(getMtx = getMtx,setMtx = setMtx,getInvMtx = getInvMtx,
       setInvMtx = setInvMtx)
}


cacheSolve <- function(x, ...) {  
  inverse <- x$getInvMtx()
  if(!is.null(inverse)){
    message("cached data exists")
    return(inverse)
  }
  
  m <- x$getMtx()        
  inverse <- solve(m,...)        
  x$setInvMtx(inverse)
  return(inverse)      
}
