## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  revx <- NULL
  
  ## set function changes the matrix stored in the main function
  set <- function(y) {
    x <<- y
    revx <<- NULL
  }
 
  ## get function returns the matrix x stored in the main function
  get <- function() x
  
  ## store inverse matrix 
  setrevx <- function(solve = matrix()) revx <<- solve
  
  ## return inverse matrix
  getrevx <- function() revx
  
  ## object to store all above defined functions
  list(set = set, get = get, setrevx = setrevx, getrevx = getrevx)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  revx <- x$getrevx()
  
  ## verify if inverse matrix exists and if yes - retrieve it from cache 
  if(!is.null(revx)) {
    message("getting cached data")
    return(revx)
  }
  
  ## get matrix stored with makeCacheMatrix, get and store inverse
  data <- x$get()
  revx <- solve(data, ...)
  x$setrev(revx)
  revx
}
