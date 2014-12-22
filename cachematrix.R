## These functions will find the inverse of a matrix x.
## Simply start by calling makeCacheMatrix(x) first, before you call cacheSolve(x).
## To save computing time, the inverse will be returned without having to be
## recalculated if it was already found before.

## The function makeCacheMatrix creates an R object by: 
## - Initialising a variable 'm' which will be used to save an inverse matrix later
## - Providing function get() to obtain "raw" matrix
## - Providing function setInvMatrix() to assign inverse matrix to m
## - Providing function getInvMatrix() to obtain the cached inverse matrix

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  get <- function() x
  setInvMatrix <- function(InvMatrix) m <<- InvMatrix
  getInvMatrix <- function() m
  list(get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}

## The function cacheSolve inverts matrix x by:
## - Checking if the inverse has been found
## - If yes, return the result and quit.
## - If no, the inverse of x is calculated, saved to cache and returned.

cacheSolve <- function(x) {
  m <- x$getInvMatrix()
  if(!is.null(m)){
    message("Cached data found.")
    return(m)
  }
  else {
    message("Calculating inverse matrix...")
    data <- x$get() # get matrix from object x
    m <- solve(data) # find inverse matrix
    x$setInvMatrix(m) # assign inverse matrix to object x
    message("Done.")
    return(m)
  }
}
