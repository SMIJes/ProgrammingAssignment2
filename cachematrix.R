## I have two main functions: makeCacheMatrix() and cacheSolve(),
## The makeCacheMatrix generally caches matrix and inverse and stores
## them indirectly in a list from which the can be aquired.
## The cacheSolve function tries to get the stored values from this
## list, and when it can't, it goes ahead to calculate the inverse by 
## itself.
## The entire aim of the functions is to calculate inverse of a given 
## matrix in a way that eliminates redundancy and hence saves time.






## THE MAKAECACHEMATRIX FUNCTION
## The parent function, makeCacheMatix(),creates and returns a list of
## it's child functions (closures): setMatrx(), getMatrx(), 
## setInvas() and getInvas().
## The parent parameters are x, the matrix; and invas, the inverse of 
## the matrix x.
## The closures gets and sets x and invas as their names implies.

makeCacheMatrix <- function(x = matrix()) {
  invas <- NULL
  setMatrx <- function(matrx) {
    x <<- matrx
    invas <<- NULL
  }
  getMatrx <- function() x
  setInvas <- function(i) invas <<- i
  getInvas <- function() invas
  list(setMatrx = setMatrx, getMatrx = getMatrx,
       setInvas = setInvas, getInvas = getInvas)
  
}





## THE CACHESOLVE FUNCTION
## The entire aim of this function is to return the inverse of cachedMatrx,
## that of newMatrix or just to return cachedInvas
## cachedMatrx and cachedInvas are matrix and inverse that have been stored 
## indirectly in the list x.
## The function first checks if cachedInvas exists, so to simply return it.
## Then if cachedInvas is NULL, the function checks if cachedMatrx exists 
## so as to use it to calculate newInvas, set the result as the current inverse
## and then return newInvas.
## Finally, if cachedMatrix is NULL, the function then takes newMatrix, 
## calculates its inverse, set the result as the current inverse and returns
## the result.

cacheSolve <- function(x, newMatrx) {
  
  cachedInvas <- x$getInvas()
  if(!is.null(cachedInvas)) {
    message("...getting cached inverse")
    return(cachedInvas)
  }
  
  cachedMatrx <- x$getMatrx()
  if(!is.null(cachedMatrx)){
    message("...getting cached matrix")
    newInvas<- solve(cachedMatrx)
    x$setInvas(newInvas)
    return(newInvas)
    
  }else {
    newInvas<- solve(newMatrx)
    x$setInvas(newInvas)
    return(newInvas)
  }
  
}