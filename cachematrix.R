## There are two functions in this file - makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a matrix and caches it's inverse
## cacheSolve returns the inverse of the created matrix

## makeCacheMatrix takes a matrix as a parameter which is defaulted to NULL. It creates a matrix and
## caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	  i<-NULL
      	  setMatrixValue<-function(y=matrix()) {
          x<<-y
          inverse<<-NULL
      }
      getMatrixValue<-function() x
      setInverse <- function(inverse) i<<-inverse
      getInverse <-function() i
      list(setMatrixValue=setMatrixValue,
           getMatrixValue=getMatrixValue,
           setInverse=setInverse,
           getInverse=getInverse) 
}


## cacheSolve returns the inverse of the created matrix. In case, the inverse is cached, it prints
## the message "Getting cached inverse" and prints the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inverse<-x$getInverse()
        if(!is.null(inverse)) {
            message("Getting cached inverse")
            return(inverse)
      }
      matrixData<-x$getMatrixValue()
      inverse<-solve(matrixData)
      x$setInverse(inverse)
      inverse 
}
