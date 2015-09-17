## There are two functions in this file
## Both of these functions are used in sequence to compute compute the "Inverse Matrix" of
##      an "Invertible Matrix".
##      It's ASSUMED that the input-matrix is INVERTIBLE. 
##             These functions do not check if the input-matrix is Invertible or not!

## 1. makeCacheMatrix()
##      This function returns a list of three functions
##      a) setInverse() of a matrix
##      b) getInverse() of the matrix, which is computed using setInverse function
##      c) get()

## 2. cacheSolve()
##       This function is called after makeCacheMatrix() is already called.
##       It checks to see if inverse of the matrix is already computed, and cached. If it has been, 
##       then it returns the cached version of the inverse-matrix.
##       Otherwise, it computes the inverse-matrix and returns the resulting inverse-matrix.


## HOW TO USE THESE FUNCTIONS
##   First you have to call the function "makeCacheMatrix()",
##     and then you need to call "cacheMatrix()" function.
##      For example...
##      > myMatrix <- matrix(c(1,3,2,4),2,2) ## or any Invertible matrix
##      > specialMatrixObject <- makeCacheMatrix(myMatrix)
##      > myInverseMatrix <-cachSolve(specialMatrixObject)
##      > myMatrix
##      > myInverseMatrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##intialize inverseMatrix to NULL
  invMatrix <- NULL
  
  ##this brings back the value of x
  get <- function() x
  
  ##Here we use the cached version of Inverse Matrix, which was already computed.
  setInverse <- function(solve) invMatrix <<- solve
  
  ##This gets the Inverse of the Matrix
  getInverse <- function() invMatrix
  
  list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function sets the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## This gets the Inverse Matrix for Matrix 'x'
  invMatrix <- x$getInverse()
  
  ## Here we check to see if the returned Inverse_Matrix is NUll or not
  if(!is.null(invMatrix)) {
    
    message("Using cached copy of Inverse-Matrix")
    
    ## Since this is not NULL, 
    ##   that means that x$getInverse returned the cached version of Inverse Matrix, 
    ##   therefore we do not need to (again) calculate the Inverse Matrix.
    ##   We just return the cached Inverse-Matrix!
    return(invMatrix)
  }
  
  ## Here we use the get() function from the original call to makeCacheMatrix,
  ## so that we can actually pass it to solve function, which 
  ## will compute the Inverse-Matrix
  original_Matrix <- x$get()
  
  ## Compute the Inverse-Matrix (while using the original Matrix)
  invMatrix <- solve(original_Matrix, ...)
  
  ## For future calls, use this invMatrix to save 
  ##   the value of Inverse-Matrix (so that it is cached!)
  x$setInverse(invMatrix)
  ## Note: If we don't use this setInverse function, 
  ##  then there will no caching, and 
  ##  Inverse Matrix will have to be computed, every time!
  
  
  ## Return this Inverse-Matrix
  return(invMatrix)
}