# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly.  These functions cache a matrix, 
# compute the inverse of a matrix (or return an error if non-invertible),
# and can return the original matrix and inverted matrix (if set).

## if a matrix is passed to makeCacheMatrix it is set as x, otherwise 
# a 1x1 matrix of NA is set as the default.

## setMatrix(matrix) will replace whatever value is set as x and clear the invMatrix variable

## getMatrix() returns the value of x

## setInvMat(inverted_matrix) sets the value passed to it to the invMatrix variable

## getInvMat() returns the value of invMatrix

makeCacheMatrix <- function(x = matrix()) {
  #from makeVector.R Example
  invMatrix <- NULL
  
  #allows the assignment of a matrix if makeCacheMatrix is initialized with default value,
  #or assigns a new matrix to makeCacheMatrix and resets the invMatrix value if a change is 
  #necessary.
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    invMatrix <<- NULL
  }
  
  # I am explicitly calling return in these sub functions only because not 
  # doing so is foreign to me.  I understand that this goes against the course
  # instruction, but I will do it anyway.
  
  #returns the value of the matrix that is assigned to the function
  getMatrix <- function() {
    return(x)
  }
  
  #cached assignment of an inverted matrix.  **This should only be used with cacheSolve()
  #as a false assignment can be made.
  setInvMat <- function(invertedMat) {
    invMatrix <<- invertedMat
  }
  
  #returns the cached inverse matrix.
  getInvMat <- function() {
    return(invMatrix)
  }
  
  #makeCacheMatrix returns the following 'List of 4' sub functions.
  return(list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMat = setInvMat,
       getInvMat = getInvMat))
}


## cacheSolve(makeCacheMatrix) takes the 'List of 4' returned by makeCacheMatrix, 
# and if invMatrix is set returns the value, if it is not set then it is calculated, 
# cached and returned

cacheSolve <- function(cacheMatrix, ...) {
  #from cachemean_makeVector.R example
  
  #takes the 'List of 4' from makeCacheMatrix() and checks to see if the Inverse
  #Matrix has already been set.
  invMatrix <- cacheMatrix$getInvMat()
  if(!is.null(invMatrix)) {
    message("Getting cached data.")
    return(invMatrix)
  }
  
  #if Inverse Matrix has not been set, then it is calculated and set.
  #using x for the original matrix to maintian consistancy with the makeCacheMatrix func.
  x <- cacheMatrix$getMatrix()
  invMat <- solve(x, ...)
  cacheMatrix$setInvMat(invMat)
  return(invMat)
}

# Usage:
# matrix <- rbind(c(1, 1, 1, 1), c(1, 2, 1, 2), c(1, 1, 1, 0), c(1, 4, 2, 3))
# cm <- makeCacheMatrix(matrix)
# cm
## $setMatrix
## function (newMatrix) 
## {
##   x <<- newMatrix
##   invMatrix <<- NULL
## }
## <environment: 0x5f434f8>
##   
##   $getMatrix
## function () 
## {
##   return(x)
## }
## <environment: 0x5f434f8>
##   
##   $setInvMat
## function (invertedMat) 
## {
##   invMat <<- invertedMat
## }
## <environment: 0x5f434f8>
##   
##   $getInvMat
## function () 
## {
##   return(invMatrix)
## }
## <environment: 0x5f434f8>
# cacheSolve(cm)
##      [,1] [,2] [,3] [,4]
## [1,]   -1    2    1   -1
## [2,]   -2    1    1    0
## [3,]    3   -3   -1    1
## [4,]    1    0   -1    0
# cm$getMatrix()
##      [,1] [,2] [,3] [,4]
## [1,]    1    1    1    1
## [2,]    1    2    1    2
## [3,]    1    1    1    0
## [4,]    1    4    2    3

## *** Check that it is properly inverted ***
# cm$getMatrix() %*% cm$getInvMat()
##      [,1] [,2]          [,3] [,4]
## [1,]    1    0 -2.220446e-16    0
## [2,]    0    1 -2.220446e-16    0
## [3,]    0    0  1.000000e+00    0
## [4,]    0    0 -4.440892e-16    1
## *** Here the correct result is an identity matrix *** (or the very close approx.)

## Sources:
# https://answers.yahoo.com/question/index?qid=20120114080640AAoM43Q
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/matmult.html
# https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions