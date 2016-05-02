## Put comments here that give an overall description of what your
## functions do

## this function makes the cache matrix, and declares variables to be used in other envirnments.  
## 

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
  
    m <<- y
    inv <<- NULL
  }
  ##this is simialr to the axample in the programming prompt
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## This function returns the inverse of (an invertable) matrix, but first checks to see if this inversion has already been calculated.  
##If not is does calculation, then stores it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          ## nverse of the original matrix in makeCacheMatrix()
  
  inv = x$getinv()
  
  # check if already calculated
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  # if not, calculate the inverse of the matrix and set as the inv
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
 
  x$setinv(inv)
  
  return(inv)
}
