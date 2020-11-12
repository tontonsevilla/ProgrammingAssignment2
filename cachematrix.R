## Put comments here that give an overall description of what your
## functions do
### makeCacheMatrix, will create a cached inverse matrix.
### cacheSolve, will try to inverse a given matrix but it will chekc first if it is already converted.
### if it is already converted it will retrieve it from the cache
### otherwise it convert the matrix into inverse matrix and cache it.

## Write a short comment describing this function
### Creates a special "matrix", which is really a list containing a function to 
### set/get the matrix and the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(mat) {
    x <<- mat
    invMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMat <<- inverse
  getinverse <- function() invMat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
### Create inverse of the special "matrix" created with the above function. 
### However, it first checks to see if the inverse matrix has already been created. 
### If so, it gets the inverse matrix from the cache and skips the conversion. 
### Otherwise, it converts the matrix of the data and sets the value of the inverse matrix 
### in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getinverse()
  if (!is.null(invMat)) {
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinverse(invMat)
  invMat
}

