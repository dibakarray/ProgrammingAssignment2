##  This function creates a special "matrix" object that can cache its inverse.
## 

## this function invese a matrix and creates a special "matrix", which is really   a list containing a function to
##set the value of the matrx
##return (get) the value of the matrx
##set the value of the inverse of the matrix
##get the value of the inverse
T##he function assumes matrix passed as paramaeter is inversible, it does not validate inverseability

makeCacheMatrix <- function(x = matrix()) {
  ##begins by setting the the variable to NULL as a placeholder for a future value
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x ##returns the the matrix, x
  setinverse<- function(inverse) inv_mat <<-inverse ##sets the inverse to inv_mat
  getinverse <- function() inv_mat ## returns the inverse inv_mat
  ## returns the 'special vector' containing all of the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##  this function calculates the inverse of of the special "matr" created with the makeCacheMatrix function. 
##  However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the inverse of the matrix in the
## cache using  the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
## Return a matrix that is the inverse of 'x'

