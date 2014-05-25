## this function create a matrix that contains four functions
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix's inverse
## get the value of the matrix's inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## this function calcules the inverse of the matrix if it's inversible
## else it display "the matrix isn't inversible !"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if(det(data, ...)==0){
    message ("the matrix isn't inversible !")
    x$setinv (NaN)
  }
  else { 
    inv <- solve(data, ...)
    x$setinv(inv)
  }
  inv
}
