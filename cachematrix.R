## Program to calculate the inverse of a given matrix with respect to the solution that might be stored in the cache;
## since the calculation of the inverse of a matrix can be very laborious, this assembly of makeCacheMatrix() and cacheSolve() first
## checks whether the inverse of the input matrix is already stored in the cache and if so, gives this matrix back and does not calculate it again

## makeCacheMatrix creates an R object that harbors a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #takes the argument y (assumes a matrix)
    x <<- y #assigns the input argument as x to the parental enviro
    inv <<- NULL #assigns NULL to inv in the parental enviro
  } #previous line clears any value of inv which might be cached from earlier exec of cacheSolve()
  get <- function() x #get = getter, assigns x from enviro to get
  setinv <- function(solve) inv <<- solve #setter for inv
  getinv <- function() inv #getter for inv
  list(set = set, get = get, #assigns all of above operations in list fromat
       setinv = setinv, #and gives it to the parental enviro as
       getinv = getinv) #makeCacheMatrix object
}


## cacheSolve returns arguments from makeCacheMatrix to retrieve an inverse Matrix stored in the cache 

cacheSolve <- function(x, ...) { #makeCacheMatrix() alone is incomplete, because cacheSolve is necessary to populate or recover the inverse Matrix)
  inv <- x$getinv() #tries to get mean from an object passed as argument into the function
  if(!is.null(inv)) { #checks if its value is not NULL
    message("getting cached data") #if it is not NULL, it gives back the cached matrix
    return(inv)
  }
  data <- x$get() 
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
