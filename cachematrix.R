##makeCacheMatrix <- function(x = matrix())
# stores the cached value of inverted matrix and 
# named list of functions generated to be called later
# initialize to NULL

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  cache <- NULL
  # create the matrix in the working environment
  y <- x
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  #print("get() Getting the value of the matrix")
  get <- function() x
  #print("setInverse() invert the matrix and store in cache")
  setinverse <- function(inverse) cache <<- inverse
  #print("getInverse() get the inverted matrix from cache")
  getinverse <- function() cache
  #print("return the created functions to the working environment")
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve <- function(x,...){}

## Tries to return the inverse of a matrix that has been stored in 
## the outer cache.  
## 1. If the inverted matrix does exist in the cache:
##    The inverted matrix will be created in the current working environment 
##    and stored in the outer cache
## 2. If the inverted matrix exists in the cache: 
##    We will call get() to access the inverted matrix
#
## Errorhandling to catch error where the matrix cannot be inverted

cacheSolve <- function(x=matrix()) {
  #print("attempt to get/retrieve the inverse of the matrix stored in the cache")
  cache <- x$getinverse()
  # return inverted matrix from cache if it exists
  # else create the matrix in the working environment
  if (!is.null(cache)) {
    message("retrieving cached data")
    # display the matrix to the console
    return(cache)
  }
  # create matrix since it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and invertible
  # if not, handle exception cleanly
  tryCatch( {
    # set and return inverse of matrix
    cache <- solve(matrix)
  },
  error = function(e) {
    message("Error:")
    message(e)
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
  return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$setinverse(cache)
    cache
  } )
  # display matrix in console
  cache
  }

## Testing Results: 
# --- Result: Singular ----
#A <- matrix(1:100,10,10)
#A1 <- makeCacheMatrix(A)
#cacheSolve(A1)
#---- Result: Works OK ------
#B <- matrix(c(1,2,3,4),2,2)
#B1 <- makeCacheMatrix(B)
#cacheSolve(B1)
#------Result: Singular -------
#T <- makeCacheMatrix(x = matrix(1:100,10,10))
#cacheSolve(T)
#---- Result: Singular ------
#C <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)
#C1 <- makeCacheMatrix(C)
#cacheSolve(C1)
