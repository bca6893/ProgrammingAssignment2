# This function creates a 'special' matrix and caches its inverse
# The object msolve is created with null variable (i.e., a empty cache for the inverse)
# 'set' assigns variable x to passed on matrix values, and resets msolve
# 'get' returns the matrix, x
# 'setInv' caches the inverse when called (resets msolve to solve)
# 'getInv' returns the cached inverse when called

makeCacheMatrix <- function(x = matrix()) { 
  msolve <- NULL      
  set <- function(y) {     
    x <<- y
    msolve <<- NULL
  }
  get <- function() x     
  setInv <- function(solve) msolve <<- solve    
    getInv <- function() msolve        
    list(set = set, get = get, setInv = setInv, getInv = getInv) 
}

# This function assumes matrices are invertible and computes the inverse 
# To avoid unneeded computation, a condition is set, evaluating if the cache holds an inverse or NULL 
# If true, the inverse and message "getting cached data" are returned
# If false, x is retrieved, the inverse is computed, then cached, and returned

cacheSolve <- function(x, ...) {
  msolve <- x$getInv()    
  if(!is.null(msolve)) {     
    message("getting cached data")  
    return(msolve)                       
  }
  data <- x$get()  
  msolve <- solve(data, ...)  
    x$setInv(msolve)   
    return(msolve)   
}

## Sample Data
#  mvals <- c(3, 7.9, 3.7, 9)
#  mdata  <- matrix(mvals, nrow = 2, ncol = 2)
#  print(mdata)
#
#       [,1] [,2]
#  [1,]  3.0  3.7
#  [2,]  7.9  9.0

## Sample Output
#  Run 1:
#  matx <- makeCacheMatrix(mdata)
#  cacheSolve(matx)
#       [,1]      [,2]
#  [1,] -4.035874  1.659193
#  [2,]  3.542601 -1.345291
#
#  Run 2:
#  cacheSolve(matx)
#  getting cached data
#       [,1]      [,2]
#  [1,] -4.035874  1.659193
#  [2,]  3.542601 -1.345291