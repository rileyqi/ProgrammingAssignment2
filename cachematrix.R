## Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly 
##The following pair of functions that cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


##The function below first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
x = rbind(c(1, 2), c(2, 1))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
