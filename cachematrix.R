##First we establish that the matrix is always invertible, then,
#"a" will be the inverse matrix and we set it to null (empty),
#We specify that, we consider "<< -" to access the variables, as main (parent we have: makeCacheMatrix, this allows us to use the variables in 2 levels.
#We invert the matrix.
#In this way the matrix is established and values are given, in the same way we do the inverse and calculate it to obtain its value.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     a <- NULL
     as <- function(y) {
         x <<- y
         a <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) a <<- inverse
     getinverse <- function() a
     list(as = as,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## #The "cacheSolve" function will calculate the inverse (after receiving it) and returns it if it is not cached, otherwise it calculates it, but takes the cache.

cacheSolve <- function(x, ...) {
     b <- x$getinverse()
     if (!is.null(b)) {
         message("getting cached data")
         return(b)
     }
     data <- x$get()
     b <- solve(data, ...)
     x$setinverse(b)
     b
}
