# Coursera - R Programming - week3 programming assignment
# Tania Savitri
# 2018/10/31

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x # Store value of x in 'get'
   setinverse <- function(solve) m <<- solve # Calculate inverse of x and store in m
   getinverse <- function() m # Call for m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get() # Calling value of x stored in 'get' in the makeCacheMatrix function, and store in 'data'
   m <- solve(data, ...) # Calculate inverse of 'data'
   x$setinverse(m) # Store value of m in 'setinverse'
   m
}