## I want to save time when calculating the inverse of a matrix.
## If it is the first time that the inverse of a matrix is calculated, then
## the calculation process is triggered,  and the value saved in the cache.
## If the matrix has not changed, the next time that it is required to calculate
## the inverse of that matrix, instead of triggering the calculation process, the
## value is taken from the cache, wich speeds up the calculation process.

## This first functions does four things:
## 1.	set the value of the matrix 2.	get the value of the matrix 
## 3.	set the value of the inverse matrix 4.	get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                     
  set <- function(y) {                          
    x <<- y                                     
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the value of the inverse has been caculated 
## (!is.null(m)). If it has been already calculated, then it displays the message
## "getting cached data - inverse matrix" and returns its value. In case it has
## not been calculated, then it triggers the process and cache the value of the
## inverse for next time it is needed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data-inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
