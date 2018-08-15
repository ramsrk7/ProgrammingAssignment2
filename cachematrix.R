## The makeCacheMatrix function is used to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
      }
    get <- function() x
    setinverse <- function(inverse){ i <<- inverse }
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function is used to compute the inverse of the special "matrix" 

##If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("printing cached data...")
        return(i)
    }
    matrx <- x$get()
    print(i<-solve(matrx,...))
    x$setinverse(i) ## Return a matrix that is the inverse of 'x'
}
