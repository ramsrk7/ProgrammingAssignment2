
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

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("printing cached data...")
        return(i)
    }
    matrx <- x$get()
    print(i<-solve(matrx,...))
    x$setinverse(i)
}
