## creates a special "matrix" object that can catch its inverse
makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y){
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## computes the inverse of the special matrix returned by make CacheMatrix. If inverse has already been calculated (and matrix has not changed), then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data,...)
        x$setinverse(mat)
        mat
}
