## Put comments here that give an overall descripti
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 in <- NULL
    set <- function(y) {
        x <<- y
        in <<- NULL
}
 get <- function() x
    setInverse <- function(inverse) in <<- inverse
    getInverse <- function() in
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 

## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
   in <- x$getInverse()
             if(!is.null(in)) {
                 message("getting cached data")
                 return(in)
             }
             data <- x$get()
             in <- Inverse(data, ...)
             x$setInverse(in)
             in
         }     
        ## Return a matrix that is the inverse of 'x'
}
