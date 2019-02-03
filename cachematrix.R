##These functions are designed to cache the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        if (!is.na(solve(x)){
            x <<- y
            inv <<- NULL
        }
    }
    get <- function() x
    setinv <- function(invx) inv <<- invx 
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The function cacheSolve computes the inverse of the inverse of 
## the special matrix returned by makeCacheMatrix above. Above all
## it checks whether the inverse has been caculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, 
## it caculates the inverse of the matrix x and set the inverse in the
## cache via setinv function

cacheSolve <- function(x, ...) {
    invx <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(invx)
    }
    mat <- x$get()
    invx <- solve(mat)
    x$setinv(invx)
    invx
}
