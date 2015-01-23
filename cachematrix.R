## Here is a pair of functions that cache the inverse of a matrix

## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special list containing a functions to
        ## "set" - set the matrix
        ## "get" - get the matrix
        ## "setinverse"- set the inverse of the matrix
        ## "getinverse"- get the inverse of the matrix
        
        inv<-NULL
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}

## Function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Block 1. Returns the inverse matrix if it is already calculated. 
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        } 
        ## Block 2. Calculates and returns the inverse of matrix if it was not cached
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
