## These functions create a special "matrix" object that can cache its inverse

## "makeCacheMatrix" takes matrix x as an argument and returns a list 
## of functions to get and set the value of x, and value of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <- function(y) {
                x<<- y
                i<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set=set, get=get,      #returns a list (of functions)
             setinverse=setinverse,
             getinverse=getinverse)
}

## Gets the inverted matrix from the cache of "makeCacheMatrix"
## or calculates it if it's not in the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse() # get the inverse matrix value from cache
        if(!is.null(i)) {   # if inverse is already calculated (=not NULL), get it
                message("getting cached data")
                return(i)
        }
        data <- x$get() # if inverse is not calculated yet, do the calculation now
        i <- solve(data, ...) # "solve" returnes the inverted matrix
        x$setinverse(i) # put the result in matrix x cache
        i            # return the inverted matrix
}