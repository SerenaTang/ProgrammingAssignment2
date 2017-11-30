## Create special objects that store a matrix ans cache its inverse
## 

## Function creates a special "matrix", containing a function to
##      1. set value of the matrix
##      2. get value of the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv<<-solve
        getinverse <- function() inv
        list(set=set, get=get,
             setinverse=setinverse, getinverse=getinverse)
}


## Function calculates inverse of special matrix created with makeCacheMatrix.
## But first checks to see if the inverse has already been calculated.
## If yes, gets inverse from cache, otherwise calculates inverse and sets inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

