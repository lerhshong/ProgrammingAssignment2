## Largely similar to the code that was provided, with the obvious changes to
## support matrices instead of numerics and inverting matrices instead of 
## calculating means.

## makeCacheMatrix takes in a 'base' matrix x and returns a LIST of four functions, 
## set ---- changes the base matrix x used in makeCacheMatrix
## get ---- returns x. Used to see what the base matrix currently is.
## setinverse ---- changes the current inverse value of matrix x. Note that it 
##                 doesn't have solve in it so something nonsensical can also
##                 be used as a value
## getinverse ---- returns m, the current inverse value of matrix x.
## To access any of these values, we use the $ operator.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, but it first checks if it has been
## stored previously. If m isn't null that means we have done it before as in 
## above, in getinverse. In that case it will simply return the cached value. 
## Otherwise, it calculates the inverse then caches it as m.

cacheSolve <- function(x =matrix()) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

## Some example code to see that this works:
a <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(a)
cacheSolve(a)
## In the second cacheSolve, a message will appear telling that we are getting 
## cached data, as we should.