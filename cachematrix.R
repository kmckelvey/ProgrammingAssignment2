## This function accepts a inversible matrix x
## and creates functions to retrieve that matrix
## and the inverse of the matrix.  It also defines
## a function that caches the inverse of the matrix.
## The set function initializes a new inversible matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(i) inv <<- i
    
    getinverse <- function() inv
    
    list(get        = get,
         setinverse = set,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a given
## "cached matrx" x.
## if first checks to see if the inverse has been cached,
## and returns the cached value if it has.
## otherwise it calculates the inverse and caches
## that value.

cacheSolve <- function(x) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    if (!is.null(inv))
    {
        #message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
