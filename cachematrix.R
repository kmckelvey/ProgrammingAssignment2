## This function accepts an inversible matrix x
## and creates functions to set and retrieve that matrix
## and the inverse of the matrix. The inverse is either 
## from a cached value or via the solve() function. 


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
## "cached matrix" x.
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
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
