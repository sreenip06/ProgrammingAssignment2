################################################################################
## Below functions provide a way to compute the inverse of a given matrix
## and cache it so we can avoid unnecessary intense computations to inverse

## makeCacheMatrix - this function defines a matrix with the given input,
## and incorporates sun-functions to set and get inverse of the input matrix

makeCacheMatrix <- function(x = matrix())
{
    # reset inverse in case the matrix input changes
    xi <- NULL
    
    # set - makes a matrix for given input
    set <- function(y = matrix())
    {
        x <<- y
        xi <<- NULL
    }
    
    # get - returns the input matrix
    get <- function() x
    
    # setsolve - sets the input matrix as the inverse 
    setsolve <- function(inv = matrix()) xi <<- inv
    
    # getsolve - returns the inverse of the matrix
    getsolve <- function() xi
    
    list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve - this function checks is the inverse of a given matrix is already 
## computed; if it is, function reads it from the cache; otherwise, function
## computes the inverse and caches it
 
cacheSolve <- function(x = matrix(), ...)
{
    # check if the inverse is already cached
    mi <- x$getsolve()
    if (!is.null(mi))
    {
        message("Getting cached inverse")
        return(mi)
    }
    # compute inverse and set it using setsolve
    m <- x$get()
    mi <- solve(m, ...)
    x$setsolve(mi)
    mi
}
################################################################################
