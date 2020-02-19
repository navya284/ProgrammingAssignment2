## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

      ## Initialization the inverse property
      i <- NULL

       ## setting up the matrix
      set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
     }

    ## Get the matrix
    get <- function() {
      ## Return the matrix
      m
    }

    ## setting up the inverse of matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getInverse <- function() {
        ## Return it
        i
    }

## Return list of respective methods
list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated and if and only if the matrix has not
## changed, then the "cachesolve" should retrieve inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculation of the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}