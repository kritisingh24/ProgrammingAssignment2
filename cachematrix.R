## A pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {

	
    inv <- NULL

    ## set the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ## get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## the inverse of the matrix
    setInv <- function(inverse) {
        inv <<- inverse
    }

    ## the inverse of the matrix
    getInv <- function() {
        ## Return the inverse property
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInv(m)

    ## Return the matrix
    m
}
