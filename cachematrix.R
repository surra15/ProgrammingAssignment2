## Efficient way to cache matrix inversions and retrieve as needed
##      The makeCacheMatrix() caches the inverse of a given matrix and provides 
##          a way to retrieve the same
##      The cacheSolve() function utilizes the methods setup in 
##          makeCacheMatrix() to return if an inverse exists
##          if one does not exist, it will compute an inverse and
##          cache the same
##      To check if you got the correct inverse you could 
##          use the following R code
##          solve(m) %*% m1 == diag(nrow = nrow(m), ncol = ncol(m))
##          where m is your input matrix
## Known limitations
##      The input matrix is assumed to be square; currently no checks are done
##      the input matrix is invertible
## Note: set Tab to 4 spaces for better readability

## How to use:
## one way of testing the functions
##      m1 <- matrix(1:4, nrow=2)           ## create a square matrix
##      m_methods <- makeCacheMatrix(m1)    ## setup methods 
##      m_methods$set(m1)                   ## set the matrix
##      cacheSolve(m_methods)               ## now calculate the inverse
## you can wrap the cacheSolve() function call with Sys.time() to check time it
##      it takes to run with and without caching


## makeCacheMatrix is a function that sets up functions (or methods) 
##      for storage and retrieval of the original matrix
##      and also the storage and retrieval of the inverse of the matrix
##      The following are the method names
##      get() -- returns the matrix
##      set() -- sets the passed in matrix
##      getinv() -- returns the inverse of the matrix if one exists else is NULL
##      setinv() -- sets the inverse of the matrix (passed in and not calculated)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## set the inverse to NULL
    ## setting the input matrix & inverse in the parent environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse ## store the inverse of the matrix
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  ## create a list of the methods
}


## cacheSolve takes a matrix and inverses it
##      it utilizes the methods setup in makeCacheMatrix() function above
##      it an inverse exists (getinv() method), and returns the same
##      if one does not exist, then calculates an inverse with solve() function
##          stores this inverse (setinv() method)
##          and returns this inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        inv <- x$getinv()   ## get the inverse of the matrix
        if (is.matrix(inv)) {
            message("getting cached data")
            return(inv)     ## return the inverse if it is already cached
        }
        ## if inverse does not exist, calculate one
        data_mat <- x$get()
        inv <- solve(data_mat) ## use solve() to inverse the matrix
        x$setinv(inv)   ## cache the inverse
        inv
}
