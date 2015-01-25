## The functions below compute the inverse of a matrix and also check if the 
## inverse of the given has already been computed. If the inverse of a matrix has
## not been computed earlier, the inverse is calculated while the inverse of a previously
## computed matrix is fetched from cache.  

## The function caches the inverse of a given matrix 
## The given matrix is fetched and assigned using the set and get functions, while the
## inverse of the matrix is computed using R's solve function. The final inverse of
## the given matrix that is stored in m is fetched using getmatrix method. 

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## The function checks to see if the inverse of the matrix has already been computed
## If that is the case, the cached inverse of the matrix is fetched. 
## Else, the inverse of the matrix is calculated.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
