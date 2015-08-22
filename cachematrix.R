## These functions allow us to compute the inverse
## of a matrix using cached data and therefore make
## computation way faster since the inverse is already
## pre-calculated

## similar to the example provided in
## the instructions, the function returns a list:
## set matrix
## get matrix
## set inverse
## get inverse
## and gives us a matrix (x) that can be cached
## for input in cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        ## <<- will assign a value to an object in an
        ## environment that is different from the current environment
        get <- function() x
        setinver <- function(inverse) inver <<- inverse
        getinver <- function() inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}

## returns inverse of matrix used in
## makeCacheMatrix
cacheSolve <- function(x, ...) {
        inver <- x$getinver()
        if(!is.null(inver)) {
                ## gets data from cache, if matrix is
                ## already calculated and cached
                message("getting cached data")
                return(inver)
        }
        ## if we have no matrix in the cache, it'll
        ## calculate the inverse regularly
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
        ## Returns a matrix that is the inverse of 'x'
}
