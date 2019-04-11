## The makeCacheMatrix function takes a square invertible matrix as an argument
## and returns a list with functions to set the value of the matrix, get the value of the matrix, set the value 
## of the inverse matrix and get the value of the inverse matrix respectively. The output can be used as an argument
## in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)

}

## The cacheSolve function calculates the inverse of the matrix which is given as an argument to the makeCacheMatrix, by taking the 
## output of this makeCachMatrix function as an argument. 
## It gets the inverse matrix from the cache if it has already been calculated and then skips the computation. 
## If not, it calculates the inverse matrix and sets the value of the inverse matrix in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("getting data from cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m
}



