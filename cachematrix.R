## The functions here calculate and cache the inverse matrix of a matrix, if have not been calculated before
## 

## make a class-like variable containing all the data

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  ## s is the variable for the solved matrix, aka inverse matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        } ## set the value of the matrix
        get <- function() x  ## get the value of the matrix
        setsolved <- function(solved) s <<- solved  ##set the value of the solved matrix
        getsolved <- function() s  ## get the value of the solved matrix
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)  ## return the list containing four functions.

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolved()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }  ## If the solved matrix exists, get it directly.
        data <- x$get() 
        s <- solve(data, ...) ## else, solve it and cache it.
        x$setsolved(s)
        s
}
