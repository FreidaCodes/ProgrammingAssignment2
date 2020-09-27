## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(Solve) m <<- Solve
        getinversematrix <- function() m
        list(set = set, get = get,
             setinvesematrix = setinversematrix,
             getinversematrix = getinversematrix)      
        
}


## computes inverse of a matrix not already cached; 
##  inverse of matrix is i cached, returns the cached matrix

cacheSolve <- function(x, ...) {
        
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- Solve(data, ...)
        x$setinversematrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
