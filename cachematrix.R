#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#
#Set the value of the matrix
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        #Get the value of the matrix
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
#Test the value of the matrix and, if it is not null, retrieve the value from
#the cache.
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #If the cache is null, calculate the inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}