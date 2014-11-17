# assignment week 3 r programming
# edresch

makeCacheMatrix <- function(x = matrix()) {
        ## This generator will create a matrix object
        ## s (the inverse) will be set to NULL every time
        ## the generator is called
        s <- NULL
        # set method will set the new matrix (y) for this object
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get method will return the contents of the matrix object
        get <- function() x
        ## setinv method will calculate the inverse of the matrix and 
        ## assign it to s
        setinv <- function(solve) s <<- solve
        ## getinv method will return the inverse of the matrix
        getinv <- function() s
        ## list of the methods
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## s wil be the inverse, called by the getinv method
        s <- x$getinv()
        ## if the return value is not null
        ## it will return the cached value for s
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## otherwise it will assign the contents of the matrix
        ## to the variable data using the get method
        data <- x$get()
        ## the solve function is then called on data
        ## thus calculating the inverse of the matrix and returning it as s
        s <- solve(data, ...)
        x$setinv(s)
        s
}
