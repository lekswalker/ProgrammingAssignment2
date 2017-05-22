## Cashing the Inverse of a Matrix

## This function creates a special "matrix" object taht can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
        {
        inversed <- NULL
        
        set <- function(y)
        {
                x <<- y
                inversed <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) inversed <<- solve
        getsolve <- function() inversed
        
        list(set = set, get = get, setsolve = setsolve, 
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getsolve()
        
        if(!is.null(inversed)) 
        {
                message("getting cached data")
                return(inversed)
        }
        
        data <- x$get()
        inversed <- solve(data, ...)
        x$setsolve(inversed)
        inversed
}

## Code to test the functions with time tracking
set.seed(1010201)
r <- rnorm(100000)
M <- matrix(r, nrow=100, ncol=100)
M1 <- makeCacheMatrix(M)
start.time <- Sys.time()
cacheSolve(M1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
