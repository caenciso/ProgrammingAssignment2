## These functions were created to more efficiently return the inverse of an matrix

## below makeCacheMatrix function creates the set(), get(), setinverse() and getinverse() functions that are executed
## in cacheSolve function and assigns them as named elements within a list 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                       ## initialize inverse variable as NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }                                               ## define set() function, with y arg = x and i = NULL to clear previous cached i values
        get <- function() x                             ## define get() function
        setinverse <- function(solve) i <<- solve       ## define setInverse() function to calculate inverse using solve()
        getinverse <- function() i                      ## define getInverse() function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)                   ## assign each function as an named element in a list 
}


## the cacheSolve function allows the user to pass in a matrix and checks if there is a cached inverse value for that matrix.  
## If it exists, the cached inverse matrix is returned. If it does not exist, it is calculated and cached (set)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()             ## returns cached inverse value
        if(!is.null(i)) {
                message("getting cashed data")
                return(i)
        }                               ## if cached inverse value is not NULL, print message and return cached inverse value
        data <- x$get()                 ## gets matrix from input object
        i <- solve(data,...)            ## calculates inverse of matrix
        x$setinverse(i)                 ## set the inverse to i
        i                               ## return i, the calculated inverse matrix
        
        ## Return a matrix that is the inverse of 'x'
}
