## This function creates a special "matrix" that will cache the inverse of a matrix. 

makeCacheMatrix <- function(A = matrix()) {
        Inv <- NULL
        set <- function(B) {
                A <<- B
                Inv <<- NULL
        }
        get <- function() A
        setInverse <- function(solve) Inv <<- solve
        getInverse <- function() Inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of a matrix. If the inverse was already cached,
## then it will return this inverse matrix with the message: 'getting cached data'.

cacheSolve <- function(A, ...) {
        Inv <- A$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- A$get()
        Inv <- solve(data, ...)
        A$setInverse(Inv)
        Inv
}
