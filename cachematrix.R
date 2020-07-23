# These functions accept a matrix, inverse it and store the inversed matrix in
# cache memory

# makeCacheMatrix is basically a list of 4 functions, which are resetMat, getMat, 
# getInv, setInv)
# resetMat can be used to modify the value of x without creating new objects.

makeCacheMatrix <- function(x = matrix()) {
        
        invMat <- NULL
        resetMat <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        getMat <- function() x
        setInv <- function(invMat) invMat <<- invMat
        getInv <- function() invMat
        list(resetMat = resetMat,
             getMat = getMat,
             getInv = getInv,
             setInv = setInv)
        
}

# cacheSolve is actually responsible for Inversion of matrix using solve() function

cacheSolve <- function(x, ...) {
        invMat <- x$getInv()
        if(!is.null(invMat)) {
                message("Getting cached data")
                return(invMat)
        }
        mat <- x$getMat()
        invMat <- solve(mat, ...)
        x$setInv(invMat)
        invMat
}

#All the best