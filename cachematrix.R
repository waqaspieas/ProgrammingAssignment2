# The function, "makeCacheMatrix" creates a special "matrix", which is really a list containing
# functions to get and set the matrix values and matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    setMatrix <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    getMatrix <- function() x
    setInv <- function(inv) xInv <<- inv
    getInv <- function() xInv
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}


## The function "cachesolve" checks if the inverse of the matrix is already calculated, the stored
## inverse matrix is returned, otherwise it calcuates and sores the inverse of the matrix

cacheSolve <- function(x, ...) {
    xInv <- x$getInv()
    if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    } else {
        message("calculating the inverse") 
    }
    data <- x$getMatrix()
    xInv <- solve(data)
    x$setInv(xInv)
    xInv
}

# Sample code to test the above two functions
myMat <- matrix(round(rnorm(16, 0, 3)), 4, 4)
x <- makeCacheMatrix(myMat)
cacheSolve(x)