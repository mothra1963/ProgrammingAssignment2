## makeCacheMatix and cacheSolve work in concert to return the inverse of square matrices that
##   been processed by these functions

##makeCacheMatrix - takes a square matrix and prepares it to be acted upon by my function cacheSolve
## you can set the contents of the matrix, yourMatrixHere, using yourMatrixHere$setmat(anotherMatrix), if anotherMatrix is 
##       not actually a matrix the set will not actually take place, 
##          anotherMatrix may be created using the matrix function
## you can get the contents of the matrix, yourMatrixHere, using yourMatrixHere$getmat()
## you can set the inverse of the matrix, yourMatrixHere, using yourMatrixHere$setsolve()
## you can get the inverse if the matrix, yourMatrixHere, using yourMatrixHere$getsolve()

makeCacheMatrix <- function(x = matrix()) {

       m <- NULL
        setmat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmat <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(setmat = setmat, getmat = getmat,
             setsolve = setsolve,
             getsolve = getsolve)

}

##cacheSolve returns the inverse of the provided square matrix, x
##   x must have been created using makeCacheMatrix for this function to work
## if the inverse of this matrix has already been calculated, a message will display indicating the solution 
##  is from the cached result
##cacheSolve may be used to display the result, by typing cacheSolve(yourMatrixHere) at the command prompt or in a script
##cacheSolve may be used to assign the result to another object by typing anotherMatrix <- cacheSolve(yourMatrixHere)
##   note that anotherMatrix has class list and if you want to find its inverse (which would be yourMatrixHere), you must
##      first run create another object yetAnotherMatrix <- makeCacheMatrix(anotherMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 	m <- x$getsolve()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmat()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
