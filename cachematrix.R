## ##General comments
## This pair of functions store the inverse of a matrix in the cache.

## If we assume the contents of a matrix are not changing, it may make 
## sense to cache the value of the inverse of a matrix so that 
## when we need it again, it can be looked up in that cache rather than 
## recomputed.
## 
## To do that I tryed to take advantage of the scoping rules of the R language 
## and how they can be manipulated.
##
## To use these functions (call them, execute them) it is neccesary to create a main "environment" from where to use them. 
For example, I called them defining the mayrix "mymatrix" as follows:

## > mymatrix <- makeCacheMatrix()
## > mymatrix$set (matrix(c(1,0,3,2,2,4,3,2,1),ncol=3))
## > cacheSolve (mymatrix)
##

## 
##
##
    makeCacheMatrix: As required, this function creates a special "matrix" object that can cache its inverse.
##
    

makeCacheMatrix  <- function(x = matrix()) {
        mm <- NULL
        set <- function(yy) {
                x <<- yy
                mm <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mm <<- solve
        getinv <- function() mm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve
## As requiered, this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##

cacheSolve <- function(x=matrix(), ...) {
        mm <- x$getinv()
        if(!is.null(mm)) {
                message("Getting inverse of the matrix cached")
                return(mm)
        }
        datamat <- x$get()
        mm <- solve(datamat, ...)
        x$setinv(mm)
        mm
}
