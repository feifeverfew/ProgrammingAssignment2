## A pair of function to cache the inverse of a square matrix

## makeCacheMatrix to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m_in <- matrix(, nrow = nrow(x), ncol = ncol(x))
    set <- function(y) {
        x <<- y
        m_in <<- matrix(, nrow = nrow(x), ncol = ncol(x))
    }
    get <- function() x
    set_inverse <- function(inverse) m_in <<- inverse
    get_inverse <- function() m_in
    list(get = get,set =set,set_inverse = set_inverse,get_inverse = get_inverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if (!(all(is.na(inverse)))) {
        message ("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$set_inverse(inverse)
    inverse
}
