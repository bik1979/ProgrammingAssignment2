## instead of computing repeteadly matrix inverses, it's possible to cache
## the results of the computation so they can be used later if needed

## provides a list object, where every of the entries is a funcion that provides
## access to read/write the matrix stored and the inverse matrix cached
makeCacheMatrix <- function(m = matrix()) {
    minv <- NULL # init inverse to null
    #sets the value of the matrix
    set <- function(y) {
        # check that y is matrix
        if(is.matrix(y)) {
            m <<- y # store y 
            minv <<- NULL #init inverse    
        }
    }
    #get just returns the matrix
    get <- function() m 
    #set the cached value for the inverse matrix
    setInv <- function(inv) minv <<- inv
    #get the cached inverse matrix
    getInv <- function() minv
    #return a list containing the four functions
    list(set = set, get = get,
            setInv = setInv,
            getInv = getInv)
}


## calculates the inverse of a matrix, wrapped in an special format, as explained
## in makeCacheMatrix. Before calculating the inverse matrix, it checks if the
## inverse has been already calculated. If so, returns the cached value. If not,
## the inverse is calculated using solve() function and result is cached.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getInv()
    # check if we have a not null value, that means inverse has been already
    # calculated and cached
    if(!is.null(minv)) {
        message("using cached data")
        return(minv)
    }
    #get the matrix
    m <- x$get()
    # calculate inverse
    minv <- solve(m)
    # set the cached value in our "special" matrix
    x$setInv(minv)
    # return the inverse
    minv
}
