## The makeCacheMatrix() function calculates the inverse matrix of a given matrix X. 
## First it searchs if the inverse matrix has already been calculated. If so, 
## it retrives the value of the inverse amtrix from the cache. If not, it calculates the value


makeCacheMatrix <- function(x=matrix()){
        m_inv <- NULL
        set <- function(y){
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x,...){
        m_inv <- x$getinv()
        if(!is.null(m_inv)){
                message("Getting cached data")
                return(m_inv)
        }
        val <-x$get()
        m_inv <- solve(val,...)
        x$setinv(m_inv)
        m_inv
}