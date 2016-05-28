## Put comments here that give an overall description of what your
## functions do

##the two functions together allow a user to store a matrix and calculated inverses in cache. When using makeCacheMatrix
##it stores the data and contains the functions within scope to be called to retrieve original matrix, or its inverse or
## to invoke the solve() function. The cacheSolve() function looks in cache for an existing calculated inverse, if not
## calculates it and writes it to cache to be potentially retrieved in future.

#makeCacheMatrix stores matrix arg x into cache. It associates previously calculated inverses in cache through setinv()
#and writes it to the cache. It also stores the solution 'inv' as the element getinv witin the functional list. 

makeCacheMatrix <- function(x = matrix()) {     
        inv <- NULL
        set <- function(y) {                    #whenever set() is called, which is whenever makeCacheMatrix is called
                x <<- y                         #it writes over old cache data, which is x (matrix) and inv (matrix)
                inv <<- NULL
        }
        get <- function() x                     #get function simply is the matrix arg to makeCacheMatrix
        setinv <- function(solve) inv <<- solve #setinv(solve) writes solve to the cache stored in inv
        getinv <- function() inv                #getinv() simply is inv
        list(set = set, get = get,              #assigns makeCacheMatrix as a list vector of functions (set, get, setinv and getinv)
             setinv = setinv,
             getinv = getinv)
}


## CacheSolve takes x, which is a list with elements $set, $get, $setinv, $getinv. This is an output of the
## makeCacheMatrix function above which stores original matrix in $get, the funtion setinv() in $setinv and the
## inverse output in $getinv. If cacheSolve finds the inverse matrix in $getinv it returns it. Otherwise it obtains
## the data, assigns the solution to the solve() function for calculating inverses and runs the setinv() function
## under scope of makeCacheMatrix(x)$setinv to assign the inverse calculated to cache in inv.

cacheSolve <- function(x, ...) {                        
        ## Return a matrix that is the inverse of 'x'   #NB/ solve(a) returns the inverse of a. 
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


