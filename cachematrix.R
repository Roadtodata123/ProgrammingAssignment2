
makeCacheMatrix <- function(x = matrix()) {        # This function creates a special "matrix" object that can cache its inverse
        invm <- NULL                               # inverse matrix and reset to null whenever makeCacheMatrix is called

        f_get <- function() {x}                    #  this function get the original avlue of x i.e. the original matrix

        f_setinversem <- function(inversematrix)   # this is called by cacheSolve
                       {invm <<- inversematrix}    #  It uses the superassignment


        f_getinversem <- function() {invm}         # This get the cache value of the invverse matrix to cacheSolve

        list (get = f_get,                         # returning a list  of methods or functions
             setinversem = f_setinversem,          # I have prefix f_ to stop confusing myself
             getinversem = f_getinversem)          #                                                  # 
}



cacheSolve <- function(x, ...) {                   # This function returns a matrix that is the inverse of 'x'
            
        invm <- x$getinversem()                    # this gets the cached value of the inverse matrix
        if(!is.null(invm)) {                       # This tests whether the inverse matrix has been cached or not
                message("getting cached data")     # If yes, returns the cached value and comes out of the function
                return(invm)
        }
        v_matrix <- x$get()                        # if no, get the original value of the matrix
        invm <- solve(v_matrix, ...)               # 'inverse it'
        x$setinversem(invm)                        # store the inverse value of the matrix to the matrix object
        invm                                       # returns the newly calculated inverse matrix 
}