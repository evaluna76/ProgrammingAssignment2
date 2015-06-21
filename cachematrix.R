## Functions that cache the inverse of a matrix.

# The first function, makeVector creates a special "matrix", 
# which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


# calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
#in the cache via the setmean function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix)
        x$setmatrix(m)
        m
}


test = function(){
        set.seed(12343398)
        r = rnorm(1000)
        mat1 = matrix(r, nrow=2, ncol=2)
        
        x = makeCacheMatrix(mat1)
        cacheSolve(x)
        
#         mat1 = rbind(c(1, 1/3), c(1/2, 1))
#         x = makeCacheMatrix(mat1)
#         cacheSolve(x)
}