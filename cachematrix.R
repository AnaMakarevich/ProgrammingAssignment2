## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # tells us if the value is cached. NULL by default
        inverse <- NULL
        
        # sets the value of the matrix
        set <- function(value) {
                x <<- value
                inverse <<- NULL
        }
        
        # returns the value of the matrix 
        get <- function() x
        
        # sets the value of the inverse equal to the passed value
        set_inverse <- function(inverse_matrix) inverse <<- inverse_matrix
        
        # returns the inverse matrix (or NULL if it hasn't been set)
        get_inverse <- function() inverse
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # exctract the value of the inverse field
        inverse <- x$get_inverse() 
        
        # check if we have a cached matrix
        # if true we notify the user and return the cached value
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # this part of the program is executed iff the inverse is NULL
        # and we need to calculate the inverse matrix from the scratch
        
        # extract the data from the matrix and save it to data variable
        data <- x$get()
        
        #calculate the inverse matrix
        inverse <- solve(data, ...)
        
        #set the inverse matrix
        x$set_inverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
        
}

# Some tests

# create some random 5x5 matrix 
test_matrix <- matrix(rnorm(25,5,2), nrow = 5, ncol = 5)
# create an instance of cached matrix object
cmatrix <- makeCacheMatrix()
# set the value of cmatrix to test_matrix
cmatrix$set(test_matrix)
# trying to get the inverse matrix - it calculates it from the scratch
print(cacheSolve(cmatrix))
# now try to get the same contents again and check that it gets the cached copy
print(cacheSolve(cmatrix))


