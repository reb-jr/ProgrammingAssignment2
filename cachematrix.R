## Put comments here that give an overall description of what your
## functions do

## returns a list containing setters and getters for both the original matrix ('x') and the inverted matrix
## caches the inverted matrix 
makeCacheMatrix <- function(x = matrix()) {
     cachedInvertedMatrix <- NULL
     
     #sets x to y and clears cachedInvertedMatrix
     set <- function(y){
          x <<- y
          cachedInvertedMatrix <<- NULL
     }
     
     #gets the x
     get <- function(){ x }
     
     #sets the cached matrix to the passed in value
     setInverse <- function(invertedMatrix){ cachedInvertedMatrix <<- invertedMatrix }
     
     #gets the cached matrix
     getInverse <- function(){ cachedInvertedMatrix }
     
     list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Returns the inverted matrix from 'x', generating and caching first when it's not already cached
##
## 'x' must be a list created by calling 'makeCacheMatrix()'
##
cacheSolve <- function(x, ...) {
     #check if the inverted matrix is already cached
     if(is.null(x$getInverse())){
          print('     not cached yet, solving and loading cache')
          #generated the inverse by calling solve() (in the base package) on the original matrix
          invertedMatrix <- solve(x$get(), ...)
          
          #cache the result of solve()
          x$setInverse(invertedMatrix)
     }
     
     print('     returning cached version')
     #return the cached invertedMatrix
     x$getInverse()
}

##function to test the functions above
# testCachedMatrix <- function(){
#      generateInvertableMatrix <- function(size,seed){
#           set.seed(seed)
#           matrix(rnorm(size^2),size,size)
#      }
# 
#      cm <- makeCacheMatrix(generateInvertableMatrix(10,516))
# 
#      print('iteration #1, call #1 to cacheSolve')
#      print('   call #1 to cacheSolve')
#      im <- cacheSolve(cm)
# 
#      print('   call #2 to cacheSolve')
#      im <- cacheSolve(cm)
# 
#      # print(cm$get())
#      # print(im)
#      iter1result <- identical(im,solve(cm$get()))
#      print(paste('   the results are identical =',iter1result))
# 
#      print('iteration #2,')
#      print('   using set() to change the original matrix')
# 
#      cm$set(generateInvertableMatrix(123,5013))
# 
#      print('   call #1 to cacheSolve')
#      im <- cacheSolve(cm)
# 
#      print('   call #2 to cacheSolve')
#      im <- cacheSolve(cm)
# 
#      # print(cm$get())
#      # print(im)
# 
#      iter2result <- identical(im,solve(cm$get()))
#      print(paste('   the results are identical =',iter2result))
#      
#      print('')
#      if(iter1result && iter2result)
#           print('PASSED')
#      else
#           print('FAILED')
# }

