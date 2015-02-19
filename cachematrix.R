#' cachematrix.R contains 3 functions: makeCacheMatrix, cacheSolve and test
#' -makeCacheMatrix enable us to create special object which is able to holds matrix and its inverse version
#' -cacheSolve enable us to get inverse matrix from list created by makeCacheMatrix by access to its cache field or
#' by calculating it if it was not calculated yet
#' -test this function will run several tests of makeCacheMatrix and cacheSolve


#' this function will return special list which holds 4 functions which are necessary for our CacheMarix
#' @param x matrix(), x has to be a square matrix
#' @return a List with 4 functions
makeCacheMatrix <- function(x = matrix()) {
  # if x in not matrix we throw an exception
  if(!is.matrix(x)){
    stop("input parameter is not matrix")
  }
  # if x in not square matrix we throw an exception
  if(nrow(x)!=ncol(x)){
    stop("input parameter have to be a square matrix")
  }
  # we initialize variable m with NULL
  m <- NULL
  # this inner function allows as to set new matrix, in that case inverse matrix is set back to NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  # this inner function return original matrix
  get <- function() x
  # this inner function return inverse matrix cached at field m
  getInv <- function() m
  # this inner function is a setter which set an inverse matrix to field m
  setInv <- function(inv) m <<- inv
  # and finally we return a list of 4 function set, get, setInv and getInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


#' this function returns a matrix that is the inverse of 'x'
#' as an input is used list which was created by makeCacheMatrix,
#' if this is first run, it will calculate inverse matrix, save it to the list and return inverse matrix to us
#' for every other runt it simply return cached inverse matrix as long as did not call set method of this object
#' @param x list created by makeCacheMatrix
#' @return a inverse matrix of X
cacheSolve <- function(x, ...) {
  # return a matrix that saved in field m of makeCacheMatrix instance'
  m <- x$getInv()
  if(!is.null(m)) {
    # if this object is not null, we will use it
    message("getting cached data")
    return(m)
  }
  # if m was null, we will have to calculate it
  # we get the field x which holds the original matrix and put it to the variable data
  data <- x$get()
  message("calculating data")
  # we will use solve function on matrix to produce inverse matrix
  m <- solve(data)
  # we sill set this calculated inverse matrix to the instance of object, to allow us use it next time
  x$setInv(m)
  # finally we return calculated inverse matrix
  m
}

#' test function offers us test functionality to test makeCacheMatrix and cacheSolve
test <- function(){
  x<-matrix(c(1,2,3,4), nrow=2, ncol=2) 
  data<- makeCacheMatrix(x)
  result <- cacheSolve(data)
  test<-matrix(c(-2,1,1.5,-0.5), nrow=2, ncol=2) 
  print(result)
  print(' output should be:')
  print(' calculating data')
  print(test)
  if(identical(test,result)){
    print('Inverse function is calculated correctly')
  }
  result <- cacheSolve(data)
  print(result)
  print(' output should be:')
  print(' getting cached data')
  print(test)
  if(identical(test,result)){
    print('Inverse function is cached correctly')
  }
  tryCatch(
    result <- makeCacheMatrix(1), 
    error = function(e){
      print(e)
    }, 
    finally={
      print("output should be")
      print("<simpleError in makeCacheMatrix(1): input parameter is not matrix>")
    }) 
  tryCatch(
    result <- makeCacheMatrix(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3) ), 
    error = function(e){
      print(e)
    }, 
    finally={
      print("output should be")
      print("<simpleError in makeCacheMatrix(1): input parameter have to be a square matrix>")
    }) 
  
}
