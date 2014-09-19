# test cases

runtests <-function()
{    
    message("Test 1")
    a <- rbind(c(1,4,9),c(2,4,6),c(3,3,3))
    message("a="); print(a)
    b <- makeCacheMatrix(a)
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 1 repeat with cache")
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 2")
    a <- rbind(c(1,2,3),c(4,4,3),c(9,6,3))
    message("a="); print(a)
    b <- makeCacheMatrix(a)
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 2 repeat with cache")
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 2 again with cache")
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 3: matrix size 1")
    a <- as.matrix(9)
    message("a="); print(a)
    b <- makeCacheMatrix(a)
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 3 repeat with cache")
    c <- cacheSolve(b)
    message("c="); print(c)
    d <- round(a %*% c, 0.000001)  # get ident (round to remove small calc residuals)
    message("ident="); print(d)
    
    message("Test 4: empty matrix")
    b <- makeCacheMatrix()
    c <- cacheSolve(b)
    message("c="); print(c)
    
    message("Test 4 repeat")
    c <- cacheSolve(b)
    message("c="); print(c)
    
    message("Test 5 non decorated matrix passed to cacheSolve")
    c <- cacheSolve(matrix(1:9, 3, 3))
}
