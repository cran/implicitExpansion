
padVector <- function(v, len, padding = 1){
    if(length(v) >= len){
        return(v)
    }
    if(is.null(padding)){
        padding <- list(NULL)
    }
    w <- rep(padding, len - length(v))
    v <- c(v, w)
    return(v)
}

trimVector <- function(v, padding = 1){
    while(length(v) > 0 && v[length(v)] == padding){
        v <- v[-length(v)]
    }
    return(v)
}

assertArray <- function(x){
    if(is.array(x)){
        x
    } else if(is.vector(x)){
        as.mColVector(x)
    } else {
        as.array(x)
    }
}

assertArrayList <- function(..., arrays=list()){
    arrays <- c(list(...), arrays)
    arrays <- lapply(arrays, assertArray)
    return(arrays)
}
