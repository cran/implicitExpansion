
#' Apply a Function to Multiple Arrays with Implicit Expansion
#'
#' Similar function to [`mapply`] with support for implicit expansion.
#' 
#' @param FUN Function to apply to each combination of arguments. Found via [`match.fun`].
#' @param ... Objects that are coerced to arrays, expanded using implicit expansion, and then vectorized over.
#' @param SIMPLIFY If `TRUE`, the resulting list array is simplified to an atomic array if possible.
#' @param USE.NAMES If `TRUE`, the dimensions are named using the names of input arguments of matching size.
#' 
#' @return An array containing the result of `FUN` for each combination of entries from `...` after implicit expansion.
#' 
#' @details 
#' The arguments are handled in a similar fashion to [`mapply`] with some key differences:
#' - `MoreArgs` is omitted, since additional arguments can be passed in `...`, either as they are
#' (if they are atomic), or as a `list()` of length one.
#' - `SIMPLIFY` only simplifies a list array to an atomic array, nothing else.
#' - `USE.NAMES` uses names from all arguments, but never uses an argument itself as names.
#' 
#' @examples
#' summaries <- list(Max = max, Min = min, avg = mean)
#' data <- list(a = 1:5, b = 2:3, c = 20:12)
#' formatStrings <- array(c('%.1f', '%.3f'), c(1,1,2))
#' mmapply(function(f, d, s) sprintf(s, f(d)), summaries, t(data), formatStrings)
#' 
#' @seealso 
#' [`expandArray`], [`expandedDim`]
#' 
#' @export
mmapply <- function(FUN, ..., SIMPLIFY = TRUE, USE.NAMES = TRUE){
    FUN <- match.fun(FUN)
    arrays <- assertArrayList(...)
    dy <- expandedDim(arrays = arrays)
    dNames <- getExpandedDimnames(arrays, dy)
    arrays <- lapply(arrays, expandArray, dy)
    ret <- array(list(), dy)
    for(i in seq_len(prod(dy))){
        args <- lapply(arrays, '[[', i)
        ret[[i]] <- do.call(FUN, args)
    }
    if(SIMPLIFY){
        ret <- unlistArray(ret)
    }
    if(USE.NAMES){
        dimnames(ret) <- dNames
    }
    return(ret)
}

#' Implied Dimension of a set of Arrays
#' 
#' @description
#' Get the dimension implied by a set of arrays as used in implicit expansion.
#' 
#' @param ... Objects that are coerced to arrays.
#' @param arrays A list of objects that are coerced to arrays.
#' 
#' @return A numberical vector containing the expanded dimension implied by the arrays.
#' 
#' 
#' @details 
#' Both the arrays in `...` and `arrays` are considered by concatenating them with `c(list(...), arrays)`.
#' Throws an error if the arrays are not compatible.
#' 
#' @examples 
#' x <- 1:3
#' y <- t(4:5)
#' z <- array(0, c(1,1,6))
#' expandedDim(x, y, z)
#' 
#' @seealso 
#' [`expandArray`]
#' 
#' @export
expandedDim <- function(..., arrays=list()){
    arrays <- assertArrayList(..., arrays = arrays)
    dims <- lapply(arrays, dim)
    len <- max(sapply(dims, length))
    dims <- lapply(dims, padVector, len)
    getExpandedDim(dims=dims, doThrow = TRUE)
    de <- numeric(len)
    de <- do.call(pmax, dims)
    return(de)
}

#' Expand an Array to a Given Dimension
#' 
#' @description
#' Expand an array to a given dimension by repeating its entries in the directions
#' where the array has dimension one.
#' 
#' @param x An object that is coerced to array.
#' @param dy The dimensions it is to be expanded to.
#' 
#' @return An array that consists of the entries in `x`, with dimension `dy`.
#' 
#' @details 
#' Throws an error if the array and dimensions are not compatible.
#' 
#' @examples
#' x <- 1:3
#' expandArray(x, c(3,4))
#' 
#' @seealso 
#' [`expandedDim`]
#' 
#' @export
expandArray <- function(x, dy){
    x <- assertArray(x)
    dx <- dim(x)

    # Check args, return early:
    if(identical(dx, dy)){
        return(x)
    }
    if(length(dx) > length(dy)){
        stop('Cannot expand x, since it has more dimensions than the target.')
    }

    dx <- padVector(dx, length(dy))

    # Check args, return early:
    if(identical(dx, dy)){
        dim(x) <- dy
        return(x)
    }
    getExpandedDimDirectional(dx, dy, 'xy')

    expandedDims <- (dx < dy)
    p0 <- order(expandedDims)
    p1 <- order(p0)

    ret0 <- array(x, dy[p0])
    ret <- aperm(ret0, p1)

    return(ret)
}

getExpandedDimnames <- function(arrays, de=NULL){
    arrays <- assertArrayList(arrays = arrays)
    if(is.null(de)){
        de <- expandedDim(arrays = arrays)
    }
    nDims <- length(de)
    arrDimNames <- lapply(arrays, function(arr){
        dn <- dimnames(arr)
        dn <- padVector(dn, nDims, NULL)
        dn
    })
    foundDimNames <- FALSE
    dNames <- replicate(length(de), NULL)
    for(i in seq_along(de)){
        for(j in seq_along(arrays)){
            dn <- arrDimNames[[j]][[i]]
            if(!is.null(dn) && length(dn) == de[i]){
                dNames[[i]] <- dn
                foundDimNames <- TRUE
                break
            }
        }
    }
    if(!foundDimNames){
        dNames <- NULL
    }
    return(dNames)
}

getExpandedDimDirectional <- function(dx, dy, direction = c('both', 'xy', 'yx')[1]){
    len <- max(length(dx), length(dy))
    dx <- padVector(dx, len)
    dy <- padVector(dy, len)
    dimsCompatible <- (dx == dy)
    if(direction %in% c('both', 'xy')){
        dimsCompatible <- dimsCompatible | (dx == 1)
    }
    if(direction %in% c('both', 'yx')){
        dimsCompatible <- dimsCompatible | (dy == 1)
    }
    mismatchedDims <- which(!dimsCompatible)
    if(length(mismatchedDims) > 0){
        stop(
            'Mismatching dimensions:',
            paste0(mismatchedDims, collapse = ', ')
        )
    }
    de <- dx
    de[dy != 1] <- dy[dy != 1]
    return(de)
}

getExpandedDim <- function(..., dims=list()){
    dims <- c(list(...), dims)
    len <- max(sapply(dims, length))
    dims <- lapply(dims, padVector, len)
    dy <- 1 + numeric(len)
    dimsCompatible <- !logical(len)
    for(i in seq_along(dims)){
        dx <- dims[[i]]
        dimsCompatible <- dimsCompatible & (
            dx == dy
            | dx == 1
            | dy == 1
        )
        mismatchedDims <- which(!dimsCompatible)
        if(length(mismatchedDims) > 0){
            stop(
                'Mismatching dimensions for argument ',
                i,
                ': ',
                paste0(mismatchedDims, collapse = ', ')
            )
        }
        newInds <- (dx != 1)
        dy[newInds] <- dx[newInds]
    }
    return(dy)
}

unlistArray <- function(arr){
    isAllAtomic <- all(sapply(arr, function(x) {
        is.atomic(x) && length(x) == 1
    }))
    if(!isAllAtomic){
        return(arr)
    }
    dimNames <- dimnames(arr)
    dy <- dim(arr)
    arr <- unlist(arr, recursive = FALSE)
    dim(arr) <- dy
    dimnames(arr) <- dimNames
    return(arr)
}

