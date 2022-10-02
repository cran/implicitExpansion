
#' Apply a Function to Multiple Arrays with Implicit Expansion
#'
#' Similar function to [`mapply`] with support for implicit expansion.
#' 
#' @param FUN Function to apply to each combination of arguments. Found via [`match.fun`].
#' @param ... Objects that are coerced to arrays, expanded using implicit expansion, and then vectorized over.
#' @param MoreArgs Pass arguments in a list instead of `...`
#' @param SIMPLIFY If `TRUE`, the resulting list array is simplified to an atomic array if possible.
#' @param USE.NAMES If `TRUE`, the dimensions are named using the names of input arguments of matching size.
#' @param ALLOW.RECYCLING Whether to allow recycling of elements in each dimension.
#' 
#' @return An array containing the result of `FUN` for each combination of entries from `...` after implicit expansion.
#' 
#' @details 
#' Most arguments are handled in a similar fashion to [`mapply`] with some key differences:
#' - Entries of `MoreArgs` are treated the same as the ones in `...`, i.e.
#' `mmapply(...)` is the same as `mmapply(MoreArgs = list(...))`.
#' Additional arguments to FUN can be passed here or in `...`, either as they are
#' (if they are atomic), or as a `list()` of length one.
#' - `SIMPLIFY` only simplifies a list array to an atomic array, nothing else.
#' - `USE.NAMES` uses names from all arguments, but never uses an argument itself as names.
#' 
#' If `ALLOW.RECYCLING` is set to `TRUE`, all arrays of any size are compatible.
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
mmapply <- function(FUN, ..., MoreArgs = list(), SIMPLIFY = TRUE, USE.NAMES = TRUE, ALLOW.RECYCLING = FALSE){
    FUN <- match.fun(FUN)
    arrays <- c(list(...), MoreArgs)
    arrays <- assertArrayList(arrays = arrays)
    dims <- lapply(arrays, dim)
    dy <- getExpandedDim(dims=dims, allowRecycling = ALLOW.RECYCLING)
    dNames <- getExpandedDimnames(arrays, dy)
    nd <- length(dy)
    dims <- lapply(dims, padVector, nd, 1)
    for(i in seq_along(arrays)){
        dim(arrays[[i]]) <- dims[[i]]
    }
    ret <- array(list(), dy)
    cp <- cumprod(c(1, dy[-length(dy)]))
    for(i in seq_len(prod(dy))){
        ai <- ((i - 1) %/% cp %% dy) + 1 # cf. `linearToArrayIndex()`
        args <- lapply(arrays, extractImpliedIndex, ai)
        ret[i] <- list(do.call(FUN, args))
    }
    if(SIMPLIFY){
        ret <- unlistArray(ret)
    }
    if(USE.NAMES){
        dimnames(ret) <- dNames
    }
    return(ret)
}


linearToArrayIndex <- function(i, dx){
    i0 <- i - 1
    cp <- cumprod(c(1, dx[-length(dx)]))
    ai0 <- i0 %/% cp %% dx + 1
    ai0 + 1
}

extractImpliedIndex <- function(arr, ind){
    impInd <- 1 + ((ind - 1) %% dim(arr))
    do.call('[[', c(list(arr), impInd))
}
getImpliedIndex <- function(dx, ind){
    1 + ((ind - 1) %% dx)
}

#' Implied Dimension of a set of Arrays
#' 
#' @description
#' Get the dimension implied by a set of arrays as used in implicit expansion.
#' 
#' @param ... Objects that are coerced to arrays.
#' @param arrays A list of objects that are coerced to arrays.
#' @param allowRecycling Whether to allow recycling of elements in each dimension.
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
expandedDim <- function(..., arrays=list(), allowRecycling=FALSE){
    arrays <- assertArrayList(..., arrays = arrays)
    dims <- lapply(arrays, dim)
    len <- max(sapply(dims, length))
    dims <- lapply(dims, padVector, len)
    de <- getExpandedDim(dims=dims, allowRecycling = allowRecycling)
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
#' [`expandedDim`], [`mmapply`]
#' 
#' @export
expandArray <- function(x, dy){
    y <- array(NA, dy)
    mmapply(function(xx, yy) xx, x, y, SIMPLIFY = !is.list(x))
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

getExpandedDimDirectional <- function(dx, dy, allowRecycling=FALSE){
    len <- max(length(dx), length(dy))
    dx <- padVector(dx, len)
    dy <- padVector(dy, len)
    if(allowRecycling){
        dimsCompatible <- !(dx == 0 & dy != 0)
    } else{
        dimsCompatible <- (dx == dy) | (dx == 1)
    }
    if(any(!dimsCompatible)){
        stop(
            'Mismatching dimensions:',
            paste0(which(!dimsCompatible), collapse = ', ')
        )
    }
    return(dy)
}

getExpandedDim <- function(..., dims=list(), allowRecycling=FALSE){
    dims <- c(list(...), dims)
    len <- max(sapply(dims, length))
    dims <- lapply(dims, padVector, len)
    dy <- rep(1, len)
    dimsCompatible <- !logical(len)
    for(i in seq_along(dims)){
        dx <- dims[[i]]
        if(allowRecycling){
            newInds <- (dy != 0 & dx > dy) | (dx == 0)
        } else {
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
        }
        dy[newInds] <- dx[newInds]
    }
    return(dy)
}

unlistArray <- function(arr){
    # Check if array can be unlisted:
    isAllAtomic <- all(sapply(arr, function(x) {
        is.atomic(x) && length(x) == 1
    }))
    if(!isAllAtomic){
        return(arr)
    }
    # Get dim and dim-names:
    dimNames <- dimnames(arr)
    dy <- dim(arr)
    # Handle early if arr has a 0-dim:
    if(length(arr) == 0){
        arr <- array(NA, dim(arr))
        dimnames(arr) <- dimNames
        return(arr)
    }
    # Make unlisted array:
    arr <- unlist(arr, recursive = FALSE)
    dim(arr) <- dy
    dimnames(arr) <- dimNames
    return(arr)
}

