#' @rdname textstat_simil_old
#' @export
#' @param p The power of the Minkowski distance.
#' @details `textstat_dist` options are: `"euclidean"` (default), 
#'   `"chisquared"`, `"chisquared2"`, 
#'   `"kullback"`. `"manhattan"`, `"maximum"`, `"canberra"`,
#'   and `"minkowski"`.
#' @references The `"chisquared"` metric is from Legendre, P., & Gallagher,
#'   E. D. (2001).
#'   "[Ecologically
#'    meaningful transformations for ordination of species data](http://adn.biol.umontreal.ca/~numericalecology/Reprints/Legendre_&_Gallagher.pdf)".
#'   *Oecologia*, 129(2), 271-280. doi.org/10.1007/s004420100716
#'   
#'   The `"chisquared2"` metric is the "Quadratic-Chi" measure from Pele,
#'   O., & Werman, M. (2010). 
#'   "[The
#'   Quadratic-Chi Histogram Distance Family](https://link.springer.com/chapter/10.1007/978-3-642-15552-9_54)". In *Computer Vision - ECCV
#'   2010* (Vol. 6312, pp. 749-762). Berlin, Heidelberg: Springer, Berlin,
#'   Heidelberg. doi.org/10.1007/978-3-642-15552-9_54.
#'
#'   `"kullback"` is the Kullback-Leibler distance, which assumes that
#'   \eqn{P(x_i) = 0} implies \eqn{P(y_i)=0}, and in case both \eqn{P(x_i)} and
#'   \eqn{P(y_i)} equals to zero, then \eqn{P(x_i) * log(p(x_i)/p(y_i))} is
#'   assumed to be zero as the limit value.  The formula is:
#'    \deqn{\sum{P(x)*log(P(x)/p(y))}}
#'    
#'   All other measures are described in the \pkg{proxy} package.
#' @importFrom RcppParallel RcppParallelLibs
#' @keywords internal textstat
#' @author Kenneth Benoit, Haiyan Wang
textstat_dist_old <- function(x, selection = NULL, 
                          margin = c("documents", "features"),
                          method = "euclidean",
                          upper = FALSE, diag = FALSE, p = 2) {
    UseMethod("textstat_dist_old")
}
    
#' @export
textstat_dist_old.default <- function(x, selection = NULL, 
                                  margin = c("documents", "features"),
                                  method = "euclidean",
                                  upper = FALSE, diag = FALSE, p = 2) {
    stop(friendly_class_undefined_message(class(x), "textstat_dist"))
}
    
#' @export
textstat_dist_old.dfm <- function(x, selection = NULL, 
                              margin = c("documents", "features"),
                              method = "euclidean",
                              upper = FALSE, diag = FALSE, p = 2) {
    
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    margin <- match.arg(margin)
    method <- char_tolower(method)
    
    if (!is.null(selection)) {
        y <- if (margin == "documents") x[selection, ] else x[, selection]
    } else {
        y <- NULL
    }
    
    m <- if (margin == "documents") 1 else 2
    methods1 <- c("euclidean", "chisquared", "chisquared2", "kullback", "manhattan", "maximum", "canberra")
    methods2 <- c("jaccard", "binary", "ejaccard", "simple matching")
    
    if (method %in% methods1) {
        temp <- get(paste0(method, "_dist"))(x, y, margin = m)
    } else if (method == "minkowski") {
        temp <- get(paste0(method, "_dist"))(x, y, margin = m, p = p)
    } else if (method %in% methods2) {
        if (method == "binary") method <- "jaccard"
        temp <- get(paste0(method, "_simil"))(x, y, margin = m)
    } else {
        stop(method, " is not implemented; consider trying proxy::dist().")
    }
    
    # create a new dist object
    if (is.null(selection)) {
        result <- stats::as.dist(temp, diag = diag, upper = upper)
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        return(result)
    } else {
        result <- as.matrix(temp)
        if(!is.null(rownames(result)))
            attr(result,"Labels") <- rownames(result)
        else if(!is.null(colnames(result)))
            attr(result,"Labels") <- colnames(result)
        attr(result, "Size") <- if (margin == "documents") nrow(result) else ncol(result)
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        class(result) <- c("dist_selection")
        return(result)
    }
}


#' Coerce a dist object into a list
#' 
#' Coerce a dist matrix into a list of selected target terms and similar terms,
#' in descending order of similarity.  Can be used after calling
#' [textstat_simil()] or [textstat_dist()].
#' @param x dist class object
#' @param sorted sort results in descending order if `TRUE`
#' @param n the top `n` highest-ranking items will be returned.  If n is 
#'   `NULL`, return all items.
#' @param ... unused
#' @method as.list dist
#' @export
as.list.dist <- function(x, sorted = TRUE, n = NULL, ...) {
    
    if (!is.null(attr(x, "Labels"))) label <- attr(x, "Labels")
    result <- lapply(seq_len(ncol(as.matrix(x))), function(i) as.matrix(x)[, i])
    attributes(x) <- NULL
    names(result) <- if (!is.null(label)) label
    
    # remove the element of each similarity vector equal to the item itself
    for (m in names(result)) {
        result[[m]] <- result[[m]][m != names(result[[m]])]
    }

    # sort each element of the list and return only first n results if n not NULL
    if (sorted == TRUE)
        result <- lapply(result, sort, decreasing = TRUE, na.last = TRUE)
    
    # truncate to n if n is not NULL
    if (!is.null(n))
        result <- lapply(result, "[", 1:n)
    
    result
    
}

#' Coerce a dist into a dist
#' 
#' Internal function to guarantee that a dist remains a dist, if for some reason
#' a user wants to coerce a dist into a dist.
#' @keywords textstat internal
#' @importFrom stats as.dist
#' @method as.dist dist
#' @export
as.dist.dist <- function(m, diag = FALSE, upper = FALSE) {
    as.dist(as.matrix(m), diag = diag, upper = upper)
}


#' Coerce a dist_selection object into a list
#' 
#' Coerce a dist_selection matrix into a list of selected terms and target terms in
#' descending order.  Can be used after calling [textstat_simil()] or
#' [textstat_dist()] when selection is not NULL
#' @param x dist_selection class object
#' @param sorted sort results in descending order if `TRUE`
#' @param n the top `n` highest-ranking items will be returned.  If n is 
#'   `NULL`, return all items.
#' @param ... unused
#' @method as.list dist_selection
#' @keywords textstat internal
#' @export
as.list.dist_selection <- function(x, sorted = TRUE, n = NULL, ...) {
    
    if (!is.null(attr(x, "Labels"))) label <- attr(x, "Labels")
    result <- lapply(seq_len(ncol(as.matrix(x))), function(i) as.matrix(x)[, i])
    names(result) <- colnames(x)
    attributes(x) <- NULL
    # names(result) <- if (!is.null(label)) label[seq_len(ncol(as.matrix(x)))]

    # remove the element of each similarity vector equal to the item itself
    for (m in names(result)) {
        result[[m]] <- result[[m]][m != names(result[[m]])]
    }
    
    # sort each element of the list and return only first n results if n not NULL
    if (sorted == TRUE)
        result <- lapply(result, sort, decreasing = TRUE, na.last = TRUE)
    
    # truncate to n if n is not NULL
    if (!is.null(n))
        result <- lapply(result, "[", 1:n)
    
    result
}

#' Print a dist_selection object
#' 
#' Print method for a dist_selection object, to make it appear as a data.frame.
#' @export
#' @method print dist_selection
#' @keywords textstat internal
print.dist_selection <- function(x, ...) {
    print(as.matrix(x))
}

#' Coerce a dist_selection object to a matrix
#' 
#' Coerce a dist_selection object to a plain matrix.
#' @export
#' @method as.matrix dist_selection
#' @keywords textstat internal
as.matrix.dist_selection <- function(x, ...) {
    attrs <- attributes(x)
    attrs <- attrs[names(attrs) %in% c("dimnames", "dim")]
    attributes(x) <- attrs
    class(x) <- "matrix"
    x
}


## used Matrix::crossprod and Matrix::tcrossprod for sparse Matrix handling
euclidean_dist <- function(x, y = NULL, margin = 1){
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_sum <- if (margin == 2) colSums else rowSums
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
    n <- if (margin == 2) ncol(x) else nrow(x)
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
        an <- func_sum(x ^ 2)
        bn <- func_sum(y ^ 2)
        
        # number of features
        kk <- y@Dim[margin]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        eucmat <- sqrt(tmp - 2 * as.matrix(func_cp(x, y)))
    } else {
        an <- func_sum(x ^ 2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        eucmat <- sqrt(tmp - 2 * as.matrix(func_cp(x)))
    }
    eucmat
}

# Hamming distance
# formula: hamming = sum(x .!= y)
# hamming_dist <- function(x, y = NULL, margin = 1) {
#     
#     if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
#     
#     # convert to binary matrix
#     x <- dfm_weight(x, "boolean") 
#     x0 <- 1 - x
#     func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
#     func_sum <- if (margin == 2) nrow else ncol
#     func_name <- if (margin == 2) colnames else rownames
#     # union 
#     an <- func_sum(x)
#     if (!is.null(y)) {
#         y <- dfm_weight(y, "boolean")
#         y0 <- 1 - y
#         a <- func_cp(x, y)
#         a0 <- func_cp(x0, y0)
#         colname <- func_name(y)
#     } else {
#         a <- func_cp(x)
#         a0 <- func_cp(x0)
#         colname <- func_name(x)
#     }
#     rowname <- func_name(x)
#     # common values
#     a <- a + a0
#     hammat <- an - a
#     dimnames(hammat) <- list(rowname, colname)
#     hammat
# }

# Chi-squared distance:divide by row sums and square root of column sums, 
# and adjust for square root of matrix total (Legendre & Gallagher 2001, 
# Bruce McCune & James b. Grace 2002). 
# http://adn.biol.umontreal.ca/~numericalecology/Reprints/Legendre_&_Gallagher.pdf
# https://www.pcord.com/book.htm
# formula: Chi = sum((x/rowsum(x_i) - y/rowsum(y_i)) ^ 2/(colsum(i)/total))
chisquared_dist <- function(x, y = NULL, margin = 1){
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_sum <- if (margin == 2) colSums else rowSums
    func_name <- if (margin == 2) colnames else rownames
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    avg <- if (margin == 2) sqrt(rowSums(x) / sum(x)) else sqrt(colSums(x) / sum(x))
    n <- if (margin == 2) ncol(x) else nrow(x)
    rowname <- func_name(x)
    colname <- func_name(x)
    if (margin == 1 ) {
        # convert into profiles
        x <- x/func_sum(x)
        
        # weighted by the average profiles
        x <- x %*% diag(1/avg)
    } else {
        x <- x %*% diag(1/func_sum(x))
        x <- x / avg
    }
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
       colname <- func_name(y)
        # avg is same as that for x 
        if (margin == 1 ) {
            # convert into profiles
            y <- y/ func_sum(y)
            
            # weighted by the average profiles
            y <- y %*% diag(1 / avg)
        } else {
            y <- if (dim(y)[margin] > 1) y %*% diag(1/func_sum(y)) else y %*% (1/func_sum(y))
            y <- y / avg
        }
        an <- func_sum(x ^ 2)
        bn <- func_sum(y ^ 2)
        
        # number of features
        kk <- y@Dim[margin]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp + matrix(rep(bn, n), nrow = n, byrow = TRUE)
        chimat <- tmp - 2 * as.matrix(func_cp(x, y))
        #chimat <-  sqrt(round(chimat, 2)) 
    } else {
        an <- func_sum(x ^ 2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <- tmp + matrix(rep(an, n), nrow = n, byrow = TRUE)
        chimat <- tmp - 2 * as.matrix(func_cp(x))
        #chimat <-  sqrt(round(chimat, 2)) 
    }
    dimnames(chimat) <- list(rowname, colname)
    chimat
}

# This chi-squared method is used for histogram: sum((x-y) ^ 2/((x+y)))/2
# http://www.ariel.ac.il/sites/ofirpele/publications/ECCV2010.pdf
chisquared2_dist <- function(x, y = NULL, margin = 1){
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_sum <- if (margin == 2) colSums else rowSums
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
    n <- if (margin == 2) ncol(x) else nrow(x)
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
        an <- func_sum(x ^ 2)
        bn <- func_sum(y ^ 2)
        
        # number of features
        kk <- y@Dim[1]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        chimat <- sqrt( tmp - 2 * as.matrix(func_cp(x, y)) )
    } else {
        an <- func_sum(x ^ 2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        
        a1 <- func_sum(x)
        sumij <- matrix(rep(a1, n), nrow = n) + matrix(rep(a1, n), nrow = n, byrow=TRUE)
        
        chimat <- (tmp - 2 * as.matrix(func_cp(x))) / sumij / 2
    }
    chimat
}

# Kullback-Leibler divergence: is a measure of the difference between probability distributions
# This metric is not symmetric, it is better applied with setting of "dist = FALSE" and "tri= FALSE"
# to avoid eoercing the result to a dist object.
# assumption: p(x_i) = 0 implies p(y_i)=0 and in case both p(x_i) and p(y_i) equals to zero, 
# p(x_i)*log(p(x_i)/p(y_i)) is assumed to be zero as the limit value.
# formula: sum(p(x)*log(p(x)/p(y)))
kullback_dist <- function(x, y = NULL, margin = 1) {
    
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    func_cp <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    func_sum <- if (margin == 2) colSums else rowSums
    func_name <- if (margin == 2) colnames else rownames
    
    # probability
    x <- x / func_sum(x)
    logx <- log(x)
    logx[is.na(logx)] <- 0L
    logx[is.infinite(logx)] <- 0L
    if (!is.null(y)) {
        y <- y / func_sum(y)
        logy <- log(y)
        logy[is.na(logy)] <- 0L
        logy[is.infinite(logy)] <- 0L
        kullmat <- func_sum(x*logx) - func_cp(x, logy)
       colname <- func_name(y)
    } else {
        kullmat <- func_sum(x*logx) - func_cp(x, logx)
        # cat("func_sum(x*logx)\n")
        # print(func_sum(x*logx))
        # cat("func_cp(x, logx)\n")
        # print(func_cp(x, logx))
        colname <- func_name(x)
    }
    rowname <- func_name(x)
    dimnames(kullmat) <- list(rowname, colname)
    kullmat
}

# Manhattan distance: sum_i |x_i - y_i|
manhattan_dist <- function(x, y=NULL, margin = 1){
    
    func_name <- if (margin == 2) colnames else rownames
    
    if (!is.null(y)) {
       colname <- func_name(y)
        manmat <- qatd_cpp_manhattan2(x, y, margin)
    } else {
       colname <- func_name(x)
        manmat <- qatd_cpp_manhattan(x, margin)
    }
    dimnames(manmat) <- list(func_name(x), colname)
    manmat
}

# Maximum/Supremum distance: max_i |x_i - y_i|
maximum_dist <- function(x, y = NULL, margin = 1){
    
    func_name <- if (margin == 2) colnames else rownames
    
    if (!is.null(y)) {
       colname <- func_name(y)
        maxmat <- qatd_cpp_maximum2(x, y, margin)
    } else {
       colname <- func_name(x)
        maxmat <- qatd_cpp_maximum(x, margin)
    }
    dimnames(maxmat) <- list(func_name(x), colname)
    maxmat
}

# Canberra distance: sum_i |x_i - y_i| / |x_i + y_i|
# Weighted by num_nonzeros_elementsum/num_element
canberra_dist <- function(x, y = NULL, margin = 1){
    
    func_name <- if (margin == 2) colnames else rownames
    
    if (!is.null(y)) {
       colname <- func_name(y)
        canmat <- qatd_cpp_canberra2(x, y, margin)
    } else {
       colname <- func_name(x)
        canmat <- qatd_cpp_canberra(x, margin)
    }
    dimnames(canmat) <- list(func_name(x), colname)
    canmat
}

# Minkowski distance: (sum_i (x_i - y_i)^p)^(1/p)
minkowski_dist <- function(x, y = NULL, margin = 1, p = 2){
    
    func_name <- if (margin == 2) colnames else rownames
    
    if (!is.null(y)) {
       colname <- func_name(y)
        minkmat <- qatd_cpp_minkowski2(x, y, margin, p)
    } else {
       colname <- func_name(x)
        minkmat <- qatd_cpp_minkowski(x, margin, p)
    }
    dimnames(minkmat) <- list(func_name(x), colname)
    minkmat
}
