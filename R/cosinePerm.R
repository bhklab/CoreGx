#' Computes the cosine similarity and significance using permutation test
#' 
#' Please note: to ensure reproducibility of this functions results
#'   remember to set.seed() in your script before running it. We 
#'   recommend writing down the number you use as a set.seed() argument
#'   to ensure you can reproduce your results in the future.
#' 
#' @examples
#' x <- factor(c(1,2,1,2,1))
#' y <- factor(c(2,2,1,1,1))
#' cosinePerm(x, y)
#' 
#' @param x [factor] is the factors for the first variable
#' @param y [factor] is the factors for the second variable
#' @param nperm [integer] is the number of permutations to comput ethe null 
#'   distribution of MCC estimates
#' @param alternative [string] indicates the alternative hypothesis and must be 
#'   one of ‘"two.sided"’, ‘"greater"’ or ‘"less"’.  You can specify just
#' the initial letter.  ‘"greater"’ corresponds to positive
#' association, ‘"less"’ to negative association.
#' Options are "two.sided", "less", or "greater"
#' @param include.perm [boolean] indicates whether the estimates for the null 
#'   distribution should be returned. Default set to 'FALSE'
#' @param nthread [integer] is the number of threads to be used to perform the 
#'   permutations in parallel
#' @return [list] estimate of the cosine similarity, p-value and estimates after
#'   random permutations (null distribution) in include.perm is set to 'TRUE'
#' @importFrom lsa cosine
#' @import parallel 
#' @export

cosinePerm <- function(x, y, 
                       nperm=1000, 
                       alternative=c("two.sided", "less", "greater"), 
                       include.perm=FALSE, 
                       nthread=1
                       )
{
  alternative <- match.arg(alternative)
  if ((length(x) != length(y))) { stop("x and y must be vectors of the same length") }
  res <- c("estimate"=NA, "p.value"=NA)
  x <- as.numeric(x)
  y <- as.numeric(y)
  ## compute cosine
  res["estimate"] <- drop(lsa::cosine(x=x, y=y))
  ## compute significance of cosine using a permutation test
  if (nperm > 0) {
    splitix <- parallel::splitIndices(nx=nperm, ncl=nthread)
    splitix <- splitix[vapply(splitix, length, FUN.VALUE=numeric(1)) > 0]
    mcres <- parallel::mclapply(splitix, function(x, xx, yy) {
      res <- vapply(x, function(x, xx, yy) {
        xx <- sample(xx)
        yy <- sample(yy)
        return (drop(lsa::cosine(x=xx, y=yy)))
      }, xx=xx, yy=yy, FUN.VALUE=numeric(1))
      return (res)
    }, xx=x, yy=y)
    mcres <- unlist(mcres)
    switch(alternative,
      "two.sided" = {
        res["p.value"] <- 2 * (min(sum(mcres < res["estimate"]), sum(mcres > res["estimate"])) / sum(!is.na(mcres)))
      },
      "less" = {
        res["p.value"] <- sum(mcres < res["estimate"]) / sum(!is.na(mcres))
      },
      "greater" = {
        res["p.value"] <- sum(mcres > res["estimate"]) / sum(!is.na(mcres))
      }
    )
    if (res["p.value"] == 0) { res["p.value"] <- 1 / (nperm + 1) }
  }
  res <- as.list(res)
  if (include.perm) {
    res <- c(res, list("estimate.random"=mcres))
  }
  return (res)
}
