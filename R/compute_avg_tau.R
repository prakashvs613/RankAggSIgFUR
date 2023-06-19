#' Compute Average tau
#'
#' @description Calculates the average tau correlation coefficient defined by Emond and Mason (2002).
#' The average tau correlation has a one-to-one relationship with total Kemeny distance,
#' While it is more common, total Kemeny distance may be preferred to avoid rounding error.
#'
#' @param total_K a positive integer as the total Kemeny distance of a ranking versus
#' the input rankings.
#'
#' @param n a positive integer for the number of objects in the ranking.
#'
#' @param k a positive integer for the number of judges or attributes in the given
#' ranking problem. This is also the number of input rankings from the problem.
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'  
#'
#' @references
#' Emond, E. J., & Mason, D. W. (2002). A new rank correlation coefficient with
#' application to the consensus ranking problem. Journal of Multi-Criteria Decision
#' Analysis, 11(1), 17-28.
#'
#' @return A numeric of the average tau correlation based on the total Kemeny distance.
#'
#' @keywords internal
#'
#' @export

# Average tau correlation coefficient
compute_avg_tau <- function(total_K, n, k, wt){
  # Assign equal weights if none are given 
  1-((2*total_K)/(n*(n-1)*sum(wt)))
}
