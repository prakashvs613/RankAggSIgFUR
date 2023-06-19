#' Modified Kemeny Rank Aggregation
#'
#' @description \emph{Modified Kemeny} algorithm determines the consensus ranking of \code{n} objects using
#' the set of all possible rankings compared to the input rankings. The algorithm is based on
#' Kemeny's axiomatic approach of minimizing the total Kemeny distance from the input rankings.
#' In case of multiple rankings with minimum total Kemeny distance, the consensus ranking is
#' determined using two additional criteria. See `Details' for additional criteria.
#' The method involves \code{n}! comparisons. Hence, it works best on a set of rankings with a small
#' number of objects.
#'
#' @details Under Kemeny's axiomatic approach, rankings with minimum total Kemeny distance are
#' considered equally optimal. Modified Kemeny attempts to break the tie among such rankings by
#' imposing two additional criteria on the basis of minimizing (a) the maximum and (b) the variance
#' of individual Kemeny distances, applied sequentially.
#'
#' @param input_rkgs a \code{k} by \code{n} matrix of \code{k} rankings of \code{n} 
#' objects, where each row is a complete ranking. Note that this is a transpose of 
#' matrix used for functions like \code{fur}, \code{sigfur}, \code{rap_greedy_alg}, 
#' and \code{subit_convergence}.
#'
#' @param universe_rkgs a matrix containing all possible permutations of ranking
#' n objects. Each row in this matrix represents one permuted ranking.
#'
#' @param obj_pairs a \code{2} by \code{n choose 2} matrix of all combinations of
#' object pairs of n objects, where each column contains a pair of object indices.
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'
#' @return A list containing the consensus ranking (expressed as ordering), total Kemeny distance, and average
#' tau correlation coefficient corresponding to the consensus ranking.
#'
#' @references Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \doi{10.1016/j.cor.2018.06.007}
#'
#' @examples
#' ## Consensus ranking from four rankings of five objects
#' n <- 5
#' input_rkgs <- matrix(c(3, 2, 5, 1, 2, 3, 1, 2, 5, 1, 3, 4, 4, 5, 4, 5, 1, 4, 2, 3), ncol = n)
#' uni_rkgs <- matrix(unlist(combinat::permn(c(1:n))), byrow = TRUE, ncol = n)
#' obj_pairs <- combinat::combn(1:n,2, simplify=TRUE)
#' wt <- rep(1,nrow(input_rkgs))
#' mod_kemeny(input_rkgs, uni_rkgs, obj_pairs,wt=wt) # Computed consensus ranking, 
#'                                                   # total Kemeny distance,
#'                                             #and average tau correlation coefficient
#'
#' @export
#'
# Kemeny distances
mod_kemeny <- function(input_rkgs, universe_rkgs, obj_pairs,wt) {
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]

  tot_kem_dists <- numeric(dim(universe_rkgs)[1])
  max_kem_dists <- numeric(dim(universe_rkgs)[1])
  var_kem_dists <- numeric(dim(universe_rkgs)[1])

  # Get the total Kemeny distances, max Kemeny distances and variances for each
  # of the universe rankings and input rankings
  kem_dists <- totalKem_mult(universe_rkgs, input_rkgs, obj_pairs,wt=wt)
  tot_kem_dists <- kem_dists[,1]
  max_kem_dists <- kem_dists[,2]
  var_kem_dists <- kem_dists[,3]

  # Determine the optimal ranking based on modified kemeny criteria
  # Find the ranking with the smallest total Kemeny distance
  i_min_totK <- which(tot_kem_dists == min(tot_kem_dists))

  if (length(i_min_totK) == 1) {
    final_index <- i_min_totK
  }

  # If multiple, then find the one with the smallest max Kemeny distance
  if (length(i_min_totK) > 1) {
    final_max_kem_dists <- max_kem_dists[i_min_totK]
    i_min_totK_max <- which(final_max_kem_dists == min(final_max_kem_dists))
    i_min_totK <- i_min_totK[i_min_totK_max]
  }

  # If multiple, find the one with the smallest variance
  if (length(i_min_totK) > 1) {
    final_var_kem_dists <- var_kem_dists[i_min_totK]
    i_min_totK_var <- which(final_var_kem_dists == min(final_var_kem_dists))
    i_min_totK <- i_min_totK[i_min_totK_var]
  }

  # else pick the first one
  if (length(i_min_totK) > 1) {
    i_min_totK <- i_min_totK[1] #picking the first, if multiple
  }

  consensus <- universe_rkgs[i_min_totK, , drop=F]
  totK <- tot_kem_dists[i_min_totK]
  avg_tau <- compute_avg_tau(totK, n, k, wt = wt)

  results <- matrix(c(consensus,totK, avg_tau), nrow = 1)
  return(list(ConsensusRanking = consensus, KemenyDistance = totK,
              tau = avg_tau))
}
