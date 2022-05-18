#' Modified Kemeny (Hidden from User)
#' @description \emph{Modified Kemeny} algorithm determines the consensus ranking using
#' the set of all possible rankings compared to the input rankings. This algorithm
#' based on minimizing the combined Kemeny distance compared to the input rankings;
#' however, see 'Details' for additional criteria to determine the consensus
#' ranking if multiple rankings have the minimum total Kemeny distance.
#'
#' @details If multiple universe rankings have the minimum total Kemeny distance,
#' then the smallest maximum Kemeny distance is considered. If there are multiple
#' rankings remaining, the ranking(s) with the smallest variance are kept. Finally,
#' if multiple rankings exist after filtering these conditions, then the first
#' ranking is chosen as the single consensus ranking.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each row is a complete ranking.
#'
#' @param universe_rkgs a matrix containing all possible permutations of ranking
#' n objects. Each row in this matrix represents one permuted ranking.
#'
#' @param obj_pairs a \code{2} by \code{n choose 2} matrix of all combinations of
#' object pairs of n objects, where each column contains a pair of object indices.
#'
#' @return A matrix containing the consensus ranking, total Kemeny distance, and
#' average tau correlation coefficient.

# Kemeny distances
mod_kemeny_int <- function(input_rkgs, universe_rkgs, obj_pairs) {
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]

  tot_kem_dists <- numeric(dim(universe_rkgs)[1])
  max_kem_dists <- numeric(dim(universe_rkgs)[1])
  var_kem_dists <- numeric(dim(universe_rkgs)[1])

  # Get the total Kemeny distances, max Kemeny distances and variances for each
  # of the universe rankings and input rankings
  kem_dists <- score_all_totalK(universe_rkgs, input_rkgs, obj_pairs)
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
  avg_tau <- compute_avg_tau(totK, n, k)

  results <- matrix(c(consensus,totK, avg_tau), nrow = 1)
  return(results)
}
