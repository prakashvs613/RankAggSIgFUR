#' SIgFUR
#'
#' @description \emph{SIgFUR} applies \emph{Seed-Based Iteration}, \emph{Greedy Algorithm},
#' and \emph{FUR} in sequence for each element of \code{subit_len_list_sbi}. The
#' \emph{mean seed ranking} is used as the input to \emph{Seed-Based Iteration}. 
#' The best of all output rankings from \emph{FUR} is considered as the consensus
#' ranking.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param subit_len_list_sbi a vector containing positive integer(s) for the subiteration
#' lengths to \emph{Seed-Based Iteration}. Recommended values are between 2 and 8. 
#' Smaller subiteration lengths result in shorter run-time.
#'
#' @param omega_sbi a positive integer for the number of repetitions of perturbing
#' the seed ranking in \emph{Seed-Based Iteration}. An \code{omega_sbi} value of 1 
#' corresponds to a single application of \emph{Subiterative Convergence}.
#'
#' @param subit_len_list_fur a vector containing positive integer(s) for the subiteration
#' lengths to \emph{FUR}.
#'
#' @param search_radius a positive integer for the maximum change in the rank of each 
#' object in the \emph{Greedy Algorithm} and \emph{FUR}. The default value
#' of \code{0} considers all possible rank changes for each object. It is
#' recommended to use a search radius of less than or equal to $\min(30, \lfloor n/2 \rfloor)$.
#'
#' @return A matrix containing the consensus ranking, total Kemeny distance, average
#' tau correlation coefficient.
#'
#' @references Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \url{https://doi.org/10.1016/j.cor.2018.06.007}
#'
#' @seealso \code{\link{seed_based_iteration}}, \code{\link{rap_greedy_alg}}, \code{\link{fur}}
#'
#' @examples
#' ## Four input rankings of five objects
#' input_rkgs <- matrix(c(3, 2, 5, 1, 2, 3, 1, 2, 5, 1, 3, 4, 4, 5, 4, 5, 1, 4, 2, 3), ncol = 5)
#' subit_len_list_sbi <- c(2:3)
#' omega_sbi <- 10
#' subit_len_list_fur <- c(2:3)
#' search_radius <- 1
#' sigfur(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius)
#' # Determined the consensus ranking, total Kemeny distance, and average tau correlation coefficient
#'
#' ## Included dataset of 15 input rankings of 50 objects
#' data(data50x15)
#' input_rkgs <- t(as.matrix(data50x15[, -1]))
#' subit_len_list_sbi <- c(6)
#' omega_sbi <- 10
#' subit_len_list_fur <- c(3:5)
#' search_radius <- 1
#' sigfur(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius)
#'
#' @export

sigfur <- function(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius) {
  I <- length(subit_len_list_sbi)
  i <- 1
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]
  results_fur <- matrix(0, nrow = (I), ncol = n+1)

  # For each subiteration length in subit_len_list_sbi
  for (i in 1:I){
    # Do seed based iteration
    out_sbi <- seed_based_iteration(subit_len_list_sbi[i], omega_sbi, input_rkgs)
    out_sbi_rkg <- out_sbi[1:n]
    out_sbitotK <- out_sbi[n+1]

    # Apply greedy algorithm using the output from seed based iteration
    out_ga <- rap_greedy_alg(out_sbi_rkg, input_rkgs, search_radius)
    out_rkg <- out_ga[1:n]
    out_totK <- out_ga[n+1]

    # Do FUR with the initial seed ranking of the output from greedy
    out_fur <- fur(input_rkgs, subit_len_list_fur, search_radius, out_rkg)
    results_fur[i, 1:n] <- out_fur[1:n]
    results_fur[i, n+1] <- out_fur[n+1]
  }

  # Return the optimal ranking from the results from FUR
  min_totK <- as.integer(min(results_fur[, n+1]))
  out_rkg <- results_fur[which(results_fur[, n+1] == min_totK)[1], 1:n]

  # Find avg tau and elapsed time
  avg_tau <- compute_avg_tau(min_totK, n, k)
  results <- matrix(c(out_rkg, min_totK, avg_tau), nrow = 1)
  return(results)
}
