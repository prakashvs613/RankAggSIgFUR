#' Seed-Based Iteration
#'
#' @description \emph{Seed-Based Iteration} is a heuristic-based seed generation used in \emph{SIgFUR}
#' to iteratively perturb the ranking to improve the consensus ranking.
#'
#' @param eta a subiteration length for intermittent \emph{Subiterative Convergence}. The recommended
#' values are between 2 and 8. Smaller subiteration lengths result in shorter run-time.
#'
#' @param omega a positive integer for the number of repetitions of perturbing
#' the seed ranking. An \code{omega} value of 1 corresponds to a single application of
#' \emph{Subiterative Convergence}.
#'
#' @param input_rkgs a \code{k} by \code{n} matrix of \code{k} rankings of \code{n} 
#' objects, where each row is a complete ranking. Note that this is a transpose of 
#' matrix used for functions like \code{fur}, \code{sigfur}, \code{rap_greedy_alg}, 
#' and \code{subit_convergence}.
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'
#' @return A list containing the consensus ranking (expressed as ordering) and total
#'  Kemeny distance corresponding to the consensus ranking.
#'
#' @references Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \doi{10.1016/j.cor.2018.06.007}
#'
#' @seealso \code{\link{sigfur}}, \code{\link{subit_convergence}}, \code{\link{mean_seed}}
#'
#' @examples
#' ## Four input rankings of five objects
#' eta <- 2
#' omega <- 10
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' seed_based_iteration(eta, omega, t(input_rkgs)) # Determined seed-based iterations
#'
#' ## Five input rankings with five objects
#' ## 2nd ranking == 3rd ranking, so if a third object is weighted as zero,
#' ## we should get the same answer as the first examples
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' eta <- 2
#' omega <- 10
#' wt = c(1,1,0,1,1)
#' seed_based_iteration(eta, omega, t(input_rkgs), wt=wt) # Determined seed-based iterations
#'
#' ## Included dataset of 15 input rankings of 50 objects
#' eta <- 3
#' omega <- 5
#' data(data50x15)
#' input_rkgs <- as.matrix(data50x15[, -1])
#' seed_based_iteration(eta, omega, t(input_rkgs)) # Determined seed-based iterations
#'
#' @export

seed_based_iteration <- function(eta, omega, input_rkgs,wt=c()) {


  
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]
  
  if (length(wt) == 0) {
    wt <- rep(1,k)
  } 
  
  pairs <- combinat::combn(1:n, 2, simplify = T)
  univ_rkgs <- matrix(unlist(combinat::permn(c(1:eta))), byrow = T, ncol = eta)
  input_rkg_list <- matrix(0, nrow = omega, ncol = n)
  output_rkg_list <- matrix(0, nrow = omega, ncol = n)

  # Find the mean seed ranking and use it as an input to Subiterative Convergence
  seed_rkg <- matrix(mean_seed(input_rkgs,wt=wt), nrow = 1)
  out_rkg <- subit_convergence(eta, seed_rkg, t(input_rkgs), univ_rkgs,wt=wt)$ConsensusRanking
  outinit_rkg <- out_rkg
	
  # Add the seed ranking to the list of input rankings and the output from
  # Subiterative Convergence to the list of output rankings
  input_rkg_list[1, ] <- seed_rkg
  output_rkg_list[1, ] <- out_rkg

  # If the number of replications is 2 or more, divide the current output ranking
  # in half and flip the halves. The reordered new ranking will be used as the input
  # (seed) ranking to the next call of Subiterative Convergence
  if(omega >= 2){
    out_ordg <- order(out_rkg)
    new_input_ordg <- c(out_ordg[floor(n/2):1], out_ordg[n:(floor(n/2)+1)])
    new_input_rkg <- order(new_input_ordg)
    new_out_rkg <- subit_convergence(eta, new_input_rkg, t(input_rkgs), univ_rkgs,wt=wt)$ConsensusRanking

    # Update input and output ranking lists
    input_rkg_list[2,] <- new_input_rkg
    output_rkg_list[2,] <- new_out_rkg
  }
	# If the number of replications is 3 or more, find the mean seed ranking of the
  # the initial seed ranking and the previous input ranking
  if(omega >= 3){
    for(loopomega in 3:omega){
	  
	    gamma_loopomega <- mean_seed(rbind(output_rkg_list[1, ], output_rkg_list[loopomega-1, ]))
     
	 # Determine if the new input ranking is in the input ranking list
      if (length(which(duplicated(rbind(gamma_loopomega, input_rkg_list[1:(loopomega-1), ]))[-1] == T)) >= 1){

        # The mean seed ranking is in the input ranking list, flip two halves of the
        # previous output ranking
        gamma_ordg <- order(output_rkg_list[loopomega-1, ])
        new_input_ordg <- c(gamma_ordg[floor(n/2):1], gamma_ordg[n:(floor(n/2)+1)])
        new_input_rkg <- order(new_input_ordg)

      } else{
        # The new input ranking, gamma_loopomea, was not found in the input ranking
        # list, and so it becomes the new input ranking to the next call to
        # Subiterative Convergence
        new_input_rkg <- gamma_loopomega
      }
    
      # Apply Subiterative Convergence and update the input and output rankin lists
      new_out_rkg <- subit_convergence(eta, new_input_rkg, t(input_rkgs), univ_rkgs,wt=wt)$ConsensusRanking
      input_rkg_list[loopomega, ] <- new_input_rkg
      output_rkg_list[loopomega, ] <- new_out_rkg
    }
  }

  # Find the index of ranking with smallest Kemeny distance among the output
  # ranking list
  all_K <- totalKem_mult(output_rkg_list, input_rkgs,pairs,wt=wt)[,1]
  optimal_K <- which.min(all_K)
  results <- matrix(0, nrow = 1, ncol = n+1)
  results[1:n] <- output_rkg_list[optimal_K, ]
  results[n+1] <- all_K[optimal_K]
  return(list(ConsensusRanking = results[1:n], KemenyDistance = results[n+1]))
}
