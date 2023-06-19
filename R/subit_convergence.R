#' Subiterative Convergence
#'
#' @description \emph{Subiterative Convergence} finds the consensus ranking by iteratively applying the
#' \emph{Modified Kemeny} algorithm on smaller number of objects, \eqn{\eta}{eta}. Starting with a given seed
#' ranking, the consensus ranking is obtained when the algorithm converges.
#'
#' @param eta a subiteration length of number of objects to consider in the smaller
#' subset. Recommended \code{eta} values are between 2 and 8. Smaller \code{eta} values result in shorter run-time.
#'
#' @param seed_rkg an initial ranking to start the algorithm. An ideal seed ranking for
#' \emph{Subiterative Convergence} is the \emph{mean seed ranking} of input rankings.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param universe_rkgs a matrix containing all possible permutations of ranking
#' \code{n} objects. Each column in this matrix represents one permuted ranking.
#' An optional parameter. 
#' 
#' @param objNames a \code{n}-length vector containing object names. An optional 
#'  parameter. 
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
#' @seealso \code{\link{mod_kemeny}}, \code{\link{fur}}, \code{\link{sigfur}}, \code{\link{mean_seed}}
#'
#' @examples
#' ## Four input rankings of five objects
#' eta <- 3
#' seed_rkg <- c(1, 2, 3, 4, 5)
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' subit_convergence(eta, seed_rkg, input_rkgs) # Determined the consensus ranking, total Kemeny
#'                                              # distance, and average tau correlation coefficient
#'
#' ## Example with eta=1
#' eta <- 1
#' seed_rkg <- c(1, 2, 3, 4, 5)
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' subit_convergence(eta, seed_rkg, input_rkgs) # Shows a warning and returns seed ranking
#'
#' ## Five input rankings with five objects
#' ## 2nd ranking == 3rd ranking, so if a third object is weighted as zero,
#' ## we should get the same answer as the first examples
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' eta <- 3
#' seed_rkg <- c(1, 2, 3, 4, 5)
#' wt = c(1,1,0,1,1)
#' subit_convergence(eta, seed_rkg, input_rkgs, wt=wt) # Determined the consensus ranking, total Kemeny
#'                                              # distance, and average tau correlation coefficient
#'
#' ## Using five input rankings with five objects with prepare_data to 
#' ## automatically prepare the weight vector
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' out = prepare_data(input_rkgs) 
#' input_rkgs = out$input_rkgs
#' wt = out$wt
#' eta <- 3
#' seed_rkg <- c(1, 2, 3, 4, 5)
#' subit_convergence(eta, seed_rkg, input_rkgs, wt=wt) # Determined the consensus ranking, total Kemeny
#'                                              # distance, and average tau correlation coefficient
#'
#' ## Included dataset of 15 input rankings of 50 objects
#' data(data50x15)
#' input_rkgs <- as.matrix(data50x15[, -1])
#' mean_seed_rkg <- mean_seed(t(input_rkgs)) # Use the mean seed ranking as the seed ranking
#' eta <- 2
#' subit_convergence(eta, seed_rkg = mean_seed_rkg, input_rkgs)
#'
#' @export

subit_convergence <- function(eta , seed_rkg, input_rkgs, universe_rkgs = c(), objNames = c(),wt = c()){


  input_rkgs <- t(input_rkgs)
  in_eq_out <- F
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]
  
  if (length(wt) == 0) {
    wt <- rep(1,k)
  } 
  
  itermax <- 200 # maximum number of iterations for cyclic convergence
  out_rkgs <- matrix(nrow = itermax, ncol = n)
  if (n == 2){
    obj_pairs_full <- matrix(c(1,2), ncol = 1)
  } else{
    obj_pairs_full <- combinat::combn(1:n, 2, simplify = T)}

  if (!all(Rfast::rowMaxs(input_rkgs,value=T) == n)){
    stop("Seems like an incomplete ranking")     
  } 
  # subiteration length is not at least 2
  if (eta == 1) {
    warning("Caution: eta=1 has no effect on subiteration converence, so the intial starting ranking is returned.")
    out_rkg <- seed_rkg
    totK <- totalKem_mult(matrix(out_rkg,nrow=1), input_rkgs, obj_pairs_full,wt=wt)[1]

  } else if (eta >= 10 & n >= eta) {
    stop("Caution: eta=10 is too large for me to handle. Please use a smaller eta")     
  } else if (eta >= 10 & n < eta) {
    warning("Caution: eta is too large for me to handle and number of objects is smaller than eta.") 
    warning("So, I am returning the best ranking according to eta = n") 
    obj_pairs <- combinat::combn(1:n, 2, simplify = T)
    
    # Creating universe of rankings for eta = n if not provided already
    if (length(universe_rkgs) == 0) {
      universe_rkgs <- matrix(unlist(combinat::permn(c(1:n))), byrow = T, ncol = n)
    }
    
    out_rkgFull <- mod_kemeny(input_rkgs, universe_rkgs, obj_pairs,wt=wt)
    out_rkg <- out_rkgFull$ConsensusRanking
    totK <- totalKem_mult(matrix(out_rkg,nrow=1), input_rkgs, obj_pairs_full,wt=wt)[1]
  } else{
    
    # Creating universe of rankings if not provided already
    if (length(universe_rkgs) == 0) {
      universe_rkgs <- matrix(unlist(combinat::permn(c(1:eta))), byrow = T, ncol = eta)
    }

    # Create object pairs
    if (eta == 2 | n == 2){
      obj_pairs <- matrix(c(1,2), ncol = 1)
    } else{
      obj_pairs <- combinat::combn(1:eta, 2, simplify = T)}

    # Iterations
    iter <- 1
    while (!in_eq_out) {
      out_rkg <- matrix(rep(0, n), ncol = n)

      # Gets the object index of the top eta ranked objects
      objs_inds <- which(seed_rkg %in% c(1:eta))

      # Get the ordering of these object indices
      ord <- seed_rkg[(seed_rkg %in% c(1:eta))]
      objs <- objs_inds[order(ord)]


      # Subiterations
      for (i in 1:(n - eta + 1)) {
        # Get subset of rankings for the current eta-th objects
        sub_rkgs = Rfast::rowRanks(input_rkgs[,objs], method = "first")
        # Get the optimal kemeny ranking of the subset
       
        mod_out1 <- mod_kemeny(sub_rkgs, universe_rkgs, obj_pairs,wt=wt)
      
        mod_out <- matrix(unlist(mod_out1), nrow = 1, byrow = T)
        # Determines the index of ranking in ascending order
        inds <- unlist(sapply(c(1:eta), get_indices, y = mod_out[, 1:eta]))

        # Finds the object index based on inds
        out_inds <- objs[inds]

        # If on the last subiteration, add the remaining object rankings
        if (i == (n - eta + 1)){
          out_rkg[out_inds] <- c(1:eta) + (n - eta)

          # Otherwise add the top ranked object to the output ranking, and
          # continue onto the next subiteration
        } else {
          out_rkg[out_inds[1]] <- i

          # remove the top-ranked obj from the list and add the next one
          obj_ind <- which(seed_rkg %in% (eta + i))
          ord <- seed_rkg[(seed_rkg %in% (eta + i))]
          objs <- obj_ind[order(ord)]
          objs <- c(out_inds[-1], objs)
        }
      }

      # Add output ranking to all output rankings
      out_rkgs[iter, ] <-  out_rkg

      # Extract the rankings from out_rkgs which may have extra NA's
      only_rkgs <- out_rkgs[!is.na(out_rkgs[,1]), , drop = F]

      # Check if any of the output rankings are the same
      if (dim(only_rkgs)[1] != dim(unique(only_rkgs))[1]) {
        in_eq_out <- T # Exit while loop
      }else{
        seed_rkg <- out_rkg # Continue to next iteration with the new input ranking
      }

      # If the output ranking does not converge within 200 iterations, then the
      # consensus ranking will be selected among the output rankings from those 200
      # iterations
      iter <- iter + 1
      if (iter > itermax){
        break;
      }
    }

    #if not converged in 200 iterations, determine the consensus ranking
    # among the 200 output rankings
    if(in_eq_out == F & iter > itermax) {
	  totKAll <- totalKem_mult(out_rkgs, input_rkgs, obj_pairs_full,wt=wt)[,1]
      ind_out <- which.min(totKAll)
      out_rkg <- out_rkgs[ind_out, ]

      # Get the total Kemeny distance of the converged output ranking
    } else {
	  totK <- totalKem_mult(matrix(out_rkg,nrow=1), input_rkgs, obj_pairs_full,wt=wt)[1]
    }
  }
  



  avg_tau <- compute_avg_tau(totK, n, k, wt = wt)
  results <- matrix(c(out_rkg, totK, avg_tau), nrow = 1)
  
  # Naming the objects if the corresponding information is provided
  if (length(objNames) != 0) {
    out_rkg = objNames[c(out_rkg)]
  }
  
  return(list(ConsensusRanking = c(out_rkg), KemenyDistance = totK,
              tau = avg_tau))
}

