#' Simulated 50  \eqn{\times}{ X }  15 Data
#'
#' Data of 50 objects and 15 attributes, which were randomly generated from the
#' 100  \eqn{\times}{ X }  15 simulated dataset (see \code{\link{data100x15}}). The first column contains the object
#' names and each subsequent column is a complete ranking of the 50 objects.
#'
#' @docType data
#'
#' @usage data(data50x15)
#'
#' @format A data frame with 50 rows and 16 columns:
#' \describe{
#'   \item{Object}{object name}
#'   \item{Ranking 1}{ranking on the first attribute}
#'   \item{Ranking 2}{ranking on the second attribute}
#'   \item{Ranking 3}{ranking on the third attribute}
#'   \item{Ranking 4}{ranking on the fourth attribute}
#'   \item{Ranking 5}{ranking on the fifth attribute}
#'   \item{Ranking 6}{ranking on the sixth attribute}
#'   \item{Ranking 7}{ranking on the seventh attribute}
#'   \item{Ranking 8}{ranking on the eigth attribute}
#'   \item{Ranking 9}{ranking on the ninth attribute}
#'   \item{Ranking 10}{ranking on the tenth attribute}
#'   \item{Ranking 11}{ranking on the eleventh attribute}
#'   \item{Ranking 12}{ranking on the twelfth attribute}
#'   \item{Ranking 13}{ranking on the thirteenth attribute}
#'   \item{Ranking 14}{ranking on the fourteenth attribute}
#'   \item{Ranking 15}{ranking on the fifteenth attribute}
#' }
#'
#' @keywords datasets
#'
#' @source Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \doi{10.1016/j.cor.2018.06.007}
#'
#' @examples
#' data(data50x15)
#' input_rkgs <- t(as.matrix(data50x15[, -1]))
#' obj_names <- data50x15[,1]
#'
#' # Determine the mean seed ranking
#' mean_seed(input_rkgs)

"data50x15"
