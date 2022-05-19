#' PrefLib 240  \eqn{\times}{ X } 4 Data
#'
#' Data of 240 cities across the globe ranked on four criteria from the
#' ED-00015-001.soc dataset in the PrefLib repository. The first column contains
#' the object names and each subsequent column is a complete ranking of the 240
#' objects with no ties)
#' .
#'
#' @docType data
#'
#' @usage data(data240x4)
#'
#' @format A data frame with 240 rows and 5 columns:
#' \describe{
#'   \item{Object}{object name}
#'   \item{Ranking 1}{ranking on the first criterion}
#'   \item{Ranking 2}{ranking on the second criterion}
#'   \item{Ranking 3}{ranking on the third criterion}
#'   \item{Ranking 4}{ranking on the fourth criterion}
#' }
#'
#' @keywords datasets
#'
#' @references Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \doi{10.1016/j.cor.2018.06.007}
#'
#' @references Mattei, N., & Walsh, T. (2013, November). Preflib: A library for
#' preferences \url{https://www.preflib.org/}. In International conference on algorithmic
#' decision theory (pp. 259-270). Springer, Berlin, Heidelberg.
#'
#' @source \url{https://www.preflib.org/}
#'
#' @examples
#' data(data240x4)
#' input_rkgs <- t(as.matrix(data240x4[, -1]))
#' obj_names <- data240x4[,1]
#'
#' # Determine the mean seed ranking
#' mean_seed(input_rkgs)

"data240x4"
