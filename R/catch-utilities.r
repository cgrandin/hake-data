#' Colors for the Hake Assessment
#'
#' Generate the color scheme for the hake assessment
#' such that as more years of historical data are added on
#' that the more recent years are still the same color.
#' For example, the most recent year of data will always be
#' black and the second most recent year will always be red.
#'
#' @param n The default number of colors to generate is 10
#'
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @author Kelli Faye Johnson
#'
#' @return A vector of colours
#'
#' @examples
#' n <-18
#' plot(data.frame(1:n, 1), col= plotcolour(n), pch = 19, cex = 5)
#'
plotcolour <- function(n.cols = 9) {

  base <- RColorBrewer::brewer.pal(name = "Set1", n = n.cols)
  colors <- c(base[(n.cols - 1):1], "#000000")

  if(n.cols > 9 && n.cols < 17) {
    extra <- RColorBrewer::brewer.pal(name = "Set2", n = 7)
    colors <- c(extra[(n.cols - 10):1], rev(base), "#000000")
  }

  if(n.cols >= 17){
    stop(n.cols, " is too many colors, only 16 are allowed")
  }

  colors
}

