#' Colors for the Hake Assessment
#'
#' Generate the color scheme for the hake assessment
#' such that as more years of historical data are added on
#' that the more recent years are still the same color.
#' For example, the most recent year of data will always be
#' black and the second most recent year will always be red.
#' If more than the maximum number of color for the `palette`
#' are requested, a color ramp will be constructed for the
#' remaining colors, based on the `palette`.
#'
#' @param num_colors The number of colors to return
#' @param palette A palette found in [RColorBrewwer::brewer.pal.info]
#'
#' @export
#' @author Kelli Faye Johnson
#'
#' @return A vector of HEX colours
#'
#' @examples
#' n <-18
#' plot(data.frame(1:n, 1), col= plotcolour(n), pch = 19, cex = 5)
plotcolour <- function(num_colors = 10,
                       palette = "Set1"){

  palette_table <- RColorBrewer::brewer.pal.info |> as_tibble(rownames = "palette")

  if(!palette %in% palette_table$palette){
    stop("`", palette, "` is not a valid palette. Valid palettes are:\n\n",
         paste(palette_table$palette, collapse = ", "))
  }

  palette_info <- brewer.pal.info[rownames(RColorBrewer::brewer.pal.info) == palette, ]
  if(num_colors <= palette_info$maxcolors){
    base <- RColorBrewer::brewer.pal(name = palette, n = num_colors)
    colors <- c(base[(num_colors - 1):1], "#000000")
  }else{
    base <- RColorBrewer::brewer.pal(name = palette, n = palette_info$maxcolors)
    colors <- c(base[(palette_info$maxcolors - 1):1], "#000000")

    palette_func <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(palette_info$maxcolors, palette))
    palette_colors <- palette_func(n = num_colors - palette_info$maxcolors + 3)

    # Get rid of the last few colors by creating 3 more color than needed and
    # then removing them from the beginning. This avoids the last color being
    # the same as the first when more than `palette_info$maxcolors` length
    palette_colors <- palette_colors[-(1:3)]
    colors <- c(palette_colors, colors)
  }

  colors
}
