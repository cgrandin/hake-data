
plot_depths <- function(d,
                        yrs = 2017:2022,
                        ylim = c(0, ifelse(units == "m", 1000, 500)),
                        units = c("fm", "m")){

  units <- match.arg(units)

  hr <- here::here()
  fig_dir <- here::here("figures-output")

  cols <- plotcolour(length(yrs))

  setEPS()
  #postscript("can-bottom-depths.eps") ##, res=300, width=10, height=7, units="in")
  png(file.path(fig_dir, "hake-bottom-depths.png"), res = 300, width = 10, height = 7, units = "in")
  oldpar <- par()
  par(mfrow = c(2, 2),
      #oma = c(5, 4, 0, 0) + 0.1,
      mar = c(4, 4, 1, 1) + 0.1)

  if(units == "m"){
    d <- d |>
      mutate(bottomdepth_fm = 1.8288 * bottomdepth_fm,
             geardepth_fm = 1.8288 * geardepth_fm)
  }

  d_ft <- d |>
    filter(vessel %in% ft_vessels$cfv_num) |>
    filter(year >= min(yrs) & year <= max(yrs))
  d_ss <- d |>
    filter(!vessel %in% ft_vessels$cfv_num) |>
    filter(year >= min(yrs) & year <= max(yrs))

  boxplot(bottomdepth_fm~year,
          data = d_ft,
          outline = FALSE,
          xlab = "",
          ylab = ifelse(units == "m", "Meters", "Fathoms"),
          main = "Freezer trawlers - bottom depth",
          lty = 1,
          lwd = 1,
          las = 1,
          col = cols,
          border = "grey",
          ylim = ylim,
          staplelty = 0)

  boxplot(geardepth_fm~year,
          data = d_ft,
          outline = FALSE,
          xlab = "",
          ylab = "",
          main = "Freezer trawlers - Fishing depth",
          lty = 1,
          lwd = 1,
          las = 1,
          col = cols,
          border = "grey",
          ylim = ylim,
          staplelty = 0)

  boxplot(bottomdepth_fm~year,
          data = d_ss,
          outline = FALSE,
          xlab = "",
          ylab = ifelse(units == "m", "Meters", "Fathoms"),
          main = "Shoreside - bottom depth",
          lty = 1,
          lwd = 1,
          las = 1,
          col = cols,
          border = "grey",
          ylim = ylim,
          staplelty = 0)

  boxplot(geardepth_fm~year,
          data = d_ss,
          outline = FALSE,
          xlab = "",
          ylab = "",
          main = "Shoreside - Fishing depth",
          lty = 1,
          lwd = 1,
          las = 1,
          col = cols,
          border = "grey",
          ylim = ylim,
          staplelty = 0)


    mtext("Year", 1, -2, outer = TRUE)
  dev.off()
  par(oldpar)
}