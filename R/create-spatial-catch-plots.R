hr <- here::here()
dat_out_dir <- file.path(hr, "data-output")
fig_out_dir <- file.path(hr, "figures-output")
r_dir <- file.path(hr, "R")

source(file.path(r_dir, "constants.R"))
source(file.path(r_dir, "catch-utilities.R"))
source(file.path(r_dir, "run-spatial-catch-sql.R"))
source(file.path(r_dir, "load-spatial-catch-data.R"))
source(file.path(r_dir, "theme.R"))
source(file.path(r_dir, "plot-area-dist.R"))

# The following line must be run while on the VPN or the DFO intranet
# You can run it on another computer, and tranfser the three CSV files
# it generates to the data-output directory on your working machine

# run_spatial_catch_sql()

lst <- load_spatial_catch_data()

theme_set(hake_theme())

g <- plot_area_dist(lst[[1]], rem_areas = c(2, 8, 9), view_legend = TRUE)
legend <- cowplot::get_legend(g)
ylim <- c(0, 50000)
plt_lst <- list(plot_area_dist(lst[[1]],
                               title = "Freezer Trawler catch",
                               rem_areas = c(2, 8, 9),
                               ylim = ylim),
                plot_area_dist(lst[[2]],
                               title = "Shorside catch",
                               rem_areas = c(2, 8, 9),
                               ylim = ylim),
                plot_area_dist(lst |> bind_rows(),
                               title = "Total catch",
                               rem_areas = c(2, 8, 9),
                               ylim = ylim))

dir.create(fig_out_dir, showWarnings = FALSE)

out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2) +
  cowplot::draw_grob(legend, 1/2, 0, 1/2, 0.5)

cowplot::save_plot(file.path(fig_out_dir, "areas-catch-distribution.png"), out)
