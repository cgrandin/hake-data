create_spatial_catch_plots <- function(){

  hr <- here::here()
  fig_out_dir <- file.path(hr, "figures-output")
  r_dir <- file.path(hr, "R")

  source(file.path(r_dir, "constants.R"))
  source(file.path(r_dir, "run-spatial-catch-sql.R"))
  source(file.path(r_dir, "load-spatial-catch-data.R"))
  source(file.path(r_dir, "theme.R"))
  source(file.path(r_dir, "plot-area-dist-catch.R"))
  source(file.path(r_dir, "plot-catch.R"))
  source(file.path(r_dir, "plotcolour.R"))

  # `run_spatial_catch_sql()` must be run while on the VPN or the DFO intranet.
  # You can run it on a DFO machine, and transfer the three CSV files
  # it generates to the data-output directory on your non-DFO machine.

  # run_spatial_catch_sql()

  lst <- load_spatial_catch_data()

  theme_set(hake_theme())

  # Make sure to check these areas each year for catch before excluding them
  # (uncomment the next line and check)
  exclude_areas <- 8:9

  # ----- Catch distribution by major area (barplots)
  g <- plot_area_dist_catch(lst[[1]],
                            rem_areas = exclude_areas,
                            view_legend = TRUE)
  legend <- cowplot::get_legend(g)
  ylim <- c(0, 50)

  plt_lst <- list(plot_area_dist_catch(lst[[1]],
                                       title = "Freezer Trawler catch",
                                       rem_areas = exclude_areas,
                                       ylim = ylim),
                  plot_area_dist_catch(lst[[2]],
                                       title = "Shorside catch",
                                       rem_areas = exclude_areas,
                                       ylim = ylim),
                  plot_area_dist_catch(lst |> bind_rows(),
                                       title = "Total catch",
                                       rem_areas = exclude_areas,
                                       ylim = ylim))

  dir.create(fig_out_dir, showWarnings = FALSE)

  out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2) +
    cowplot::draw_grob(legend, 1/2, 0, 1/2, 0.5)

  cowplot::save_plot(file.path(fig_out_dir, "catch-by-areas-barplot.png"), out)

  # ----- Catch distribution by major area and month for 2022 (stacked barplots)
  g <- plot_area_dist_catch(lst[[1]],
                            bymonth = TRUE,
                            bymonth_year = 2022,
                            rem_areas = exclude_areas,
                            view_legend = TRUE)
  legend <- cowplot::get_legend(g)
  ylim <- c(0, 50)

  plt_lst <- list(plot_area_dist_catch(lst[[1]],
                                       title = "Freezer Trawler catch",
                                       bymonth = TRUE,
                                       bymonth_year = 2022,
                                       rem_areas = exclude_areas),
                  plot_area_dist_catch(lst[[2]],
                                       title = "Shorside catch",
                                       bymonth = TRUE,
                                       bymonth_year = 2022,
                                       rem_areas = exclude_areas),
                  plot_area_dist_catch(lst |> bind_rows(),
                                       title = "Total catch",
                                       bymonth = TRUE,
                                       bymonth_year = 2022,
                                       rem_areas = exclude_areas))

  dir.create(fig_out_dir, showWarnings = FALSE)

  out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2) +
    cowplot::draw_grob(legend, 1/2, 0, 1/2, 0.5)

  cowplot::save_plot(file.path(fig_out_dir, "catch-by-areas-month-barplot.png"), out)


  # ----- Catch and cumulative catch plots
  g <- plot_catch(lst[[1]],
                  rem_areas = exclude_areas,
                  view_legend = TRUE)
  legend <- cowplot::get_legend(g)
  catch_ylim <- c(0, 12)
  cumcatch_ylim <- c(0, 55)

  plt_lst <- list(plot_catch(lst[[1]],
                             title = "Freezer Trawler catch",
                             rem_areas = exclude_areas,
                             ylim = catch_ylim),
                  plot_catch(lst[[1]],
                             title = "Freezer Trawler cumulative catch",
                             rem_areas = exclude_areas,
                             cumulative = TRUE,
                             ylim = cumcatch_ylim),
                  plot_catch(lst[[2]],
                             title = "Shoreside catch",
                             rem_areas = exclude_areas,
                             ylim = catch_ylim),
                  plot_catch(lst[[2]],
                             title = "Shoreside cumulative catch",
                             rem_areas = exclude_areas,
                             cumulative = TRUE,
                             view_legend = TRUE,
                             leg_pos = c(0.2, 0.68),
                             ylim = cumcatch_ylim))

  dir.create(fig_out_dir, showWarnings = FALSE)

  out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2)

  cowplot::save_plot(file.path(fig_out_dir, "catch-by-years-lines.png"), out)
}