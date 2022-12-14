create_spatial_catch_plots <- function(lst){

  hr <- here::here()
  fig_out_dir <- file.path(hr, "figures-output")
  r_dir <- file.path(hr, "R")

  # Make sure to check these areas each year for catch before excluding them
  # (uncomment the next line and check)
  exclude_areas <- 8:9

  # ----- Catch distribution by major area (barplots)
  g <- plot_area_dist_catch(lst[[1]],
                            area_lu = areas_names_lu,
                            rem_areas = exclude_areas,
                            view_legend = TRUE)
  legend <- cowplot::get_legend(g)
  ylim <- c(0, 50)

  plt_lst <- list(plot_area_dist_catch(lst[[1]],
                                       title = "Freezer Trawler catch",
                                       rem_areas = exclude_areas,
                                       area_lu = areas_names_lu,
                                       ylim = ylim),
                  plot_area_dist_catch(lst[[2]],
                                       title = "Shorside catch",
                                       rem_areas = exclude_areas,
                                       area_lu = areas_names_lu,
                                       ylim = ylim),
                  plot_area_dist_catch(lst |> bind_rows(),
                                       title = "Total catch",
                                       rem_areas = exclude_areas,
                                       area_lu = areas_names_lu,
                                       ylim = ylim))

  dir.create(fig_out_dir, showWarnings = FALSE)

  out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2) +
    cowplot::draw_grob(legend, 1/2, 0, 1/2, 0.5)

  cowplot::save_plot(file.path(fig_out_dir, "catch-by-areas-barplot.png"), out)

  # ----- Catch distribution by major area and month by year
  yrs_to_plot <- 2021:2022
  catch_ylim <- c(0, 5)
  cumcatch_ylim <- list(c(0, 25), c(0, 25))
  walk2(yrs_to_plot, cumcatch_ylim, ~{
    g <- plot_catch(lst[[1]],
                    rem_areas = exclude_areas,
                    view_legend = TRUE)
    legend <- cowplot::get_legend(g)

    plt_lst <- list(plot_catch(lst[[1]],
                               title = "Freezer Trawler catch",
                               grp_col = "areas",
                               area_lu = areas_names_lu,
                               inc_years = .x,
                               ylim = catch_ylim,
                               rem_areas = exclude_areas),
                    plot_catch(lst[[1]],
                               title = "Freezer Trawler cumulative catch",
                               grp_col = "areas",
                               area_lu = areas_names_lu,
                               inc_years = .x,
                               cumulative = TRUE,
                               ylim = .y,
                               rem_areas = exclude_areas),
                    plot_catch(lst[[2]],
                               title = "Shoreside catch",
                               grp_col = "areas",
                               area_lu = areas_names_lu,
                               inc_years = .x,
                               ylim = catch_ylim,
                               rem_areas = exclude_areas),
                    plot_catch(lst[[2]],
                               title = "Shoreside cumulative catch",
                               grp_col = "areas",
                               area_lu = areas_names_lu,
                               inc_years = .x,
                               view_legend = TRUE,
                               leg_pos = c(0.35, 0.68),
                               cumulative = TRUE,
                               ylim = .y,
                               rem_areas = exclude_areas))

    dir.create(fig_out_dir, showWarnings = FALSE)

    out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2)# +
    #cowplot::draw_grob(legend, 1/2, 0, 1/2, 0.5)

    cowplot::save_plot(file.path(fig_out_dir, paste0(.x, "-catch-by-areas-month-lines.png")), out)
  })

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
                             view_legend = TRUE,
                             leg_pos = c(0.2, 0.68),
                             ylim = cumcatch_ylim),
                  plot_catch(lst[[2]],
                             title = "Shoreside catch",
                             rem_areas = exclude_areas,
                             ylim = catch_ylim),
                  plot_catch(lst[[2]],
                             title = "Shoreside cumulative catch",
                             rem_areas = exclude_areas,
                             cumulative = TRUE,
                             ylim = cumcatch_ylim))

  dir.create(fig_out_dir, showWarnings = FALSE)

  out <- cowplot::plot_grid(plotlist = plt_lst, nrow = 2, ncol = 2)

  cowplot::save_plot(file.path(fig_out_dir, "catch-by-years-lines.png"), out)
}