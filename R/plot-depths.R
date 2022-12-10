#' Plot depth boxplots for Freezer trawlers and Shoreside fleets
#'
#' @param fleet_lst A list as output by [create_depth_by_year()]
#' @param yrs A vector of years to include in the plot
#' @param ylim The limits of the y-axis
#' @param units Either "fm" for fathoms or "m" for meters (default)
#' @param fn The filename to save the plot to. If `NULL`, return
#' the plot instead
#'
#' @return Either a [ggplot2::ggplot()] plot or Nothing is `fn` is `NULL`
#' @export
plot_depths <- function(fleet_lst,
                        yrs = 2017:2022,
                        ylim = c(0, ifelse(units == "m", 1000, 500)),
                        units = c("m", "fm"),
                        fn = here::here("figures-output",
                                        "can-depths.png")){

  units <- match.arg(units)

  hr <- here::here()
  fig_dir <- here::here("figures-output")

  cols <- plotcolour(length(yrs))

  depth_cols <- c("bottom", "gear")

  # Remove JV
  fleet_lst <- fleet_lst[-3]

  d <- imap(fleet_lst, function(depth_lst, fleet_name){
    imap(depth_lst, function(depth_df, depth_name){
      depth_df |>
        mutate(fleet = ifelse(fleet_name == "ft",
                              "Freezer Trawlers",
                              ifelse(fleet_name == "ss",
                                     "Shoreside",
                                     fleet_name)),
               type = paste0(depth_name, " depth"))
    }) |>
      map_df(~{.x})
  }) |>
    map_df(~{.x}) |>
    filter(year %in% yrs) |>
    mutate(fleet_type = paste0(fleet, " - ", type)) |>
    select(-fleet, -type) |>
    mutate(Year = as.character(year))

  if(units == "fm"){
    d <- d |>
      mutate_at(vars(lower95,
                     lowerhinge,
                     median,
                     upperhinge,
                     upper95),
                ~{.x / 1.8288})
  }

  g <- ggplot(d, aes(x = Year,
                     fill = Year,
                     group = Year,
                     ymin = lower95,
                     lower = lowerhinge,
                     middle = median,
                     upper = upperhinge,
                     ymax = upper95)) +
    geom_boxplot(stat = "identity", color = "grey") +
    ylab(ifelse(units == "m", "Meters", "Fathoms")) +
    scale_fill_manual(values = cols) +

    facet_wrap(~ fleet_type) +
    theme(strip.background = element_rect(fill="white"),
          strip.placement = "outside")
  if(!is.null(ylim)){
    g <- g +
      coord_cartesian(ylim = ylim)
  }

  if(!is.null(fn)){
    ggsave(fn, g)
  }
}