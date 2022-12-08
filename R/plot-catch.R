#' Plot the cumulative catch by day for a group of years
#'
#' @param d A tibble holding the spatial data, as output by
#' [load_spatial_catch_data()]
#' @param cumulative If `TRUE`, plot cumulative catches
#' @param lu A lookup table which is a data frame of the areas with the
#' columns `code` <character>, `short_name` <character>, and
#' `desc` <character>
#' @param rem_areas A vector of areas to remove. Numerical values representing
#' the `code` column in `lu` are required
#' @param title Plot title
#' @param inc_years Years to include in the plot. If `NULL`, all years will
#' be included
#' @param scale_factpr The value to divide catches by to get to thousands
#' of tonnes
#'
#' @return
#' @export
#'
#' @examples
plot_catch <- function(d,
                       cumulative = FALSE,
                       #area_lu = areas_lu,
                       month_lu = months_lu,
                       rem_areas = NULL,
                       title = NULL,
                       inc_years = (year(now()) - 4):year(now()),
                       scale_factor = 1e6,
                       view_legend = FALSE,
                       leg_pos = NULL,
                       line_thickness = 0.5,
                       point_size = 0.5,
                       angle_x_labels = FALSE,
                       show_x_axis_labels = TRUE,
                       ylim = NULL,
                       x_axis_font_size = 4,
                       y_axis_font_size = 4,
                       y_label_font_size = 6,
                       title_font_size = 6){

  if(!is.null(rem_areas)){
    d <- d |>
      filter(!PFMC %in% rem_areas)
  }

  if(!is.null(inc_years)){
    d <- d |>
      filter(years %in% inc_years)
  }

  d <- d |>
    mutate(catch = catch / scale_factor)

  #area_lu <- area_lu |>
    #mutate(code = as.numeric(code))

  # Get all unique included areas
  #unq_areas <- sort(unique(d$PFMC))
  #unq_areas_nms <- area_lu |>
  #  filter(code %in% unq_areas) |>
  #  pull(short_name)
  unq_months <- as.character(sort(unique(d$months)))

  #names(area_lu) <- c("area", "short_name", "desc")

  tab <- d |>
    select(years, months, catch) |>
    mutate(years = as.character(years)) |>
    mutate(months = recode(months, !!!month_lu)) |>
    group_by(years) |>
    complete(months = month_lu, fill = list(catch = 0)) |>
    mutate(months = as.factor(months)) |>
    mutate(months = forcats::fct_relevel(months,  month_lu)) |>
    arrange(months) |>
    ungroup() |>
    group_by(years, months) |>
    summarize(catch = sum(catch)) |>
    ungroup() |>
    mutate(years = as.factor(years))

    num_yrs <- length(unique(tab$years))

    if(cumulative){
      tab <- tab |>
        group_by(years) |>
        mutate(catch = cumsum(catch)) |>
        ungroup()
    }

    g <- ggplot(tab, aes(x = months, y = catch, color = years, group = years)) +
      geom_line(size = line_thickness,
                show.legend = view_legend) +
      geom_point(size = point_size,
                 show.legend = view_legend) +
      scale_color_manual(values = plotcolour(num_yrs))

    #num_areas <- length(unq_areas)

    g <- g +
      scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
      #xlab("Area") +
      ylab("Catch (thousand tonnes)")

    if(angle_x_labels){
      g <- g +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    if(!show_x_axis_labels){
      g <- g +
        theme(axis.text.x = element_blank())
    }

    if(!is.null(title)){
      g <- g +
        ggtitle(title)
    }

    if(!is.null(ylim)){
      g <- g +
        coord_cartesian(ylim = ylim)
    }

    if(!is.null(x_axis_font_size)){
      g <- g +
        theme(axis.text.x = element_text(size = x_axis_font_size))
    }

    if(!is.null(y_axis_font_size)){
      g <- g +
        theme(axis.text.y = element_text(size = y_axis_font_size))
    }

    if(!is.null(y_label_font_size)){
      g <- g +
        theme(axis.title.y = element_text(size = y_label_font_size))
    }

    if(!is.null(title_font_size)){
      g <- g +
        theme(plot.title = element_text(size = title_font_size))
    }

    if(view_legend){
      if(!is.null(leg_pos)){
        g <- g +
          guides(color = guide_legend(ncol = 2,
                                      title = "Year"),
                 shape = guide_legend(override.aes = list(size = 0.75)),
                 color = guide_legend(override.aes = list(size = 0.75))) +
          theme(legend.position = leg_pos,
                legend.title = element_text(size = 5),
                legend.text = element_text(size = 4))
      }else{
        g <- g +
          guides(color = guide_legend(ncol = 2,
                                      title = "Year"))
      }
    }

    g <- g +
      theme(axis.title.x = element_blank())

    g
}

