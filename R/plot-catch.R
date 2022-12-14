#' Plot the cumulative catch by day for a group of years
#'
#' @param d A tibble holding the spatial data, as output by
#' [load_spatial_catch_data()]
#' @param cumulative If `TRUE`, plot cumulative catches
#' @param grp_col A column name to use as the grouping variable in the
#' [ggplot2::ggplot()]
#' @param month_lu A lookup table containing month numbers and month names
#' @param rem_areas A vector of areas to remove. Numerical values representing
#' the `code` column in `lu` are required
#' @param area_lu A lookup table containing the area numbers and names
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
                       grp_col = c("years", "areas"),
                       month_lu = months_lu,
                       rem_areas = NULL,
                       area_lu = NULL,
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
                       x_axis_font_size = 6,
                       y_axis_font_size = 6,
                       y_label_font_size = 6,
                       title_font_size = 6){

  grp_col <- match.arg(grp_col)

  d <- d |>
    rename(areas = PFMC)

  if(!grp_col %in% names(d)){
    stop("The `grp_col` column `", grp_col, "` does not exist in data frame `d`",
         call. = FALSE)
  }

  if(!is.null(rem_areas)){
    d <- d |>
      filter(!areas %in% rem_areas)
  }

  if(!is.null(inc_years)){
    d <- d |>
      filter(years %in% inc_years)
  }

  d <- d |>
    mutate(catch = catch / scale_factor)

  grp_col_sym <- sym(grp_col)

  tab <- d |>
    select(!!grp_col_sym, months, catch) |>
    mutate(!!grp_col_sym := as.character(!!grp_col_sym)) |>
    mutate(months = recode(months, !!!month_lu)) |>
    group_by(!!grp_col_sym) |>
    complete(months = month_lu, fill = list(catch = 0)) |>
    mutate(months = as.factor(months)) |>
    mutate(months = forcats::fct_relevel(months,  month_lu)) |>
    arrange(months) |>
    ungroup() |>
    group_by(!!grp_col_sym, months) |>
    summarize(catch = sum(catch)) |>
    ungroup() |>
    mutate(grp_col_sym := as.factor(!!grp_col_sym))

    num_grps <- length(unique(tab[[grp_col]]))

    if(cumulative){
      tab <- tab |>
        group_by(!!grp_col_sym) |>
        mutate(catch = cumsum(catch)) |>
        ungroup()
    }

    if(grp_col == "areas"){
      if(is.null(area_lu)){
        stop("`area_lu` is `NULL` but is needed because you chose `areas` ",
             "for `grp_col`", call. =  FALSE)
      }
      tab <- tab |>
        arrange(areas) |>
        mutate(areas = recode(areas, !!!area_lu))
    }

    g <- ggplot(tab, aes(x = months, y = catch, color = !!grp_col_sym, group = !!grp_col_sym)) +
      geom_line(size = line_thickness,
                show.legend = view_legend) +
      geom_point(size = point_size,
                 show.legend = view_legend) +
      scale_color_manual(values = plotcolour(num_grps))

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
                                      title = str_to_title(grp_col)),
                 shape = guide_legend(override.aes = list(size = 0.75)),
                 color = guide_legend(override.aes = list(size = 0.75))) +
          theme(legend.position = leg_pos,
                legend.title = element_text(size = 5),
                legend.text = element_text(size = 4))
      }else{
        g <- g +
          guides(color = guide_legend(ncol = 2,
                                      title = str_to_title(grp_col)))
      }
    }

    g <- g +
      theme(axis.title.x = element_blank())

    g
}

