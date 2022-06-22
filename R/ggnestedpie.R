#' Create nested pie plot.
#'
#' @param data Data frame contains full data or summarized data.
#' @param group_key Column used to summarize the data. Default: NULL.
#' @param count_type Data frame type, chosen from "count" and "full". "count" means summarized data and "full" means full data. Default: count.
#' @param r0 The radius of inner blank circle. Default: 0.5 (donut plot). When set to 0, inner plot is pie.
#' @param r1 The radius of inner pie plot. Default: 1.5.
#' @param r2 The radius of outer pie plot. Default: 2.5.
#' @param inner_thick The width of inner pie plot. Default: 1.
#' @param outer_thick The width of outer pie plot. Default: 1.
#' @param inner_fill_color Colors used for inner pie plot. Default: NULL (conduct automatic selection).
#' @param inner_label Logical value, whether to show label on inner pie label. Default: TRUE.
#' @param inner_label_info Label information type of inner pie plot, chosen from count, ratio and all (count and ratio). Default: count.
#' @param inner_label_color Color of the label on inner pie. Default: black.
#' @param inner_label_split Pattern used to split the label of inner pie, support regular expression. Default: space.
#' @param inner_labal_threshold Threashold of the ratio to determine label or not on inner pie. Default: NULL.
#' @param inner_label_size Size of the label on inner pie. Default: 4.
#' @param outer_fill_color Colors used for outer pie plot. Default: NULL (conduct automatic selection).
#' @param outer_label_type Label style of outer pie plot, chosen from circle, horizon and none (no label). Default: circle.
#' @param outer_label_pos Label position of outer pie, chosen from in and out. Default: in.
#' @param outer_label_info Label information type of outer pie plot, chosen from count, ratio and all (count and ratio). Default: count.
#' @param outer_label_split Pattern used to split the label of outer pie, support regular expression. Default: space.
#' @param outer_label_color Color of the label on outer pie. Default: black.
#' @param outer_label_gap Gap between label and outer pie plot, used when \code{outer_label_pos} is out.
#' @param outer_labal_threshold Threashold of the ratio to determine label position (in/out pie). Default: NULL.
#' @param outer_label_size Size of the label on outer pie. Default: 4.
#' @param border_color Border color. Default: black.
#'
#' @return A ggplot2 object.
#' @importFrom dplyr group_by summarise select
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @import ggnewscale
#' @export
#'
#' @examples
#' library(ggpie)
#' library(ggplot2)
#' data(diamonds)
#' # inner circle label, outer circle label and in pie plot
#' ggnestedpie(
#'   data = diamonds, group_key = c("cut", "color"), count_type = "full",
#'   inner_label_info = "all", inner_label_split = NULL,
#'   outer_label_type = "circle", outer_label_pos = "in", outer_label_info = "all"
#' )
#' # inner circle label, outer circle label and in pie plot, remove fraction below 1 of inner pie
#' ggnestedpie(
#'   data = diamonds, group_key = c("cut", "color"), count_type = "full",
#'   inner_label_info = "all", inner_label_split = NULL,
#'   inner_labal_threshold = 1, inner_label_size = 3,
#'   outer_label_type = "circle", outer_label_pos = "in", outer_label_info = "all"
#' )
#' # inner circle label, outer circle label and out of pie plot
#' ggnestedpie(
#'   data = diamonds, group_key = c("cut", "color"), count_type = "full",
#'   inner_label_info = "all", inner_label_split = NULL,
#'   outer_label_type = "circle", outer_label_pos = "out", outer_label_info = "all"
#' )
#' # inner circle label and no split, outer horizon label and out of pie plot,
#' # remove fraction below 1 of inner pie
#' ggnestedpie(
#'   data = diamonds, group_key = c("cut", "color"), count_type = "full",
#'   inner_label_info = "all", inner_label_split = NULL,
#'   inner_labal_threshold = 1, inner_label_size = 3,
#'   outer_label_type = "horizon", outer_label_pos = "out", outer_label_info = "all"
#' )
#' # inner circle label and no split, outer horizon label and in pie plot,
#' # remove fraction below 1 of inner pie,
#' # adjust fraction below 10 to out of pie of outer pie plot.
#' ggnestedpie(
#'   data = diamonds, group_key = c("cut", "color"), count_type = "full",
#'   inner_label_info = "all", inner_label_split = NULL,
#'   inner_labal_threshold = 1, inner_label_size = 3,
#'   outer_label_type = "horizon", outer_label_pos = "in",
#'   outer_label_info = "all", outer_labal_threshold = 10
#' )
#' # create blank between inner and outer pie
#' ggnestedpie(
#'   data = diamonds, group_key = c("cut", "color"), count_type = "full", r0 = 0.5, r1 = 1.5, r2 = 2.6,
#'   inner_label_info = "all", inner_label_split = NULL,
#'   inner_labal_threshold = 1, inner_label_size = 3,
#'   outer_label_type = "horizon", outer_label_pos = "in",
#'   outer_label_info = "all", outer_labal_threshold = 10
#' )
ggnestedpie <- function(data, group_key = NULL, count_type = c("count", "full"), r0 = 0.5, r1 = 1.5, r2 = 2.5, inner_thick = 1, outer_thick = 1,
                        inner_fill_color = NULL, inner_label = TRUE, inner_label_info = c("count", "ratio", "all"), inner_label_color = "black",
                        inner_label_split = "[[:space:]]+", inner_labal_threshold = NULL, inner_label_size = 4,
                        outer_fill_color = NULL, outer_label_type = c("circle", "horizon", "none"), outer_label_pos = c("in", "out"),
                        outer_label_info = c("count", "ratio", "all"), outer_label_split = "[[:space:]]+", outer_label_color = "black",
                        outer_label_gap = 0.05, outer_labal_threshold = NULL, outer_label_size = 4,
                        border_color = "black") {
  # check parameters
  count_type <- match.arg(arg = count_type)
  inner_label_info <- match.arg(arg = inner_label_info)
  outer_label_info <- match.arg(arg = outer_label_info)
  outer_label_type <- match.arg(arg = outer_label_type)
  outer_label_pos <- match.arg(arg = outer_label_pos)

  # check group key
  if (is.null(group_key)) {
    stop("Group information is required.")
  } else {
    if (length(group_key) != 2) {
      stop("Please provide two group columns.")
    } else {
      if (!all(group_key %in% colnames(data))) {
        stop("Not all group columns are in data.")
      }
    }
  }

  # stat data
  data[group_key] <- apply(data[group_key], 2, as.character)
  if (count_type == "full") {
    data <- data %>%
      dplyr::group_by(across(all_of(group_key))) %>%
      dplyr::summarise(count = n()) %>%
      as.data.frame()
    data$group <- paste(data[, group_key[1]], data[, group_key[2]], sep = "_")
  } else {
    if (!"count" %in% colnames(data)) {
      stop("count column is missing in your data.")
    }
  }
  main_data <- data %>%
    dplyr::group_by(.data[[group_key[1]]]) %>%
    dplyr::summarise(count = sum(count))
  colnames(main_data) <- c("group", "count")
  sub_data <- data %>% dplyr::select(c("group", "count", group_key[2]))
  colnames(sub_data) <- c("group", "count", "subgroup")
  # get first non-unique index
  subgroup_unique_index <- match(unique(sub_data$subgroup), sub_data$subgroup)
  sub_data[subgroup_unique_index, "group"] <- sub_data[subgroup_unique_index, "subgroup"]

  ############ inner pie
  # prepare inner fill color
  all_subgroups <- unique(as.character(sub_data$subgroup))
  if (is.null(inner_fill_color)) {
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    color_len <- length(all_subgroups)
    inner_fill_color <- getPalette(color_len)
  } else if (length(inner_fill_color) != length(all_subgroups)) {
    stop("The length of fill color is greater than 1 and not equal to group number.")
  }
  names(inner_fill_color) <- all_subgroups
  inner_fill_full_df <- merge(sub_data, as.data.frame(inner_fill_color, stringsAsFactors = FALSE), by.x = "subgroup", by.y = 0)
  inner_fill_full <- inner_fill_full_df$inner_fill_color
  names(inner_fill_full) <- as.character(inner_fill_full_df$group)
  sub_data$group <- factor(sub_data$group, levels = sub_data$group)

  # inner pie width
  if (is.null(inner_thick)) {
    inner_width <- r1 - r0
  } else {
    inner_width <- inner_thick
  }
  if ((r1 - r0) < inner_width) {
    warning("There may be some error, becourse the width of inner pie is greater than it should be!")
  }

  # create inner pie
  if (inner_label) {
    sub_data$preangle <- (cumsum(sub_data$count) - 0.5 * sub_data$count) / sum(sub_data$count) * 360
    sub_data$angle <- sub_data$preangle %% 180 - 90
    # create label
    if (inner_label_info == "count") {
      sub_data$label <- as.character(sub_data$count)
    } else if (inner_label_info == "ratio") {
      sub_data$label <- as.character(scales::percent(sub_data$count / sum(sub_data$count)))
    } else if (inner_label_info == "all") {
      sub_data$label <- paste0(sub_data$count, " (", scales::percent(sub_data$count / sum(sub_data$count)), ")")
    }
    # split label
    if (!is.null(inner_label_split)) {
      sub_data$label <- gsub(pattern = inner_label_split, replacement = "\n", x = sub_data$label)
    }
    # prepare label color
    if (is.null(inner_label_color)) {
      inner_label_full <- inner_fill_full
    } else {
      if (length(inner_label_color) == 1) {
        inner_label_full <- rep(inner_label_color, nrow(sub_data))
        names(inner_label_full) <- as.character(inner_fill_full_df$group)
      } else if (length(inner_label_color) != length(all_subgroups)) {
        stop("The length of label color is greater than 1 and not equal to group number.")
      } else {
        names(inner_label_color) <- all_subgroups
        inner_label_full_df <- merge(sub_data, as.data.frame(inner_label_color, stringsAsFactors = FALSE), by.x = "subgroup", by.y = 0)
        inner_label_full <- inner_label_full_df$inner_label_color
        names(inner_label_full) <- inner_label_full_df$group
      }
    }
    # get label data
    if (is.null(inner_labal_threshold)) {
      inner_label_data <- sub_data
    } else {
      inner_label_data <- sub_data
      inner_label_data[(inner_label_data$count * 100 / sum(inner_label_data$count)) < inner_labal_threshold, "label"] <- ""
    }
    inner_pie_plot <- ggplot() +
      geom_bar(sub_data, mapping = aes(x = (r0 + r1) / 2, y = count, fill = group), colour = border_color, stat = "identity", width = inner_width) +
      geom_text(
        data = inner_label_data,
        mapping = aes(x = (r0 + r1) / 2, y = count, label = label, angle = angle, color = group), show.legend = FALSE,
        position = position_stack(vjust = 0.5),
        size = inner_label_size
      ) +
      coord_polar(theta = "y", start = 0, clip = "off") +
      theme_void() +
      scale_fill_manual(
        values = inner_fill_full,
        breaks = all_subgroups
      ) +
      scale_color_manual(values = inner_label_full)
  } else {
    inner_pie_plot <- ggplot() +
      geom_bar(sub_data,
        mapping = aes(x = (r0 + r1) / 2, y = count, fill = group), colour = border_color,
        stat = "identity", width = inner_width
      ) +
      coord_polar(theta = "y", start = 0, clip = "off") +
      theme_void() +
      scale_fill_manual(
        values = inner_fill_full,
        breaks = all_subgroups
      )
  }

  ############ outer pie
  # outer pie width
  if (is.null(outer_thick)) {
    outer_width <- r2 - r1
  } else {
    outer_width <- outer_thick
  }
  if ((r2 - r1) < outer_width) {
    warning("There may be some error, becourse the width of outer pie is greater than it should be!")
  }
  # prepare outer fill color
  all_maingroups <- unique(as.character(main_data$group))
  if (is.null(outer_fill_color)) {
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    outer_color_len <- length(all_maingroups)
    outer_fill_color <- getPalette(outer_color_len)
  } else if (length(outer_fill_color) != length(all_maingroups)) {
    stop("The length of fill color is greater than 1 and not equal to group number.")
  }
  names(outer_fill_color) <- all_maingroups

  if (outer_label_type != "none") {
    # create label
    if (outer_label_info == "count") {
      main_data$label <- as.character(main_data$count)
    } else if (outer_label_info == "ratio") {
      main_data$label <- as.character(scales::percent(main_data$count / sum(main_data$count)))
    } else if (outer_label_info == "all") {
      main_data$label <- paste0(main_data$count, " (", scales::percent(main_data$count / sum(main_data$count)), ")")
    }
    # split label
    if (!is.null(outer_label_split)) {
      main_data$label <- gsub(pattern = outer_label_split, replacement = "\n", x = main_data$label)
    }
    # get outer label color
    if (is.null(outer_label_color)) {
      outer_label_color <- outer_fill_color
    } else {
      if (length(outer_label_color) == 1) {
        outer_label_color <- rep(outer_label_color, length(all_maingroups))
      } else if (length(outer_label_color) != length(all_maingroups)) {
        stop("The length of outer label color is greater than 1 and not equal to group number.")
      }
      names(outer_label_color) <- all_maingroups
    }
    if (outer_label_type == "circle") {
      main_data$preangle <- (cumsum(main_data$count) - 0.5 * main_data$count) / sum(main_data$count) * 360
      main_data$angle <- main_data$preangle %% 180 - 90
      main_data$CumSum <- rev(round(cumsum(rev(main_data$count)) - rev(main_data$count / 2), 2))
      if (outer_label_pos == "out") {
        pie_plot <- inner_pie_plot + guides(fill = guide_legend(title = group_key[2])) +
          new_scale_color() + new_scale_fill() +
          geom_bar(main_data,
            mapping = aes(x = (r2 + r1) / 2, y = count, fill = group),
            width = outer_width, stat = "identity", color = border_color
          ) +
          geom_text(main_data,
            mapping = aes(x = r2 + outer_label_gap, y = CumSum, label = label, angle = angle, colour = group), show.legend = FALSE,
            hjust = ifelse(main_data$preangle > 180, 0, 1), size = outer_label_size
          ) +
          coord_polar(theta = "y", start = 0, clip = "off") +
          theme_void() +
          scale_fill_manual(values = outer_fill_color) +
          scale_colour_manual(values = outer_label_color) +
          guides(fill = guide_legend(title = group_key[1])) +
          xlim(0, NA)
      } else if (outer_label_pos == "in") {
        pie_plot <- inner_pie_plot + guides(fill = guide_legend(title = group_key[2])) +
          new_scale_color() + new_scale_fill() +
          geom_bar(main_data,
            mapping = aes(x = (r2 + r1) / 2, y = count, fill = group),
            width = outer_width, stat = "identity", color = border_color
          ) +
          geom_text(main_data,
            mapping = aes(x = (r2 + r1) / 2, y = CumSum, label = label, angle = angle, colour = group),
            show.legend = FALSE, size = outer_label_size
          ) +
          coord_polar(theta = "y", start = 0, clip = "off") +
          theme_void() +
          scale_fill_manual(values = outer_fill_color) +
          scale_colour_manual(values = outer_label_color) +
          guides(fill = guide_legend(title = group_key[1])) +
          xlim(0, NA)
      }
    }
    # create horizon label plot
    if (outer_label_type == "horizon") {
      main_data$CumSum <- rev(round(cumsum(rev(main_data$count)) - rev(main_data$count / 2), 2))
      if (outer_label_pos == "out") {
        pie_plot <- inner_pie_plot + guides(fill = guide_legend(title = group_key[2])) +
          new_scale_color() + new_scale_fill() +
          geom_bar(main_data,
            mapping = aes(x = (r2 + r1) / 2, y = count, fill = group),
            width = outer_width, stat = "identity", color = border_color
          ) +
          geom_text_repel(
            data = main_data,
            mapping = aes(label = label, y = CumSum, x = after_stat(r2), colour = group), show.legend = FALSE,
            point.padding = NA, max.overlaps = Inf, nudge_x = 1, nudge_y = 1,
            segment.curvature = -0.2, segment.ncp = 10, segment.angle = 20, size = outer_label_size
          ) +
          coord_polar(theta = "y", start = 0, clip = "off") +
          theme_void() +
          scale_fill_manual(values = outer_fill_color) +
          scale_colour_manual(values = outer_label_color) +
          guides(fill = guide_legend(title = group_key[1])) +
          xlim(0, NA)
      } else if (outer_label_pos == "in") {
        if (is.null(outer_labal_threshold)) {
          pie_plot <- inner_pie_plot + guides(fill = guide_legend(title = group_key[2])) +
            new_scale_color() + new_scale_fill() +
            geom_bar(main_data,
              mapping = aes(x = (r2 + r1) / 2, y = count, fill = group),
              width = outer_width, stat = "identity", color = border_color
            ) +
            geom_text_repel(
              data = main_data,
              mapping = aes(x = (r2 + r1) / 2, y = CumSum, label = label, colour = group),
              show.legend = FALSE, size = outer_label_size
            ) +
            coord_polar(theta = "y", start = 0, clip = "off") +
            theme_void() +
            scale_fill_manual(values = outer_fill_color) +
            scale_colour_manual(values = outer_label_color) +
            guides(fill = guide_legend(title = group_key[1])) +
            xlim(0, NA)
        } else {
          pie_plot <- inner_pie_plot + guides(fill = guide_legend(title = group_key[2])) +
            new_scale_color() + new_scale_fill() +
            geom_bar(main_data,
              mapping = aes(x = (r2 + r1) / 2, y = count, fill = group),
              width = outer_width, stat = "identity", color = border_color
            ) +
            geom_text_repel(
              data = main_data[main_data$count * 100 / sum(main_data$count) < outer_labal_threshold, ],
              aes(label = label, y = CumSum, x = after_stat(r2), colour = group), show.legend = FALSE,
              size = outer_label_size, point.padding = NA, max.overlaps = Inf, nudge_x = 1, nudge_y = 1,
              segment.curvature = -0.2, segment.ncp = 10, segment.angle = 20
            ) +
            geom_text(
              data = main_data[main_data$count * 100 / sum(main_data$count) >= outer_labal_threshold, ],
              aes(y = CumSum, x = (r2 + r1) / 2, label = label, colour = group),
              show.legend = FALSE, size = outer_label_size
            ) +
            coord_polar(theta = "y", start = 0, clip = "off") +
            theme_void() +
            scale_fill_manual(values = outer_fill_color) +
            scale_colour_manual(values = outer_label_color) +
            guides(fill = guide_legend(title = group_key[1])) +
            xlim(0, NA)
        }
      }
    }
  } else {
    pie_plot <- inner_pie_plot + guides(fill = guide_legend(title = group_key[2])) +
      new_scale_color() + new_scale_fill() +
      geom_bar(main_data,
        mapping = aes(x = (r2 + r1) / 2, y = count, fill = group),
        width = outer_width, stat = "identity", color = border_color
      ) +
      coord_polar(theta = "y", start = 0, clip = "off") +
      theme_void() +
      scale_fill_manual(values = outer_fill_color) +
      guides(fill = guide_legend(title = group_key[1])) +
      xlim(0, NA)
  }
  return(pie_plot)
}
