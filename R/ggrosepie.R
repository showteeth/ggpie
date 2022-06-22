#' Create rose pie plot.
#'
#' @param data Data frame contains full data or summarized data.
#' @param group_key Column used to summarize the data, one or two are acceptable. Default: NULL.
#' @param count_type Data frame type, chosen from "count" and "full". "count" means summarized data and "full" means full data. Default: count.
#' @param fill_color Colors used. When length of \code{group_key} is two, color the subgroup, otherwise the main group. Default: NULL (conduct automatic selection).
#' @param label_info Label information type, chosen from count, ratio and all (count and ratio). Default: count.
#' @param label_color Color of the label. When length of \code{group_key} is two, this should be set to one color. Default: black.
#' @param sort Logical value, whether to order the plot by counts. Default: TRUE.
#' @param show_tick Logical value, whether to show the tick. Default: TRUE.
#' @param tick_break The break of tick. Default: NULL (conduct automatic selection).
#' @param show_label Logical value, whether to show the label. Default: TRUE.
#' @param label_sep The separator between group and count info. Default: |.
#' @param label_gap The gap between label and plot. Default: 0.05 (count + 0.05*count).
#' @param label_size The size of label. Default: 4.
#' @param donut_frac The fraction of donut. Default: 0.1 (0.1*max(count)).
#' @param donut_label Logical value, whether to show total number in the center of the plot. Default: TRUE.
#' @param donut_label_size The label size of center label. Default: 4.
#' @param donut_label_color The color of center label. Default: red.
#' @param border_color Border color. Default: black.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom dplyr mutate group_by summarise n arrange select distinct
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent extended_breaks
#' @importFrom stats median
#' @import ggplot2
#'
#' @examples
#' library(ggpie)
#' library(ggplot2)
#' data(diamonds)
#' # do not show tick
#' ggrosepie(diamonds,
#'   group_key = "color", count_type = "full", label_info = "all",
#'   show_tick = FALSE, donut_frac = 0.3, donut_label_size = 3
#' )
#' # show tick and with automatic selection
#' ggrosepie(diamonds,
#'   group_key = "color", count_type = "full", label_info = "all",
#'   donut_frac = 0.3, donut_label_size = 3
#' )
#' # show tick and with specific break
#' ggrosepie(diamonds,
#'   group_key = "color", count_type = "full", label_info = "all",
#'   tick_break = c(3000, 5000, 7000, 11000), donut_frac = 0.3, donut_label_size = 3
#' )
#' # two group variable, and do not show tick
#' ggrosepie(diamonds,
#'   group_key = c("color", "clarity"),
#'   count_type = "full", label_info = "all",
#'   show_tick = FALSE, donut_frac = 0.3, donut_label_size = 3
#' )
#' # two group variable, show tick and with automatic selection
#' ggrosepie(diamonds,
#'   group_key = c("color", "clarity"),
#'   count_type = "full", label_info = "all",
#'   donut_frac = 0.3, donut_label_size = 3
#' )
#' # two group variable, show tick and with specific break
#' ggrosepie(diamonds,
#'   group_key = c("color", "clarity"),
#'   count_type = "full", label_info = "all",
#'   tick_break = c(3000, 5000, 7000, 11000), donut_frac = 0.3, donut_label_size = 3
#' )
ggrosepie <- function(data, group_key = NULL, count_type = c("count", "full"), fill_color = NULL, label_info = c("count", "ratio", "all"),
                      label_color = "black", sort = TRUE, show_tick = TRUE, tick_break = NULL, show_label = TRUE, label_sep = "|", label_gap = 0.05,
                      label_size = 4, donut_frac = 0.1, donut_label = TRUE, donut_label_size = 4, donut_label_color = "red", border_color = "black") {
  # check parameters
  count_type <- match.arg(arg = count_type)
  label_info <- match.arg(arg = label_info)

  # prepare data
  if (length(group_key) == 1) {
    plot.data <- PrepareData(
      data = data, group_key = group_key, count_type = count_type, fill_color = fill_color,
      label_info = label_info, label_split = NULL, label_color = label_color
    )
    data <- plot.data[["data"]]
    fill_color <- plot.data[["fill_color"]]
    label_color <- plot.data[["label_color"]]

    # add tick info
    if (show_tick) {
      # add new row
      add_row <- data.frame("Tick", 0, "")
      colnames(add_row) <- colnames(data)
      data <- rbind(add_row, as.data.frame(data)) %>% as.data.frame()
      data$Sum <- data$count
      # sort the data
      if (sort) {
        data <- data %>%
          dplyr::arrange(Sum) %>%
          as.data.frame()
        data$group <- factor(data$group, levels = unique(as.character(data$group)))
      }
      start_pi <- -pi / length(levels(data$group))
      data_group_sum <- unique(data$Sum)
      # create tick break
      if (is.null(tick_break)) {
        tick_df <- data.frame(x = "Tick", y = scales::extended_breaks()(range(data_group_sum)))
      } else {
        tick_df <- data.frame(x = "Tick", y = tick_break)
      }
      # create plot
      rose_plot <- ggplot() +
        geom_bar(
          data = data, mapping = aes_string(x = "group", y = "count", fill = "group"),
          stat = "identity", color = border_color
        ) +
        coord_polar(theta = "x", start = start_pi, clip = "off") +
        theme_bw() +
        scale_fill_manual(values = fill_color) +
        theme(
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        guides(fill = guide_legend(title = group_key))
      # create donut info
      if (!is.null(donut_frac)) {
        inner_ymin <- max(data$Sum) * donut_frac * -1
        rose_plot <- rose_plot +
          geom_text(
            data = tick_df,
            aes(x = x, y = y, label = y), hjust = 0
          ) +
          scale_y_continuous(breaks = tick_df$y, limits = c(inner_ymin, NA))
        if (donut_label) {
          inner_median <- median(1:length(unique(as.character(data[, 1]))))
          rose_plot <- rose_plot + annotate("text",
            x = inner_median, y = inner_ymin, label = paste0("Total: ", sum(data$Sum)),
            size = donut_label_size, colour = donut_label_color
          )
        }
      } else {
        rose_plot <- rose_plot +
          geom_text(
            data = tick_df,
            aes(x = x, y = y, label = y), hjust = 0
          ) +
          scale_y_continuous(breaks = tick_df$y)
      }
      # add label info
      # create label
      if (show_label) {
        # remove other labels
        data_group_num <- length(levels(data$group))
        x_text <- c("Tick", rep("", data_group_num - 1))
        names(x_text) <- levels(data$group)
        rose_plot <- rose_plot + scale_x_discrete(labels = x_text)
        # add label
        label_df <- data %>%
          dplyr::select(c("group", "Sum")) %>%
          dplyr::distinct()
        if (label_info == "count") {
          label_df$label <- as.character(label_df$Sum)
        } else if (label_info == "ratio") {
          label_df$label <- as.character(scales::percent(label_df$Sum / sum(label_df$Sum)))
        } else if (label_info == "all") {
          label_df$label <- paste0(label_df$Sum, " (", scales::percent(label_df$Sum / sum(label_df$Sum)), ")")
        }
        label_df$label <- paste(label_df$group, label_df$label, sep = label_sep)
        # calculate angle
        label_df$id <- 1:nrow(label_df)
        angle <- 90 - 360 * (label_df$id - 0.5) / nrow(label_df) - start_pi * 60
        label_df$hjust <- ifelse(angle < -90, 1, 0)
        label_df$angle <- ifelse(angle < -90, angle + 180, angle)
        if (!is.null(label_gap)) {
          label_df$Sum <- label_df$Sum + label_gap * label_df$Sum
        }
        rose_plot <- rose_plot +
          geom_text(
            data = label_df[label_df$group != "Tick", ],
            mapping = aes(
              x = group, y = Sum, label = label, angle = angle,
              hjust = hjust
            ), colour = label_color,
            show.legend = FALSE, size = label_size
          )
      }
    } else {
      data$Sum <- data$count
      # sort the data
      if (sort) {
        data <- data %>%
          dplyr::arrange(Sum) %>%
          as.data.frame()
        data$group <- factor(data$group, levels = unique(as.character(data$group)))
      }
      rose_plot <- ggplot() +
        geom_bar(
          data = data, mapping = aes_string(x = "group", y = "count", fill = "group"),
          stat = "identity", color = border_color
        ) +
        coord_polar(theta = "x", start = 0, clip = "off") +
        theme_bw() +
        scale_fill_manual(values = fill_color) +
        theme(
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        guides(fill = guide_legend(title = group_key))
      # create donut info
      if (!is.null(donut_frac)) {
        inner_ymin <- max(data$Sum) * donut_frac * -1
        rose_plot <- rose_plot +
          scale_y_continuous(limits = c(inner_ymin, NA))
        if (donut_label) {
          inner_median <- median(1:length(unique(as.character(data[, 1]))))
          rose_plot <- rose_plot + annotate("text",
            x = inner_median, y = inner_ymin, label = paste0("Total: ", sum(data$Sum)),
            size = donut_label_size, colour = donut_label_color
          )
        }
      }
      # add label info
      # create label
      if (show_label) {
        # remove x tick labels
        rose_plot <- rose_plot + theme(axis.text.x = element_blank())
        # add label
        label_df <- data %>%
          dplyr::select(c("group", "Sum")) %>%
          dplyr::distinct()
        if (label_info == "count") {
          label_df$label <- as.character(label_df$Sum)
        } else if (label_info == "ratio") {
          label_df$label <- as.character(scales::percent(label_df$Sum / sum(label_df$Sum)))
        } else if (label_info == "all") {
          label_df$label <- paste0(label_df$Sum, " (", scales::percent(label_df$Sum / sum(label_df$Sum)), ")")
        }
        label_df$label <- paste(label_df$group, label_df$label, sep = label_sep)
        # calculate angle
        label_df$id <- 1:nrow(label_df)
        angle <- 90 - 360 * (label_df$id - 0.5) / nrow(label_df)
        label_df$hjust <- ifelse(angle < -90, 1, 0)
        label_df$angle <- ifelse(angle < -90, angle + 180, angle)
        if (!is.null(label_gap)) {
          label_df$Sum <- label_df$Sum + label_gap * label_df$Sum
        }
        rose_plot <- rose_plot +
          geom_text(
            data = label_df,
            mapping = aes(
              x = group, y = Sum, label = label, angle = angle,
              hjust = hjust
            ), colour = label_color,
            show.legend = FALSE, size = label_size
          )
      }
    }
  } else if (length(group_key) == 2) {
    if (!all(group_key %in% colnames(data))) {
      stop("Not all group columns are in data.")
    } else {
      # stat data
      data[group_key] <- apply(data[group_key], 2, as.character)
      if (count_type == "full") {
        data <- data %>%
          dplyr::group_by(across(all_of(group_key))) %>%
          dplyr::summarise(count = n()) %>%
          as.data.frame()
      } else {
        if (!"count" %in% colnames(data)) {
          stop("count column is missing in your data.")
        }
      }
      data <- data %>%
        dplyr::group_by(across(group_key[1])) %>%
        dplyr::mutate(Sum = sum(count)) %>%
        as.data.frame()
      # with two group variable, color by subgroup and with single label color
      # preapare fill color
      all_subgroups <- unique(as.character(data[, group_key[2]]))
      if (is.null(fill_color)) {
        getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
        color_len <- length(all_subgroups)
        fill_color <- getPalette(color_len)
      } else if (length(fill_color) != length(all_subgroups)) {
        stop("The length of fill color is greater than 1 and not equal to group number.")
      }
      names(fill_color) <- all_subgroups
      # prepare label color
      if (is.null(label_color)) {
        label_color <- "black"
      } else if (length(label_color) > 1) {
        label_color <- label_color[1]
      }
      # add tick info
      if (show_tick) {
        # add new row
        add_row <- data.frame("Tick", NA, 0, 0)
        colnames(add_row) <- colnames(data)
        data <- rbind(add_row, as.data.frame(data)) %>% as.data.frame()
        # sort the data
        if (sort) {
          data <- data %>%
            dplyr::arrange(Sum) %>%
            as.data.frame()
          data[, group_key[1]] <- factor(data[, group_key[1]], levels = unique(as.character(data[, group_key[1]])))
        }
        start_pi <- -pi / length(levels(data[, group_key[1]]))
        data_group_sum <- unique(data$Sum)
        # create tick break
        if (is.null(tick_break)) {
          tick_df <- data.frame(x = "Tick", y = scales::extended_breaks()(range(data_group_sum)))
        } else {
          tick_df <- data.frame(x = "Tick", y = tick_break)
        }
        # create plot
        rose_plot <- ggplot() +
          geom_bar(
            data = data, mapping = aes_string(x = group_key[1], y = "count", fill = group_key[2]),
            stat = "identity", color = border_color
          ) +
          coord_polar(theta = "x", start = start_pi, clip = "off") +
          theme_bw() +
          scale_fill_manual(values = fill_color) +
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          )
        # create donut info
        if (!is.null(donut_frac)) {
          inner_ymin <- max(data$Sum) * donut_frac * -1
          rose_plot <- rose_plot +
            geom_text(
              data = tick_df,
              aes(x = x, y = y, label = y), hjust = 0
            ) +
            scale_y_continuous(breaks = tick_df$y, limits = c(inner_ymin, NA))
          if (donut_label) {
            inner_median <- median(1:length(unique(as.character(data[, 1]))))
            rose_plot <- rose_plot + annotate("text",
              x = inner_median, y = inner_ymin, label = paste0("Total: ", sum(data$Sum)),
              size = donut_label_size, colour = donut_label_color
            )
          }
        } else {
          rose_plot <- rose_plot +
            geom_text(
              data = tick_df,
              aes(x = x, y = y, label = y), hjust = 0
            ) +
            scale_y_continuous(breaks = tick_df$y)
        }

        # add label info
        # create label
        if (show_label) {
          # remove other labels
          data_group_num <- length(levels(data[, group_key[1]]))
          x_text <- c("Tick", rep("", data_group_num - 1))
          names(x_text) <- levels(data[, group_key[1]])
          rose_plot <- rose_plot + scale_x_discrete(labels = x_text)
          # add label
          label_df <- data %>%
            dplyr::select(c(group_key[1], "Sum")) %>%
            dplyr::distinct()
          colnames(label_df) <- c("group", "Sum")
          if (label_info == "count") {
            label_df$label <- as.character(label_df$Sum)
          } else if (label_info == "ratio") {
            label_df$label <- as.character(scales::percent(label_df$Sum / sum(label_df$Sum)))
          } else if (label_info == "all") {
            label_df$label <- paste0(label_df$Sum, " (", scales::percent(label_df$Sum / sum(label_df$Sum)), ")")
          }
          label_df$label <- paste(label_df$group, label_df$label, sep = label_sep)
          # calculate angle
          label_df$id <- 1:nrow(label_df)
          angle <- 90 - 360 * (label_df$id - 0.5) / nrow(label_df) - start_pi * 60
          label_df$hjust <- ifelse(angle < -90, 1, 0)
          label_df$angle <- ifelse(angle < -90, angle + 180, angle)
          if (!is.null(label_gap)) {
            label_df$Sum <- label_df$Sum + label_gap * label_df$Sum
          }
          rose_plot <- rose_plot +
            geom_text(
              data = label_df[label_df$group != "Tick", ],
              mapping = aes(
                x = group, y = Sum, label = label, angle = angle,
                hjust = hjust
              ), colour = label_color,
              show.legend = FALSE, size = label_size
            )
        }
      } else {
        # sort the data
        if (sort) {
          data <- data %>%
            dplyr::arrange(Sum) %>%
            as.data.frame()
          data[, group_key[1]] <- factor(data[, group_key[1]], levels = unique(as.character(data[, group_key[1]])))
        }
        rose_plot <- ggplot() +
          geom_bar(
            data = data, mapping = aes_string(x = group_key[1], y = "count", fill = group_key[2]),
            stat = "identity", color = border_color
          ) +
          coord_polar(theta = "x", start = 0, clip = "off") +
          theme_bw() +
          scale_fill_manual(values = fill_color) +
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          )
        # create donut info
        if (!is.null(donut_frac)) {
          inner_ymin <- max(data$Sum) * donut_frac * -1
          rose_plot <- rose_plot +
            scale_y_continuous(limits = c(inner_ymin, NA))
          if (donut_label) {
            inner_median <- median(1:length(unique(as.character(data[, 1]))))
            rose_plot <- rose_plot + annotate("text",
              x = inner_median, y = inner_ymin, label = paste0("Total: ", sum(data$Sum)),
              size = donut_label_size, colour = donut_label_color
            )
          }
        }
        # add label info
        # create label
        if (show_label) {
          # remove x tick labels
          rose_plot <- rose_plot + theme(axis.text.x = element_blank())
          # add label
          label_df <- data %>%
            dplyr::select(c(group_key[1], "Sum")) %>%
            dplyr::distinct()
          colnames(label_df) <- c("group", "Sum")
          if (label_info == "count") {
            label_df$label <- as.character(label_df$Sum)
          } else if (label_info == "ratio") {
            label_df$label <- as.character(scales::percent(label_df$Sum / sum(label_df$Sum)))
          } else if (label_info == "all") {
            label_df$label <- paste0(label_df$Sum, " (", scales::percent(label_df$Sum / sum(label_df$Sum)), ")")
          }
          label_df$label <- paste(label_df$group, label_df$label, sep = label_sep)
          # calculate angle
          label_df$id <- 1:nrow(label_df)
          angle <- 90 - 360 * (label_df$id - 0.5) / nrow(label_df)
          label_df$hjust <- ifelse(angle < -90, 1, 0)
          label_df$angle <- ifelse(angle < -90, angle + 180, angle)
          if (!is.null(label_gap)) {
            label_df$Sum <- label_df$Sum + label_gap * label_df$Sum
          }
          rose_plot <- rose_plot +
            geom_text(
              data = label_df,
              mapping = aes(
                x = group, y = Sum, label = label, angle = angle,
                hjust = hjust
              ), colour = label_color,
              show.legend = FALSE, size = label_size
            )
        }
      }
    }
  } else {
    stop("Please provide up to two group variables.")
  }
  return(rose_plot)
}
