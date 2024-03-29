#' Create donut plot.
#'
#' @param data Data frame contains full data or summarized data.
#' @param group_key Column used to summarize the data. Default: NULL.
#' @param count_type Data frame type, chosen from "count" and "full". "count" means summarized data and "full" means full data. Default: count.
#' @param fill_color Colors used. Default: NULL (conduct automatic selection).
#' @param label_info Label information type, combine from group, count, ratio.
#' For example, use "count" , "ratio" will show count and ratio, count is main label info, ratio is in brackets. Default: count.
#' @param label_split Pattern used to split the label, support regular expression. Default: space.
#' @param label_len The length of label text. Used when \code{label_split} is NULL. Default: 40.
#' @param label_color Color of the label. Default: black.
#' @param label_type Label style, chosen from circle, horizon and none (no label). Default: circle.
#' @param label_pos Label position, chosen from in and out. Default: in.
#' @param label_gap Gap between label and pie plot, used when \code{label_pos} is out.
#' @param label_threshold Threshold of the ratio to determine label position (in/out pie). Default: NULL.
#' @param label_size Size of the label. Default: 4.
#' @param border_color Border color. Default: black.
#' @param border_size Border thickness. Default: 1.
#' @param r0 The radius of inner blank circle. Default: 1.
#' @param r1 The radius of outer circle. Default: 3.
#' @param donut.label Logical value, whether to show total number in the center of the plot. Default: TRUE.
#' @param donut.label.size The label size of center label. Default: 4.
#' @param donut.label.color The color of center label. Default: red.
#' @param nudge_x Parameter of \code{\link{geom_text_repel}}. Default: 1.
#' @param nudge_y Parameter of \code{\link{geom_text_repel}}. Default: 1.
#'
#' @return A ggplot2 object.
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#'
#' @examples
#' library(ggpie)
#' library(ggplot2)
#' data(diamonds)
#' # circle label and out of pie
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "circle",
#'   label_size = 4, label_pos = "out"
#' )
#' # circle label and in pie plot, with no split
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "circle", label_split = NULL,
#'   label_size = 4, label_pos = "in"
#' )
#' # horizon label and in pie plot, with no split
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "horizon", label_split = NULL,
#'   label_size = 4, label_pos = "in"
#' )
#' # horizon label and in pie plot
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "horizon",
#'   label_size = 4, label_pos = "in"
#' )
#' # horizon label and out of pie plot, with no split
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "horizon", label_split = NULL,
#'   label_size = 4, label_pos = "out"
#' )
#' # horizon label and out of pie plot
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "horizon",
#'   label_size = 4, label_pos = "out"
#' )
#' # with label threshold
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "horizon", label_split = NULL,
#'   label_size = 4, label_pos = "in", label_threshold = 10
#' )
#' ggdonut(
#'   data = diamonds, group_key = "cut", count_type = "full",
#'   label_info = c("count", "ratio"), label_type = "horizon",
#'   label_size = 4, label_pos = "in", label_threshold = 10
#' )
ggdonut <- function(data, group_key = NULL, count_type = c("count", "full"), fill_color = NULL, label_info = "count",
                    label_split = "[[:space:]]+", label_len = 40, label_color = "black",
                    label_type = c("circle", "horizon", "none"), label_pos = c("in", "out"), label_gap = 0.05,
                    label_threshold = NULL, label_size = 4, border_color = "black", border_size = 1,
                    r0 = 1, r1 = 3, donut.label = TRUE, donut.label.size = 4, donut.label.color = "red",
                    nudge_x = 1, nudge_y = 1) {
  # check parameters
  count_type <- match.arg(arg = count_type)
  label_type <- match.arg(arg = label_type)
  label_pos <- match.arg(arg = label_pos)

  # prepare plot data
  plot.data <- PrepareData(
    data = data, group_key = group_key, count_type = count_type, fill_color = fill_color,
    label_info = label_info, label_split = label_split, label_len = label_len, label_color = label_color
  )
  data <- plot.data[["data"]]
  fill_color <- plot.data[["fill_color"]]
  label_color <- plot.data[["label_color"]]

  # create circle label plot
  if (label_type == "circle") {
    data$preangle <- (cumsum(data$count) - 0.5 * data$count) / sum(data$count) * 360
    data$angle <- data$preangle %% 180 - 90
    data$CumSum <- rev(round(cumsum(rev(data$count)) - rev(data$count / 2), 2))
    if (label_pos == "out") {
      pie_plot <- ggplot() +
        geom_bar(data,
          mapping = aes(x = (r0 + r1) / 2, y = count, fill = group),
          width = r1 - r0, stat = "identity", color = border_color, size = border_size
        ) +
        xlim(0, NA) +
        geom_text(data,
          mapping = aes(x = r1 + label_gap, y = CumSum, label = label, angle = angle, colour = group), show.legend = FALSE,
          hjust = ifelse(data$preangle > 180, 0, 1), size = label_size
        ) +
        coord_polar(theta = "y", start = 0, clip = "off") +
        theme_void() +
        scale_fill_manual(values = fill_color) +
        scale_colour_manual(values = label_color)
    } else if (label_pos == "in") {
      pie_plot <- ggplot() +
        geom_bar(data,
          mapping = aes(x = (r0 + r1) / 2, y = count, fill = group),
          width = r1 - r0, stat = "identity", color = border_color, size = border_size
        ) +
        xlim(0, NA) +
        geom_text(data,
          mapping = aes(x = (r0 + r1) / 2, y = CumSum, label = label, angle = angle, colour = group),
          show.legend = FALSE, size = label_size
        ) +
        coord_polar(theta = "y", start = 0, clip = "off") +
        theme_void() +
        scale_fill_manual(values = fill_color) +
        scale_colour_manual(values = label_color)
    }
  }
  # create horizon label plot
  if (label_type == "horizon") {
    data <- data %>%
      dplyr::mutate(Freq = count * 100 / sum(count)) %>%
      dplyr::mutate(CumFreq = rev(round(cumsum(rev(Freq)) - rev(Freq / 2), 2)))
    if (label_pos == "out") {
      pie_plot <- ggplot() +
        geom_bar(data,
          mapping = aes(x = (r0 + r1) / 2, y = Freq, fill = group),
          width = r1 - r0, stat = "identity", color = border_color, size = border_size
        ) +
        xlim(0, NA) +
        geom_text_repel(
          data = data,
          mapping = aes(label = label, y = CumFreq, x = after_stat(r1), colour = group), show.legend = FALSE,
          size = label_size, point.padding = NA, max.overlaps = Inf, nudge_x = nudge_x, nudge_y = nudge_y,
          segment.curvature = -0.2, segment.ncp = 10, segment.angle = 20
        ) +
        coord_polar(theta = "y", start = 0, clip = "off") +
        theme_void() +
        scale_fill_manual(values = fill_color) +
        scale_colour_manual(values = label_color)
    } else if (label_pos == "in") {
      if (is.null(label_threshold)) {
        pie_plot <- ggplot() +
          geom_bar(data,
            mapping = aes(x = (r0 + r1) / 2, y = Freq, fill = group),
            width = r1 - r0, stat = "identity", color = border_color, size = border_size
          ) +
          xlim(0, NA) +
          geom_text_repel(
            data = data,
            mapping = aes(x = (r0 + r1) / 2, y = CumFreq, label = label, colour = group),
            show.legend = FALSE, size = label_size
          ) +
          coord_polar(theta = "y", start = 0, clip = "off") +
          theme_void() +
          scale_fill_manual(values = fill_color) +
          scale_colour_manual(values = label_color)
      } else {
        pie_plot <- ggplot() +
          geom_bar(data,
            mapping = aes(x = (r0 + r1) / 2, y = Freq, fill = group),
            width = r1 - r0, stat = "identity", color = border_color, size = border_size
          ) +
          xlim(0, NA) +
          geom_text_repel(
            data = data[data$Freq < label_threshold, ],
            aes(label = label, y = CumFreq, x = after_stat(r1), colour = group), show.legend = FALSE,
            size = label_size, point.padding = NA, max.overlaps = Inf, nudge_x = nudge_x, nudge_y = nudge_y,
            segment.curvature = -0.2, segment.ncp = 10, segment.angle = 20
          ) +
          geom_text(
            data = data[data$Freq >= label_threshold, ],
            aes(y = CumFreq, x = (r0 + r1) / 2, label = label, colour = group),
            show.legend = FALSE, size = label_size
          ) +
          coord_polar(theta = "y", start = 0, clip = "off") +
          theme_void() +
          scale_fill_manual(values = fill_color) +
          scale_colour_manual(values = label_color)
      }
    }
  }
  # create with no label
  if (label_type == "none") {
    pie_plot <- ggplot() +
      geom_bar(data,
        mapping = aes(x = (r0 + r1) / 2, y = count, fill = group),
        width = r1 - r0, stat = "identity", color = border_color, size = border_size
      ) +
      xlim(0, NA) +
      coord_polar(theta = "y", start = 0, clip = "off") +
      theme_void() +
      scale_fill_manual(values = fill_color)
  }
  if (donut.label) {
    pie_plot <- pie_plot + annotate("text",
      x = 0, y = 0, label = paste0("Total: ", sum(data$count)),
      size = donut.label.size, colour = donut.label.color
    )
  }
  pie_plot <- pie_plot + theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
  return(pie_plot)
}
