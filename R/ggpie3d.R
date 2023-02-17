adapt2polygon <- function(data, fill_color = NULL, start_degrees = 0, tilt_degrees = -20,
                          height = 0.1, darken = 0.15, camera_eye = c(0, 3, 5), camera_look_at = c(0, 0, 0)) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pie mesh3d objects
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  objs <- create_pie_objects(data$group, data$count,
    start_degrees  = start_degrees,
    tilt_degrees   = tilt_degrees,
    height         = height,
    camera_eye     = camera_eye,
    camera_look_at = camera_look_at
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the mesh3d objects to data.frames
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pie <- as.data.frame(objs$pie)
  pie$group <- pie$label

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare color used
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_groups <- unique(as.character(pie$group))
  if (is.null(fill_color)) {
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    color_len <- length(all_groups)
    fill_color <- getPalette(color_len)
  }
  names(fill_color) <- all_groups
  polygon_df <- pie %>% dplyr::mutate(fill = fill_color[group])
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Darken the quads that make up the side of the pie
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygon_df <- polygon_df %>%
    dplyr::mutate(
      fill   = ifelse(element_type == 4, darken_colours(fill, amount = darken), fill),
      colour = fill
    )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare plot label
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygon_df <- polygon_df %>%
    dplyr::mutate(group = ifelse(element_type == 4, paste0(group, element_type), as.character(group)))

  polygon_df <- polygon_df %>% dplyr::arrange(element_id)
  return(polygon_df)
}


#' Create 3D pie plot.
#'
#' @param data Data frame contains full data or summarized data.
#' @param group_key Column used to summarize the data. Default: NULL.
#' @param count_type Data frame type, chosen from "count" and "full". "count" means summarized data and "full" means full data. Default: count.
#' @param fill_color Colors used. Default: NULL (conduct automatic selection).
#' @param start_degrees starting angle for first pie slice (in degrees). Default: 0.
#' @param tilt_degrees angle by which to tilt the pie towards the camera (in degrees). Default: 0.
#' @param height height of the pie. Default: 0.1.
#' @param darken Shadow degree. Default: 0.15.
#' @param camera_eye location of camera eye. Default: c(0, 3, 5).
#' @param camera_look_at at what point is the camera looking. Default: c(0, 0, 0).
#' @param show_label Logical value, whether to show label or not. Default: TRUE.
#' @param label_info Label information type, combine from group, count, ratio.
#' For example, use "count" , "ratio" will show count and ratio, count is main label info, ratio is in brackets. Default: count.
#' @param label_split Pattern used to split the label, support regular expression. Default: space.
#' @param label_len The length of label text. Used when \code{label_split} is NULL. Default: 40.
#' @param label_size Size of the label. Default: 4.
#'
#' @return A ggplot2 object.
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette col2rgb rgb
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @importFrom utils head tail
#' @importFrom stats aggregate setNames
#' @importFrom ggrepel geom_text_repel
#' @export
#'
#' @examples
#' library(ggpie)
#' library(ggplot2)
#' data(diamonds)
#' ggpie3D(data = diamonds, group_key = "cut", count_type = "full", tilt_degrees = -10)
#' ggpie3D(
#'   data = mtcars, group_key = "cyl", count_type = "full",
#'   tilt_degrees = -10, start_degrees = 0, label_info = c("count", "ratio")
#' )
#' data <- data.frame(group = letters[1:5], count = c(1, 2, 3, 1, 1), stringsAsFactors = FALSE)
#' ggpie3D(data = data, start_degrees = 0, label_split = NULL, label_info = c("count", "ratio"))
ggpie3D <- function(data, group_key = NULL, count_type = c("count", "full"), fill_color = NULL, start_degrees = 0, tilt_degrees = -20,
                    height = 0.1, darken = 0.15, camera_eye = c(0, 3, 5), camera_look_at = c(0, 0, 0), show_label = TRUE,
                    label_info = "count", label_split = "[[:space:]]+", label_len = 40, label_size = 4) {
  # check parameters
  count_type <- match.arg(arg = count_type)

  # create plot data frame
  if (is.null(group_key)) {
    if (!"group" %in% colnames(data)) {
      stop("Group information is missing in your data.")
    } else {
      group_key <- "group"
    }
  }
  data <- data %>% dplyr::mutate(group = as.character(.data[[group_key]]))
  if (count_type == "full") {
    data <- data %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(count = dplyr::n())
  } else {
    if (!"count" %in% colnames(data)) {
      stop("count column is missing in your data.")
    }
  }
  plot_data <- adapt2polygon(
    data = data, fill_color = fill_color, start_degrees = start_degrees, tilt_degrees = tilt_degrees,
    height = height, darken = darken, camera_eye = camera_eye, camera_look_at = camera_look_at
  )
  # preapare color used
  color_used <- plot_data$fill
  names(color_used) <- plot_data$group
  # get groups
  pie_data <- plot_data %>% dplyr::filter(element_type != 4)
  label_group <- unique(as.character(pie_data$group))
  # prepare label
  valid_label_info <- intersect(c("group", "count", "ratio"), label_info)
  if (length(valid_label_info) < 1) {
    stop("Please provide valid label_info, choose from 'group', 'count', 'ratio'.")
  } else {
    label_list <- list(
      count = as.character(data$count),
      ratio = as.character(scales::percent(data$count / sum(data$count))),
      group = as.character(data$group)
    )
    if (length(valid_label_info) == 1) {
      label_vec <- label_list[[valid_label_info]]
    } else if (length(valid_label_info) == 2) {
      label_vec <- paste0(label_list[[valid_label_info[1]]], " (", label_list[[valid_label_info[2]]], ")")
    } else if (length(valid_label_info) == 3) {
      label_vec <- paste0(label_list[[valid_label_info[1]]], " (", paste(label_list[[valid_label_info[2]]], label_list[[valid_label_info[3]]], sep = " ,"), ")")
    }
    data$label <- label_vec
  }
  # split label or specify label length
  if (!is.null(label_split)) {
    data$label <- gsub(pattern = label_split, replacement = "\n", x = data$label)
  } else {
    if (!is.null(label_len)) {
      data$label <- stringr::str_wrap(data$label, width = label_len)
    }
  }
  # create label data frame
  anno_pos <- pie_data %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(Num = length(unique(element_id))) %>%
    dplyr::mutate(Median = floor((Num + 1) / 2)) %>%
    dplyr::mutate(Num = cumsum(Num))
  anno_pos$Add <- c(0, anno_pos$Num[1:nrow(anno_pos) - 1])
  anno_pos <- anno_pos %>% dplyr::mutate(element_id = Median + Add)
  anno_pos <- merge(anno_pos, pie_data %>% dplyr::filter(vorder == "1") %>% dplyr::select(c("element_id", "x", "y")), by = "element_id") %>%
    dplyr::select(-c("Median", "Add"))
  anno_data <- merge(anno_pos, data, by = "group")
  # create pie plot
  if (show_label) {
    pie_plot <- ggplot() +
      geom_polygon(data = plot_data, aes(x, y, group = zorder, fill = group, color = group)) +
      geom_text_repel(
        data = anno_data,
        aes(label = label, y = y, x = x), show.legend = FALSE,
        size = label_size, point.padding = NA, max.overlaps = Inf
      ) +
      scale_fill_manual(
        breaks = label_group,
        values = color_used
      ) +
      scale_color_manual(
        breaks = label_group,
        values = color_used
      ) +
      coord_equal(clip = "off") +
      theme_void() +
      labs(fill = group_key, colour = group_key)
  } else {
    pie_plot <- ggplot() +
      geom_polygon(data = plot_data, aes(x, y, group = zorder, fill = group, color = group)) +
      scale_fill_manual(
        breaks = label_group,
        values = color_used
      ) +
      scale_color_manual(
        breaks = label_group,
        values = color_used
      ) +
      coord_equal(clip = "off") +
      theme_void() +
      labs(fill = group_key, colour = group_key)
  }
  return(pie_plot)
}
