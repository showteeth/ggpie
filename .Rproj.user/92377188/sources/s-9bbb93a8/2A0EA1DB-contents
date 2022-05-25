# modify from https://github.com/coolbutuseless/ggthreed/blob/master/R/geom-threedpie.R
darken_colours <- function(hex_colour, amount = 0.15) {
  if (amount < 0 || amount > 1) {
    stop("darken_colours(): amount must be beween 0 and 1.")
  }
  return(rgb(t(col2rgb(hex_colour) * (1 - amount)), maxColorValue = 255))
}

# stolen from https://github.com/coolbutuseless/ggthreed/blob/master/R/create-pie-objects.R
create_pie_objects <- function(labels, counts,
                               start_degrees = 0,
                               tilt_degrees = 0,
                               height = 0.1,
                               camera_eye = c(0, 3, 5),
                               camera_look_at = c(0, 0, 0)) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What are all the angles that have a point on the circumference.
  # Bigger numbers mean a coarser looking pie.
  # Can't see any artefacts at 2 degrees, so going to use that as the slice
  # size
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle <- seq(0, 359, 2)
  N <- length(angle)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the counts into an angle cutoff
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle_cutoffs <- round(360 * counts / sum(counts), 0)
  groups <- cut(angle, breaks = cumsum(c(0, angle_cutoffs)), labels = FALSE, include.lowest = TRUE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Apply extra rotation by 'start degrees'
  # - Initial 90 rotation is to start the pie at the 12 o'clock position rather
  #   than the 3 o'clock position
  # - Reverse angle so that pie-pieces go clockwise-by-label
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle <- -(angle - 90 + start_degrees)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # (x, y) coordinates around circumference
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- cos(angle * pi / 180)
  y <- sin(angle * pi / 180)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Matrix of vertices for the faces on the top of the pie
  # - Put the centre index at the start
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertices_top <- t(unname(cbind(x, y, z = 0, w = 1)))
  vertices_top <- cbind(c(0, 0, 0, 1), vertices_top)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix of vertices for the edge of the bottom of the pie
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertices_bot <- vertices_top
  vertices_bot[3, ] <- -abs(height)

  vertices <- cbind(vertices_top, vertices_bot)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix of vertex indices for each triangular face on top
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rotr <- function(x) {
    c(tail(x, 1), head(x, -1))
  }
  rotl <- function(x) {
    c(tail(x, -1), head(x, 1))
  }

  it1 <- seq(1, N)
  it2 <- rotl(it1)

  it1 <- it1 + 1L # offset from first vertex which is the zero point
  it2 <- it2 + 1L # offset from first vertex which is the zero point

  it <- t(unname(cbind(it1, it2, 1L)))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Vertex indices for quads around side of the pie
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ib1 <- seq(2, N + 1)
  ib2 <- rotl(ib1)

  ib3 <- ib1 + ncol(vertices_top)
  ib4 <- rotl(ib3)

  ib <- t(unname(cbind(ib1, ib2, ib4, ib3)))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Properties for each element. Needs 1 row for each tri element on top and
  # each quaad along the side
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  properties <- tibble::tibble(
    label = c(labels[groups], labels[groups]),
    group = as.integer(as.factor(label))
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a mesh3d object of the pie
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pie_obj <- list(
    vb            = vertices,
    it            = it,
    ib            = ib,
    primitivetype = "quad",
    material      = list(),
    properties    = properties,
    texcoords     = NULL
  )

  class(pie_obj) <- c("mesh3d", "shaped3d")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Define where the camera is looking
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  camera_to_world <- threed::look_at_matrix(eye = camera_eye, at = camera_look_at)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Initial xyz space is xy in the screen plane and z coming out of the screen
  #   i.e. right-handed coordinate system.
  # - Rotate the pie from the x/y into the x/z plane i.e. laying flat.
  # - Further rotate the pie by the tilt angle, where a positive tile brings the
  #   back edge of the pie towards the camera
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tilt_radians <- tilt_degrees * pi / 180
  pie_obj <- pie_obj %>%
    rotate_by(-pi / 2 + tilt_radians, c(1, 0, 0)) %>%
    transform_by(invert_matrix(camera_to_world)) %>%
    perspective_projection()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create and return a mesh3dlist of objects. In this case there's only
  # one object, but for more complex 3d plot types there may be more.
  # Also I may want to add text labels to the pie slices later.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mesh3dlist(
    pie = pie_obj
  )
}

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
#' @param label_info Label information type, chosen from count, ratio and all (count and ratio). Default: count.
#' @param label_split Pattern used to split the label, support regular expression. Default: space.
#' @param label_size Size of the label. Default: 4.
#'
#' @return A ggplot2 object.
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette col2rgb rgb
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent
#' @import ggplot2
#' @import threed
#' @importFrom utils head tail
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
#'   tilt_degrees = -10, start_degrees = 0
#' )
#' data <- data.frame(group = letters[1:5], count = c(1, 2, 3, 1, 1), stringsAsFactors = FALSE)
#' ggpie3D(data = data, start_degrees = 0, label_split = NULL)
ggpie3D <- function(data, group_key = NULL, count_type = c("count", "full"), fill_color = NULL, start_degrees = 0, tilt_degrees = -20,
                    height = 0.1, darken = 0.15, camera_eye = c(0, 3, 5), camera_look_at = c(0, 0, 0), show_label = TRUE,
                    label_info = c("count", "ratio", "all"), label_split = "[[:space:]]+", label_size = 4) {
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
  label_info <- "all"
  if (label_info == "count") {
    data$label <- as.character(data$count)
  } else if (label_info == "ratio") {
    data$label <- as.character(scales::percent(data$count / sum(data$count)))
  } else if (label_info == "all") {
    data$label <- paste0(data$count, " (", scales::percent(data$count / sum(data$count)), ")")
  }
  # split label
  if (!is.null(label_split)) {
    data$label <- gsub(pattern = label_split, replacement = "\n", x = data$label)
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
        aes(label = label, y = y, x = x), show.legend = F,
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
