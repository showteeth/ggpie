PrepareData <- function(data, group_key = NULL, count_type = c("count", "full"), fill_color = NULL, label_info = c("count", "ratio", "all"),
                        label_split = "[[:space:]]+", label_color = "black") {
  # check parameters
  count_type <- match.arg(arg = count_type)
  label_info <- match.arg(arg = label_info)

  # create plot data frame
  ## get group key
  if (is.null(group_key)) {
    if (!"group" %in% colnames(data)) {
      stop("Group information is missing in your data.")
    } else {
      group_key <- "group"
    }
  }
  data <- data %>% dplyr::mutate(group = as.character(.data[[group_key]]))
  # stat data
  if (count_type == "full") {
    data <- data %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(count = dplyr::n())
  } else {
    if (!"count" %in% colnames(data)) {
      stop("count column is missing in your data.")
    }
  }
  # create label
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
  # prepare fill color
  all_groups <- unique(as.character(data$group))
  if (is.null(fill_color)) {
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
    color_len <- length(all_groups)
    fill_color <- getPalette(color_len)
  } else if (length(fill_color) != length(all_groups)) {
    stop("The length of fill color is greater than 1 and not equal to group number.")
  }
  names(fill_color) <- all_groups
  # prepare label color
  if (is.null(label_color)) {
    label_color <- fill_color
  } else {
    if (length(label_color) == 1) {
      label_color <- rep(label_color, length(all_groups))
    } else if (length(label_color) != length(all_groups)) {
      stop("The length of label color is greater than 1 and not equal to group number.")
    }
    names(label_color) <- all_groups
  }
  plot.data <- list(data = data, fill_color = fill_color, label_color = label_color)
  return(plot.data)
}
