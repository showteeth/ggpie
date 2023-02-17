PrepareData <- function(data, group_key = NULL, count_type = c("count", "full"), fill_color = NULL, label_info = c("group", "count", "ratio"),
                        label_split = "[[:space:]]+", label_len = 40, label_color = "black") {
  # check parameters
  count_type <- match.arg(arg = count_type)

  # create plot data frame
  ## get group key
  if (is.null(group_key)) {
    if (!"group" %in% colnames(data)) {
      stop("Group information is missing in your data.")
    } else {
      group_key <- "group"
    }
  }
  ## get group factors
  if (is.null(levels(data[[group_key]]))) {
    data <- data %>% dplyr::mutate(group = as.character(.data[[group_key]]))
    data$group <- factor(data$group, levels = unique(data$group))
  } else {
    data.levels <- levels(data[[group_key]])
    data <- data %>% dplyr::mutate(group = as.character(.data[[group_key]]))
    data$group <- factor(data$group, levels = data.levels)
  }

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
