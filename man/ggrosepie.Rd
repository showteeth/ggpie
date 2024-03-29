% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggrosepie.R
\name{ggrosepie}
\alias{ggrosepie}
\title{Create rose pie plot.}
\usage{
ggrosepie(
  data,
  group_key = NULL,
  count_type = c("count", "full"),
  fill_color = NULL,
  label_info = c("count", "ratio", "all"),
  label_split = NULL,
  label_len = 40,
  label_color = "black",
  sort = TRUE,
  show_tick = TRUE,
  tick_break = NULL,
  show_label = TRUE,
  label_sep = "|",
  label_gap = 0.05,
  label_size = 4,
  donut_frac = 0.1,
  donut_label = TRUE,
  donut_label_size = 4,
  donut_label_color = "red",
  border_color = "black",
  border_size = 1
)
}
\arguments{
\item{data}{Data frame contains full data or summarized data.}

\item{group_key}{Column used to summarize the data, one or two are acceptable. Default: NULL.}

\item{count_type}{Data frame type, chosen from "count" and "full". "count" means summarized data and "full" means full data. Default: count.}

\item{fill_color}{Colors used. When length of \code{group_key} is two, color the subgroup, otherwise the main group. Default: NULL (conduct automatic selection).}

\item{label_info}{Label information type of outer pie plot, chosen from count, ratio and all (count and ratio). Default: count.}

\item{label_split}{Pattern used to split the label, support regular expression. Default: NULL.}

\item{label_len}{The length of label text. Used when \code{label_split} is NULL. Default: 40.}

\item{label_color}{Color of the label. When length of \code{group_key} is two, this should be set to one color. Default: black.}

\item{sort}{Logical value, whether to order the plot by counts. Default: TRUE.}

\item{show_tick}{Logical value, whether to show the tick. Default: TRUE.}

\item{tick_break}{The break of tick. Default: NULL (conduct automatic selection).}

\item{show_label}{Logical value, whether to show the label. Default: TRUE.}

\item{label_sep}{The separator between group and count info. Default: |.}

\item{label_gap}{The gap between label and plot. Default: 0.05 (count + 0.05*count).}

\item{label_size}{The size of label. Default: 4.}

\item{donut_frac}{The fraction of donut. Default: 0.1 (0.1*max(count)).}

\item{donut_label}{Logical value, whether to show total number in the center of the plot. Default: TRUE.}

\item{donut_label_size}{The label size of center label. Default: 4.}

\item{donut_label_color}{The color of center label. Default: red.}

\item{border_color}{Border color. Default: black.}

\item{border_size}{Border thickness. Default: 1.}
}
\value{
A ggplot2 object.
}
\description{
Create rose pie plot.
}
\examples{
library(ggpie)
library(ggplot2)
data(diamonds)
# do not show tick
ggrosepie(diamonds,
  group_key = "color", count_type = "full", label_info = "all",
  show_tick = FALSE, donut_frac = 0.3, donut_label_size = 3
)
# show tick and with automatic selection
ggrosepie(diamonds,
  group_key = "color", count_type = "full", label_info = "all",
  donut_frac = 0.3, donut_label_size = 3
)
# show tick and with specific break
ggrosepie(diamonds,
  group_key = "color", count_type = "full", label_info = "all",
  tick_break = c(3000, 5000, 7000, 11000), donut_frac = 0.3, donut_label_size = 3
)
# two group variable, and do not show tick
ggrosepie(diamonds,
  group_key = c("color", "clarity"),
  count_type = "full", label_info = "all",
  show_tick = FALSE, donut_frac = 0.3, donut_label_size = 3
)
# two group variable, show tick and with automatic selection
ggrosepie(diamonds,
  group_key = c("color", "clarity"),
  count_type = "full", label_info = "all",
  donut_frac = 0.3, donut_label_size = 3
)
# two group variable, show tick and with specific break
ggrosepie(diamonds,
  group_key = c("color", "clarity"),
  count_type = "full", label_info = "all",
  tick_break = c(3000, 5000, 7000, 11000), donut_frac = 0.3, donut_label_size = 3
)
}
