# the following codes are from threed (https://github.com/coolbutuseless/threed) and ggthreed (https://github.com/coolbutuseless/ggthreed)
# to avoid get error when uploading to CRAN

# Transform the vertex coordinates by the transformation matrix
transform_by <- function(x, transform_matrix) {
  UseMethod("transform_by")
}
transform_by.default <- function(x, transform_matrix) {
  stop("transform_by.default called on object with class: ", class(x))
}
transform_by.matrix <- function(x, transform_matrix) {
  if (ncol(x) == 4) {
    res <- x %*% t(transform_matrix)
    res <- res / res[, 4]
  } else if (nrow(x) == 4) {
    res <- transform_matrix %*% x
    res <- t(t(res) / res[4, ])
  } else {
    stop("Non-sane dimensions: ", deparse(dim(x)))
  }
  res
}
transform_by.mesh3d <- function(x, transform_matrix) {
  if (is.null(x$transform_matrix)) {
    x$transform_matrix <- transform_matrix
  } else {
    x$transform_matrix <- transform_matrix %*% x$transform_matrix
  }

  # In general, if you transform an object you should transform the normals
  # For my purposes, I'm just going to blank them out, and they'll have to
  # be recalcualted if needed.
  x$face_normals <- NULL
  x$normals <- NULL
  x
}

# Create a rotation matrix
rotation_matrix <- function(angle, v) {
  if (angle == 0) {
    return(identity_matrix())
  }

  u <- vec3_normalize(v[1:3])

  x <- u[1]
  y <- u[2]
  z <- u[3]
  c <- cos(angle)
  s <- sin(angle)
  t <- 1 - c

  matrix(c(
    t * x * x + c, t * x * y - s * z, t * x * z + s * y, 0,
    t * x * y + s * z, t * y * y + c, t * y * z - s * x, 0,
    t * x * z - s * y, t * y * z + s * x, t * z * z + c, 0,
    0, 0, 0, 1
  ), byrow = TRUE, ncol = 4)
}

# Rotate the given object
rotate_by <- function(x, angle, v) {
  transform_by(x, rotation_matrix(angle, v))
}

# Normalize a vector to be of unit length
vec3_normalize <- function(v) {
  v[1:3] / sqrt(sum(v[1:3]^2))
}

# Calculate the vector cross-product of 2 3d vectors
vec3_crossproduct <- function(v1, v2) {
  v1[c(2L, 3L, 1L)] * v2[c(3L, 1L, 2L)] - v1[c(3L, 1L, 2L)] * v2[c(2L, 3L, 1L)]
}

# Invert a matrix
invert_matrix <- function(mat) {
  solve(mat)
}

# Create a list of mesh3d objects
mesh3dlist <- function(...) {
  l <- list(...)
  class(l) <- "mesh3dlist"
  l
}

# Create a perspective projection matrix (symmetric frustrum)
perspective_projection_matrix <- function(w = 2, h = 2, n = 1, f = 10) {
  stopifnot(w > 0 && h > 0 && n > 0 && f > 0)

  matrix(c(
    2 * n / w, 0, 0, 0,
    0, 2 * n / h, 0, 0,
    0, 0, -(f + n) / (f - n), 2 * f * n / (f - n),
    0, 0, -1, 0
  ), byrow = TRUE, ncol = 4)
}

# Perspective projection (symmetric frustrum)
perspective_projection <- function(x, w = 2, h = 2, n = 1, f = 10) {
  transform_by(x, perspective_projection_matrix(w, h, n, f))
}

# Create a camera 'look-at' transformation matrix i.e. camera-to-world transform
look_at_matrix <- function(eye, at) {
  forward <- vec3_normalize(eye - at)
  right <- vec3_normalize(vec3_crossproduct(c(0, 1, 0), forward))
  up <- vec3_crossproduct(forward, right)

  matrix(c(
    right[1], up[1], forward[1], eye[1],
    right[2], up[2], forward[2], eye[2],
    right[3], up[3], forward[3], eye[3],
    0, 0, 0, 1
  ), byrow = TRUE, ncol = 4)
}

# Darken a hex colour by the given amount
darken_colours <- function(hex_colour, amount = 0.15) {
  if (amount < 0 || amount > 1) {
    stop("darken_colours(): amount must be beween 0 and 1.")
  }
  return(rgb(t(col2rgb(hex_colour) * (1 - amount)), maxColorValue = 255))
}

# Create a set of mesh3d objects representing a pie chart
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
  camera_to_world <- look_at_matrix(eye = camera_eye, at = camera_look_at)

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

# Convert mesh3d object to a data.frame representation
as.data.frame.mesh3d <- function(x, ...) {
  obj <- x

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # "Concretize" the transform of the object
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- actualize_transformation(obj)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate normals if feasible
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- add_normals(obj)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # vertices
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_vertices <- t(obj$vb)
  all_vertices <- all_vertices[, 1:3] / all_vertices[, 4]
  all_vertices <- cbind(all_vertices, seq(nrow(all_vertices)))
  colnames(all_vertices) <- c("x", "y", "z", "vertex")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What element types are present in the object. Can be multiple!
  # e.g. cuboctahedron is both quads and tris
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_types <- get_element_types(obj)


  obj_mats <- lapply(element_types, function(x) {
    get_matrix_for_element_type(obj, element_type = x, all_vertices = all_vertices)
  })
  obj_mat <- do.call(rbind, obj_mats)
  obj_df <- as.data.frame(obj_mat)

  obj_df <- transform(
    obj_df,
    element_type = as.integer(element_type),
    vorder       = as.integer(vorder),
    vertex       = as.integer(vertex),
    element_id   = as.integer(as.factor(interaction(element_id, element_type)))
  )


  obj_df <- add_zorder(obj_df)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Desireded column ordering
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  column_ordering <- c(
    "element_id", "element_type", "vorder", "x", "y", "z", "vertex",
    "vnx", "vny", "vnz",
    "fnx", "fny", "fnz",
    "fcx", "fcy", "fcz", "zorder"
  )
  column_ordering <- intersect(column_ordering, colnames(obj_df))
  obj_df <- obj_df[, c(column_ordering, setdiff(colnames(obj_df), column_ordering))]


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the face normal points towards the negative z axis, the it's hidden
  # (in the default perspectvve proj and orthographic proj)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ("fnz" %in% colnames(obj_df)) {
    obj_df$hidden <- obj_df$fnz < 0
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add in any properties by element_id
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(obj$properties)) {
    cols_to_add <- setdiff(colnames(obj$properties), colnames(obj_df))

    if (!"element_id" %in% names(obj$properties)) {
      obj$properties$element_id <- seq(nrow(obj$properties))
    }
    cols_to_add <- c("element_id", cols_to_add)

    if (length(cols_to_add) > 0L) {
      obj_df <- merge(obj_df, obj$properties[, cols_to_add, drop = FALSE], all.x = TRUE)
    }
  }


  obj_df
}

# Actualize a transform
actualize_transformation <- function(obj) {

  # Is there any transformation? if not, return the original object
  if (is.null(obj$transform_matrix)) {
    return(obj)
  }

  # otherwise transform the vertices
  vb <- obj$vb
  vb <- obj$transform_matrix %*% vb
  vb <- t(t(vb) / vb[4, ])
  obj$vb <- vb

  obj$transform_matrix <- NULL

  # recalculate normals if already present
  if (!is.null(obj$normals) | !is.null(obj$face_normals)) {
    obj <- add_normals(obj)
  }

  obj
}

# Add face and vertex normals to an object
add_normals <- function(x, ...) UseMethod("add_normals")

# Add face and vertex normals for mesh3d objects.
add_normals.mesh3d <- function(x, ...) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check: Only element_types 3 + 4 can have a normal
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_types <- get_element_types(x)
  if (length(intersect(element_types, 3:4)) == 0L) {
    return(x)
  }

  v <- x$vb

  # Make sure v is homogeneous with unit w
  if (nrow(v) == 3L) {
    v <- rbind(v, 1)
  } else {
    v <- t(t(v) / v[4L, ])
  }

  normals <- v * 0
  v <- v[1:3, ]

  if (length(x$it)) {
    it <- x$it
    face_normals <- matrix(0, ncol = ncol(it), nrow = 4)

    for (i in 1:ncol(it)) {
      normal <- vec3_normalize(vec3_crossproduct(
        v[, it[1, i]] - v[, it[3, i]],
        v[, it[2, i]] - v[, it[1, i]]
      ))
      face_normals[, i] <- c(normal, 1)
      if (!any(is.na(normal))) {
        for (j in 1:3) {
          if (sum(normals[1:3, it[j, i]] * normal) < 0) {
            normals[, it[j, i]] <- normals[, it[j, i]] + c(-normal, 1)
          } else {
            normals[, it[j, i]] <- normals[, it[j, i]] + c(normal, 1)
          }
        }
      }
    }

    x$triangle_normals <- face_normals
  }

  if (length(x$ib)) {
    it <- x$ib
    face_normals <- matrix(0, ncol = ncol(it), nrow = 4)

    for (i in 1:ncol(it)) {
      normal <- vec3_normalize(vec3_crossproduct(
        v[, it[1, i]] - v[, it[4, i]],
        v[, it[2, i]] - v[, it[1, i]]
      ))
      face_normals[, i] <- c(normal, 1)
      if (!any(is.na(normal))) {
        for (j in 1:4) {
          if (sum(normals[1:3, it[j, i]] * normal) < 0) {
            normals[, it[j, i]] <- normals[, it[j, i]] + c(-normal, 1)
          } else {
            normals[, it[j, i]] <- normals[, it[j, i]] + c(normal, 1)
          }
        }
      }
    }

    x$quad_normals <- face_normals
  }

  # homogenise
  normals <- t(t(normals) / normals[4, ])

  # Now make into unit normals
  lengths <- sqrt(colSums(normals[1:3, ]^2))
  normals[1:3, ] <- t(t(normals[1:3, ]) / lengths)

  x$normals <- normals
  x$face_normals <- face_normals

  x
}

# element type
all_data_names <- c("ip", "il", "it", "ib")
all_element_names <- c("point", "line", "triangle", "quad")
# Determine element types present in object.
get_element_types <- function(x) {
  UseMethod("get_element_types")
}
get_element_types.data.frame <- function(x) {
  stopifnot("element_id" %in% colnames(x))

  vertices_per_element <- (aggregate(x$element_id, list(element_id = x$element_id), length))$x
  vertices_per_element <- unique(vertices_per_element)

  stopifnot(all(vertices_per_element %in% 1:4))

  vertices_per_element
}
get_element_types.mesh3d <- function(x) {
  # check which matrices are in object
  vertices_per_element <- which(c("ip", "il", "it", "ib") %in% names(x))

  stopifnot(all(vertices_per_element %in% 1:4))

  vertices_per_element
}

# Get a matrix of data to represent the given element type
get_matrix_for_element_type <- function(obj, element_type, all_vertices = NULL) {
  data_name <- all_data_names[element_type]
  vertices_per_element <- element_type

  if (is.null(obj[[data_name]])) {
    return(NULL)
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The vertex indicies for tihs element
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertex_indices <- as.vector(obj[[data_name]])
  n_elements <- ncol(obj[[data_name]])
  n_vertices <- length(vertex_indices)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All vertices
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(all_vertices)) {
    all_vertices <- t(obj$vb)
    all_vertices <- all_vertices[, 1:3] / all_vertices[, 4]
    all_vertices <- cbind(all_vertices, seq(nrow(all_vertices)))
    colnames(all_vertices) <- c("x", "y", "z", "vertex")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the actual vertex coords based upon the vertex_indicies for the elements
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertices <- all_vertices[vertex_indices, ]
  vertices <- cbind(vertices, vorder = rep(seq(vertices_per_element), n_elements))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set to NULL by default
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertex_normals <- NULL
  face_normals <- NULL


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # element_id ordering
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_id <- rep(seq(n_elements), each = vertices_per_element)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Normals
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vertices_per_element > 2) {
    obj <- add_normals(obj)

    face_normals_name <- ifelse(vertices_per_element == 3L, "triangle_normals", "quad_normals")

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # vertex normals. ensure they are unit vectors
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(obj$normals)) {
      vertex_normals <- t(obj$normals)[vertex_indices, 1:3]
      vertex_normals <- t(apply(vertex_normals, 1, vec3_normalize))
      colnames(vertex_normals) <- c("vnx", "vny", "vnz")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # face normals
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(obj[[face_normals_name]])) {
      face_normals <- t(obj[[face_normals_name]])[, 1:3]
      colnames(face_normals) <- c("fnx", "fny", "fnz")
      face_normals <- face_normals[rep(seq(n_elements), each = vertices_per_element), ]
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_mat <- cbind(vertices, vertex_normals, face_normals, element_id)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the centroid for each element_id
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  face_centroid <- aggregate(element_mat[, c("x", "y", "z")], by = list(element_id = element_mat[, "element_id"]), mean)
  face_centroid <- setNames(face_centroid, c("element_id", "fcx", "fcy", "fcz"))
  element_mat <- merge(element_mat, face_centroid, all.x = TRUE)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the zorder for each element_id based upon the z coordinates
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vertices_per_element == 1) {
    # for points, pick the z coordinate
    element_mat[, "zorder_var"] <- element_mat[, "z"]
  } else if (vertices_per_element == 2) {
    # For lines, pick the most negative z coord of each 'element_id'
    min_z <- aggregate(element_mat[, "z"], list(element_id = element_mat[, "element_id"]), max)$x
    element_mat[, "zorder_var"] <- rep(min_z, each = 2)
  } else {
    # for tris and quads, use the face centroid
    element_mat[, "zorder_var"] <- element_mat[, "fcz"]
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add dummy normal values if actual normals not found or not possible
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!"vnx" %in% colnames(element_mat)) {
    element_mat[, c("vnx", "vny", "vnz")] <- NA_real_
  }
  if (!"fnx" %in% colnames(element_mat)) {
    element_mat[, c("fnx", "fny", "fnz")] <- NA_real_
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Element type
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  element_mat[, "element_type"] <- vertices_per_element


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Desireded column ordering
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  column_ordering <- c(
    "element_type", "element_id", "vorder", "x", "y", "z", "vertex",
    "vnx", "vny", "vnz",
    "fnx", "fny", "fnz",
    "fcx", "fcy", "fcz", "zorder", "zorder_var"
  )
  column_ordering <- intersect(column_ordering, colnames(element_mat))
  element_mat <- element_mat[, c(column_ordering, setdiff(colnames(element_mat), column_ordering))]


  element_mat
}

# Add zordering
add_zorder <- function(obj_df) {
  missing_cols <- setdiff(c("zorder_var", "element_id", "vorder"), colnames(obj_df))
  if (length(missing_cols) > 0) {
    stop("data.frame is missing columns: ", deparse(missing_cols))
  }


  stopifnot(!"object_id" %in% colnames(obj_df))

  tmp <- obj_df
  tmp <- with(tmp, tmp[order(zorder_var, element_id, vorder), ])
  tmp$zorder <- with(tmp, factor(element_id, labels = seq_along(unique(element_id)), levels = unique(element_id)))
  tmp <- with(tmp, tmp[order(element_id, vorder), ])

  tmp
}
