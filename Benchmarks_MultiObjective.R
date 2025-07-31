ZDT1 <- function(x) {
  n <- length(x)
  f1 <- x[1]
  g <- 1 + (9 / (n - 1)) * sum(x[2:n])
  ratio_f1_g <- f1 / g
  if (ratio_f1_g < 0) ratio_f1_g <- 0 
  h <- 1 - sqrt(ratio_f1_g)
  f2 <- g * h
  return(c(f1, f2))
}

ZDT2 <- function(x) {
  n <- length(x)
  f1 <- x[1]
  g <- 1 + (9 / (n - 1)) * sum(x[2:n])
  h <- 1 - (f1 / g)^2
  f2 <- g * h
  return(c(f1, f2))
}

ZDT3 <- function(x) {
  n <- length(x)
  f1 <- x[1]
  g <- 1 + (9 / (n - 1)) * sum(x[2:n])
  ratio_f1_g <- f1 / g
  if (ratio_f1_g < 0) ratio_f1_g <- 0 
  h <- 1 - sqrt(ratio_f1_g) - (ratio_f1_g) * sin(10 * pi * f1)
  f2 <- g * h
  return(c(f1, f2))
}

ZDT4 <- function(x) {
  n <- length(x) 
  f1 <- x[1]
  g <- 1 + 10 * (n - 1) + sum(x[2:n]^2 - 10 * cos(4 * pi * x[2:n]))
  ratio_f1_g <- f1 / g
  if (ratio_f1_g < 0) ratio_f1_g <- 0 
  h <- 1 - sqrt(ratio_f1_g)
  f2 <- g * h
  return(c(f1, f2))
}

ZDT6 <- function(x) {
  n <- length(x) 
  f1 <- 1 - exp(-4 * x[1]) * (sin(6 * pi * x[1]))^6
  g <- 1 + 9 * ((sum(x[2:n])) / 9)^0.25
  h <- 1 - (f1 / g)^2
  f2 <- g * h
  return(c(f1, f2))
}

is_dominated <- function(obj_a, obj_b) {
  if (any(!is.finite(obj_a)) || any(!is.finite(obj_b))) {
    if (any(is.na(obj_a)) || any(is.na(obj_b))) return(FALSE)
    if (any(obj_a == Inf & is.finite(obj_b))) return(FALSE)
    if (any(obj_b == Inf & is.finite(obj_a))) {
      all_le_finite <- all(obj_a[is.finite(obj_a)] <= obj_b[is.finite(obj_a)])
      any_lt_finite <- any(obj_a[is.finite(obj_a)] < obj_b[is.finite(obj_a)])
      if (all_le_finite && any_lt_finite) return(TRUE)
      else if (any(obj_a == -Inf & is.finite(obj_b))) return(TRUE)
      else return(FALSE)
    }
  }
  all_le <- all(obj_a <= obj_b)
  any_lt <- any(obj_a < obj_b)
  return(all_le && any_lt)
}

get_nondominated_front <- function(objectives_matrix) {
  num_solutions <- nrow(objectives_matrix)
  if (num_solutions == 0) return(matrix(nrow = 0, ncol = ncol(objectives_matrix)))
  if (num_solutions == 1) return(matrix(objectives_matrix, nrow = 1))
  is_nondominated <- rep(TRUE, num_solutions)
  for (i in 1:num_solutions) {
    if (!is_nondominated[i]) next
    for (j in 1:num_solutions) {
      if (i == j) next
      if (is_dominated(objectives_matrix[j, ], objectives_matrix[i, ])) {
        is_nondominated[i] <- FALSE
        break
      }
    }
  }
  return(objectives_matrix[is_nondominated, , drop = FALSE])
}

nondominated_sort <- function(objectives_matrix) {
  if (is.null(dim(objectives_matrix)) && length(objectives_matrix) > 0) objectives_matrix <- matrix(objectives_matrix, nrow = 1)
  if (nrow(objectives_matrix) == 0) return(list()) 
  num_solutions <- nrow(objectives_matrix)
  population_indices <- 1:num_solutions
  fronts <- list()
  n_p <- rep(0, num_solutions) 
  s_p <- vector("list", num_solutions) 
  fronts[[1]] <- c() 
  for (p_idx in population_indices) {
    s_p[[p_idx]] <- c()
    n_p[p_idx] <- 0
    for (q_idx in population_indices) {
      if (p_idx == q_idx) next
      if (is_dominated(objectives_matrix[p_idx, ], objectives_matrix[q_idx, ])) {
        s_p[[p_idx]] <- c(s_p[[p_idx]], q_idx) 
      } else if (is_dominated(objectives_matrix[q_idx, ], objectives_matrix[p_idx, ])) {
        n_p[p_idx] <- n_p[p_idx] + 1 
      }
    }
    if (n_p[p_idx] == 0) {
      fronts[[1]] <- c(fronts[[1]], p_idx)
    }
  }
  k <- 1
  while (length(fronts[[k]]) > 0) {
    Q <- c() 
    for (p_idx in fronts[[k]]) { 
      for (q_idx in s_p[[p_idx]]) { 
        n_p[q_idx] <- n_p[q_idx] - 1 
        if (n_p[q_idx] == 0) {
          Q <- c(Q, q_idx)
        }
      }
    }
    k <- k + 1
    fronts[[k]] <- Q
  }
  if (length(fronts) > 0 && length(fronts[[length(fronts)]]) == 0) {
    fronts <- fronts[-length(fronts)]
  }
  if (length(fronts) == 0 && num_solutions > 0) {
    warning("nondominated_sort: Nenhuma fronteira de Pareto encontrada. Retornando o primeiro indivíduo.")
    return(list(1))
  }
  return(fronts)
}

crowding_distance <- function(obj_matrix) {
  num_points <- nrow(obj_matrix)
  num_objectives <- ncol(obj_matrix)
  distances <- rep(0, num_points)
  if (num_points <= 2) {
    return(rep(Inf, num_points)) 
  }
  for (m in 1:num_objectives) {
    sorted_indices <- order(obj_matrix[, m])
    sorted_obj_values <- obj_matrix[sorted_indices, m]
    distances[sorted_indices[1]] <- Inf
    distances[sorted_indices[num_points]] <- Inf
    obj_range <- sorted_obj_values[num_points] - sorted_obj_values[1]
    if (obj_range == 0) {
      distances[sorted_indices[2:(num_points - 1)]] <- Inf
    } else {
      for (i in 2:(num_points - 1)) {
        distances[sorted_indices[i]] <- distances[sorted_indices[i]] + 
                                         (sorted_obj_values[i+1] - sorted_obj_values[i-1]) / obj_range
      }
    }
  }
  return(distances)
}

calculate_hypervolume <- function(solutions_objectives, reference_point) {
  if (is.null(dim(solutions_objectives))) { 
    if (length(solutions_objectives) == 0) return(0) 
    solutions_objectives <- matrix(solutions_objectives, nrow = 1)
  }
  num_solutions <- nrow(solutions_objectives)
  num_objectives <- ncol(solutions_objectives)
  if (num_solutions == 0) return(0)
  if (num_objectives != length(reference_point)) {
    stop("Número de objetivos nas soluções e no ponto de referência deve ser o mesmo.")
  }
  if (num_objectives == 2) {
    valid_solutions_indices <- which(
        apply(solutions_objectives, 1, function(obj) all(obj <= reference_point))
    )
    if (length(valid_solutions_indices) == 0) return(0)
    filtered_objectives <- solutions_objectives[valid_solutions_indices, , drop = FALSE]
    sorted_obj <- filtered_objectives[order(filtered_objectives[, 1]), , drop = FALSE]
    hv_val <- 0
    current_f1_coord <- 0 
    current_f2_ceiling <- reference_point[2] 
    for (i in 1:nrow(sorted_obj)) {
      f1_current <- sorted_obj[i, 1]
      f2_current <- sorted_obj[i, 2]
      width <- f1_current - current_f1_coord
      height <- current_f2_ceiling - f2_current
      if (width > 0 && height > 0) {
        hv_val <- hv_val + (width * height)
      }
      current_f1_coord <- f1_current
      current_f2_ceiling <- f2_current
    }
    return(hv_val)
  } else {
    warning("Cálculo de Hypervolume para mais de 2 objetivos não implementado. Retornando 0.")
    return(0)
  }
}
