MO_BAT_PSO_Hybrid <- function(
    multi_obj_func, lb, ub, num_objectives, pop.size, dimension, max.it = 250,
    archive_size = 100, 
    A = 0.5, r = 0.5, Qmin = 0, Qmax = 2, alpha = 0.9, gamma = 0.9,
    w = 0.9, c1 = 2.0, c2 = 2.0, w_damp = 0.99,
    prob_bat = 0.5,
    hv_reference_point = c(1.1, 1.1) 
) {
  pop.size <- as.numeric(pop.size) 
  dimension <- as.numeric(dimension) 
  
  if (is.na(pop.size) || pop.size <= 0) stop("Erro: pop.size deve ser um número positivo.")
  if (is.na(dimension) || dimension <= 0) stop("Erro: dimension deve ser um número positivo.")

  bats <- matrix(runif(pop.size * dimension), nrow = pop.size)
  bats <- lb + bats * (ub - lb)
  velocity <- matrix(0, nrow = pop.size, ncol = dimension)
  
  frequency <- rep(0, pop.size)
  loudness <- rep(A, pop.size)
  pulse_rate <- rep(r, pop.size)
  
  objectives <- t(apply(bats, 1, multi_obj_func))
  pbest_positions <- bats
  pbest_objectives <- objectives
  
  archive_positions <- matrix(NA, nrow = 0, ncol = dimension)
  archive_objectives <- matrix(NA, nrow = 0, ncol = num_objectives)
  
    initial_front_indices <- nondominated_sort(objectives)[[1]]
    if (length(initial_front_indices) > 0) {
        archive_positions <- bats[initial_front_indices, , drop=FALSE]
        archive_objectives <- objectives[initial_front_indices, , drop=FALSE]
    }

  history_f1_mean_archive <- rep(NA, max.it)
  history_hypervolume <- rep(NA, max.it)

  current_w <- w 
  
  for(it in 1:max.it) {
    for(i in 1:pop.size) {
      gbest_guide_position <- if (nrow(archive_positions) > 0) {
        archive_positions[sample(1:nrow(archive_positions), 1), ]
      } else {
        pbest_positions[i, ] 
      }
      
      if (runif(1) < prob_bat) {
        frequency[i] <- Qmin + (Qmax - Qmin) * runif(1)
        velocity[i, ] <- velocity[i, ] + (bats[i, ] - gbest_guide_position) * frequency[i] 
        max_vel <- abs(ub - lb) * 0.5 
        velocity[i, ] <- pmax(pmin(velocity[i, ], max_vel), -max_vel)
        new_position <- bats[i, ] + velocity[i, ]

        if(runif(1) > pulse_rate[i]) {
          new_position <- gbest_guide_position + loudness[i] * runif(dimension, -1, 1)
        }
      } else {
        r1 <- runif(dimension)
        r2 <- runif(dimension)
        velocity[i, ] <- current_w * velocity[i, ] +
                         c1 * r1 * (pbest_positions[i, ] - bats[i, ]) +
                         c2 * r2 * (gbest_guide_position - bats[i, ]) 
        max_vel <- abs(ub - lb) * 0.5
        velocity[i, ] <- pmax(pmin(velocity[i, ], max_vel), -max_vel)
        new_position <- bats[i, ] + velocity[i, ]
      }
      
      new_position <- pmax(pmin(new_position, ub), lb) 
      new_objectives <- multi_obj_func(new_position)

      if (is_dominated(new_objectives, pbest_objectives[i, ])) {
        pbest_positions[i, ] <- new_position
        pbest_objectives[i, ] <- new_objectives
      } else if (!is_dominated(pbest_objectives[i, ], new_objectives) && new_objectives[1] < pbest_objectives[i, 1]) { 
        pbest_positions[i, ] <- new_position
        pbest_objectives[i, ] <- new_objectives
      }
      
      bats[i, ] <- new_position
      objectives[i, ] <- new_objectives

      loudness[i] <- alpha * loudness[i]
      pulse_rate[i] <- r * (1 - exp(-gamma * it))
    }
    
    current_w <- current_w * w_damp
    
    combined_objectives <- rbind(objectives, archive_objectives)
    combined_positions <- rbind(bats, archive_positions) 

    current_nondominated_objectives <- get_nondominated_front(combined_objectives)
    
    temp_archive_positions <- matrix(NA, nrow = 0, ncol = dimension)
    temp_archive_objectives <- matrix(NA, nrow=0, ncol=num_objectives)
    
    for (sol_idx in 1:nrow(combined_objectives)) {
      if (any(apply(current_nondominated_objectives, 1, function(row) all(row == combined_objectives[sol_idx, ])))) {
        temp_archive_positions <- rbind(temp_archive_positions, combined_positions[sol_idx, ])
        temp_archive_objectives <- rbind(temp_archive_objectives, combined_objectives[sol_idx, ])
      }
    }

    if (nrow(temp_archive_objectives) > archive_size) {
      cd_values <- crowding_distance(temp_archive_objectives)
      sorted_indices_by_cd <- order(cd_values, decreasing = TRUE)
      
      archive_objectives <- temp_archive_objectives[sorted_indices_by_cd[1:archive_size], , drop=FALSE]
      archive_positions <- temp_archive_positions[sorted_indices_by_cd[1:archive_size], , drop=FALSE]
    } else {
      archive_objectives <- temp_archive_objectives
      archive_positions <- temp_archive_positions
    }
    
    if (nrow(archive_objectives) > 0) {
      history_f1_mean_archive[it] <- mean(archive_objectives[, 1])
      history_hypervolume[it] <- calculate_hypervolume(archive_objectives, hv_reference_point)
    } else {
      history_f1_mean_archive[it] <- NA
      history_hypervolume[it] <- 0 
    }
  }
  
  return(list(pop = bats, objectives = objectives, 
              history_f1_mean_archive = history_f1_mean_archive,
              history_hypervolume = history_hypervolume,
              final_archive_positions = archive_positions, final_archive_objectives = archive_objectives))
}