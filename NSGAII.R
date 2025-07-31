NSGAII <- function(
    multi_obj_func, lb, ub, num_objectives, pop.size, dimension, max.it = 250,
    pc = 0.9, pm = 1/dimension, 
    t.size = 2, 
    hv_reference_point = c(1.1, 1.1) 
) {
  pop.size <- as.numeric(pop.size)
  dimension <- as.numeric(dimension)

  if (is.na(pop.size) || pop.size <= 0) stop("Erro: pop.size deve ser um número positivo.")
  if (is.na(dimension) || dimension <= 0) stop("Erro: dimension deve ser um número positivo.")

  pop_positions <- matrix(runif(pop.size * dimension), nrow = pop.size)
  pop_positions <- lb + pop_positions * (ub - lb)
  pop_objectives <- t(apply(pop_positions, 1, multi_obj_func))
  
  history_f1_mean_front1 <- rep(NA, max.it)
  history_hypervolume <- rep(NA, max.it) 

  for (it in 1:max.it) {
    all_fronts_current_pop <- nondominated_sort(pop_objectives)
    
    rankings <- rep(NA, pop.size)
    crowding_distances <- rep(NA, pop.size)
    
    for (f_rank in 1:length(all_fronts_current_pop)) {
      front_indices <- all_fronts_current_pop[[f_rank]]
      if (length(front_indices) == 0) next
      
      rankings[front_indices] <- f_rank 
      
      if (length(front_indices) == 1) { 
        crowding_distances[front_indices] <- Inf 
      } else {
        crowding_distances[front_indices] <- crowding_distance(pop_objectives[front_indices, , drop=FALSE])
      }
    }
    
    tournament_selection_nsga2 <- function(pool_indices) {
        selected_idx <- sample(pool_indices, t.size, replace = FALSE) 
        
        best_idx_in_tournament <- selected_idx[1]
        for (j in 2:t.size) {
            candidate_idx <- selected_idx[j]
            if (rankings[candidate_idx] < rankings[best_idx_in_tournament]) {
                best_idx_in_tournament <- candidate_idx
            } else if (rankings[candidate_idx] == rankings[best_idx_in_tournament]) {
                if (crowding_distances[candidate_idx] > crowding_distances[best_idx_in_tournament]) {
                    best_idx_in_tournament <- candidate_idx
                }
            }
        }
        return(best_idx_in_tournament)
    }

    offspring_positions <- matrix(NA, nrow = pop.size, ncol = dimension)
    
    for (k in 1:(pop.size / 2)) { 
      parent1_idx <- tournament_selection_nsga2(1:pop.size)
      parent2_idx <- tournament_selection_nsga2(1:pop.size)
      
      parent1_pos <- pop_positions[parent1_idx, ]
      parent2_pos <- pop_positions[parent2_idx, ]
      
      if (runif(1) < pc) {
        l_crossover <- runif(1) 
        offspring1_pos <- l_crossover * parent1_pos + (1 - l_crossover) * parent2_pos
        offspring2_pos <- (1 - l_crossover) * parent1_pos + l_crossover * parent2_pos
      } else {
        offspring1_pos <- parent1_pos
        offspring2_pos <- parent2_pos
      }
      
      mutation_prob_for_dim <- runif(dimension)
      offspring1_pos[mutation_prob_for_dim < pm] <- lb[mutation_prob_for_dim < pm] + runif(sum(mutation_prob_for_dim < pm)) * (ub[mutation_prob_for_dim < pm] - lb[mutation_prob_for_dim < pm])
      offspring2_pos[mutation_prob_for_dim < pm] <- lb[mutation_prob_for_dim < pm] + runif(sum(mutation_prob_for_dim < pm)) * (ub[mutation_prob_for_dim < pm] - lb[mutation_prob_for_dim < pm])
      
      offspring1_pos <- pmax(pmin(offspring1_pos, ub), lb)
      offspring2_pos <- pmax(pmin(offspring2_pos, ub), lb)

      offspring_positions[2*k-1, ] <- offspring1_pos
      if (2*k <= pop.size) { 
        offspring_positions[2*k, ] <- offspring2_pos
      }
    }
    offspring_objectives <- t(apply(offspring_positions, 1, multi_obj_func))

    combined_positions <- rbind(pop_positions, offspring_positions)
    combined_objectives <- rbind(pop_objectives, offspring_objectives)
    
    all_combined_fronts <- nondominated_sort(combined_objectives)
    
    new_pop_positions <- matrix(NA, nrow = 0, ncol = dimension)
    new_pop_objectives <- matrix(NA, nrow = 0, ncol = num_objectives)
    
    current_pop_size <- 0
    for (f_rank in 1:length(all_combined_fronts)) {
      front_indices <- all_combined_fronts[[f_rank]]
      if (length(front_indices) == 0) next
      
      if (current_pop_size + length(front_indices) <= pop.size) {
        new_pop_positions <- rbind(new_pop_positions, combined_positions[front_indices, , drop=FALSE])
        new_pop_objectives <- rbind(new_pop_objectives, combined_objectives[front_indices, , drop=FALSE])
        current_pop_size <- current_pop_size + length(front_indices)
      } else {
        remaining_capacity <- pop.size - current_pop_size
        current_front_objectives <- combined_objectives[front_indices, , drop=FALSE]
        cd_values <- crowding_distance(current_front_objectives)
        sorted_by_cd_indices_in_front <- order(cd_values, decreasing = TRUE)[1:remaining_capacity]
        
        new_pop_positions <- rbind(new_pop_positions, combined_positions[front_indices[sorted_by_cd_indices_in_front], , drop=FALSE])
        new_pop_objectives <- rbind(new_pop_objectives, combined_objectives[front_indices[sorted_by_cd_indices_in_front], , drop=FALSE])
        current_pop_size <- pop.size 
        break 
      }
    }
    
    pop_positions <- new_pop_positions
    pop_objectives <- new_pop_objectives

    current_first_front_indices <- all_combined_fronts[[1]] 
    current_first_front_objectives <- combined_objectives[current_first_front_indices, , drop=FALSE]
    
    if (nrow(current_first_front_objectives) > 0) {
      history_f1_mean_front1[it] <- mean(current_first_front_objectives[, 1]) 
      history_hypervolume[it] <- calculate_hypervolume(current_first_front_objectives, hv_reference_point)
    } else {
      history_f1_mean_front1[it] <- NA
      history_hypervolume[it] <- 0 
    }
  }
  
  final_nondominated_front_indices <- nondominated_sort(pop_objectives)[[1]]
  final_nondominated_positions <- pop_positions[final_nondominated_front_indices, , drop=FALSE]
  final_nondominated_objectives <- pop_objectives[final_nondominated_front_indices, , drop=FALSE]
  
  return(list(
    final_positions = final_nondominated_positions,
    final_objectives = final_nondominated_objectives,
    history_f1_mean_front1 = history_f1_mean_front1,
    history_hypervolume = history_hypervolume
  ))
}