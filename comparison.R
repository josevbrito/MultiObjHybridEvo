# comparison.R

# ============================================================================
# FRAMEWORK DE COMPARAÇÃO EXPERIMENTAL (Foco: OTIMIZAÇÃO MULTIOBJETIVO - Atividade 4)
# ============================================================================

run_comparison <- function() {
  set.seed(123) # Para reprodutibilidade

  # Parâmetros experimentais comuns
  execs <- 30
  pop.size <- 50
  max.it <- 250 
  dimension <- 30 
  num_objectives <- 2 

  hv_ref_point_zdt <- c(1.1, 1.1) 

  functions <- list(
    ZDT1 = list(func = ZDT1, lb = rep(0, dimension), ub = rep(1, dimension), 
                name = "ZDT1 (Multiobjetivo)", dimension = dimension, 
                num_objectives = num_objectives, max.it = max.it, hv_ref_point = hv_ref_point_zdt)
  )
  
  algorithms <- c("MO_BAT_PSO_Hybrid", "NSGAII") 
  
  results <- array(NA, dim = c(length(functions), length(algorithms), execs))
  convergence_data <- list() 
  final_pareto_fronts <- list()

  cat("Iniciando experimento de comparação de Algoritmos Multiobjetivo (Atividade 4 - Foco: ZDT1)...\n")
  cat("ATENÇÃO: Resultados de MO_BAT_PSO_Hybrid e NSGAII serão SIMULADOS.\n")
  
  for(f_idx in 1:length(functions)) {
    func_info <- functions[[f_idx]]
    current_func <- func_info$func
    current_lb <- func_info$lb
    current_ub <- func_info$ub
    current_dimension <- func_info$dimension
    current_max_it <- func_info$max.it
    current_num_obj <- func_info$num_objectives 
    current_hv_ref_point <- func_info$hv_ref_point 

    cat("Testando função:", func_info$name, "\n")
    
    for(alg_idx in 1:length(algorithms)) {
      algorithm <- algorithms[alg_idx]
      cat("  Algoritmo:", algorithm, "\n")
      
      for(exec in 1:execs) {
        if(exec %% 10 == 0) cat("    Execução:", exec, "\n")
        
        result <- NULL
        current_final_metric_value <- NA 
        
        if (algorithm == "MO_BAT_PSO_Hybrid") {
            simulated_final_hvs <- runif(1, min = -0.75, max = -0.70)
            simulated_convergence_history <- seq(from = -0.4, to = -0.72, length.out = func_info$max.it) + runif(func_info$max.it, -0.01, 0.01)
            sim_f1 <- seq(0, 1, length.out = 50)
            sim_f2 <- 1 - sim_f1
            simulated_final_pf <- cbind(sim_f1, sim_f2 + runif(50, -0.03, 0.03)) 
            
            current_final_metric_value <- simulated_final_hvs
            if(exec == 1) { 
                convergence_data[[paste(func_info$name, algorithm, sep = "_")]] <- simulated_convergence_history
                final_pareto_fronts[[paste(func_info$name, algorithm, sep = "_")]] <- simulated_final_pf
            }
        } else if (algorithm == "NSGAII") {
            simulated_final_hvs <- runif(1, min = -0.79, max = -0.75) 
            simulated_convergence_history <- seq(from = -0.5, to = -0.78, length.out = func_info$max.it) + runif(func_info$max.it, -0.01, 0.01)
            sim_f1 <- seq(0, 1, length.out = 50)
            sim_f2 <- 1 - sim_f1
            simulated_final_pf <- cbind(sim_f1, sim_f2 + runif(50, -0.02, 0.02))
            
            current_final_metric_value <- simulated_final_hvs
            if(exec == 1) { 
                convergence_data[[paste(func_info$name, algorithm, sep = "_")]] <- simulated_convergence_history
                final_pareto_fronts[[paste(func_info$name, algorithm, sep = "_")]] <- simulated_final_pf
            }
        } 
        results[f_idx, alg_idx, exec] <- current_final_metric_value
      }
    }
  }
  
  return(list(results = results, convergence_data = convergence_data, 
              functions = functions, algorithms = algorithms,
              final_pareto_fronts = final_pareto_fronts,
              execs = execs,
              pop.size = pop.size,
              max.it = max.it,
              dimension = dimension
              )) 
}

perform_statistical_analysis <- function(experiment_results) {
  results <- experiment_results$results
  functions <- experiment_results$functions
  algorithms <- experiment_results$algorithms
  execs <- experiment_results$execs
  
  cat("\n============================================================================\n")
  cat("ANÁLISE ESTATÍSTICA - ALGORITMOS MULTIOBJETIVO\n") 
  cat("============================================================================\n")
  
  for(f_idx in 1:length(functions)) {
    func_info <- functions[[f_idx]]
    func_name <- func_info$name
    
    cat("\nFunção:", func_name, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    
    func_results <- results[f_idx, , , drop = TRUE] 

    cat("\nEstatísticas de Resumo (Métrica: -Hypervolume):\n")
    cat(sprintf("%-20s %12s %12s %12s %12s %12s\n", 
                "Algoritmo", "Média", "Desvio Padrão", "Mínimo", "Máximo", "Mediana"))
    cat(paste(rep("-", 90), collapse = ""), "\n")
    
    for(alg_idx in 1:length(algorithms)) {
      alg_results <- func_results[alg_idx, ] 
      cat(sprintf("%-20s %12.6f %12.6f %12.6f %12.6f %12.6f\n", 
                  algorithms[alg_idx], 
                  mean(alg_results, na.rm = TRUE), 
                  sd(alg_results, na.rm = TRUE), 
                  min(alg_results, na.rm = TRUE), 
                  max(alg_results, na.rm = TRUE), 
                  median(alg_results, na.rm = TRUE)))
    }
    
    cat("\nRanking Médio (1 = melhor, baseada em -Hypervolume):\n")

    rankings <- matrix(NA, nrow = length(algorithms), ncol = execs)
    for(exec_col in 1:execs) {
      current_exec_results <- func_results[, exec_col] 
      current_exec_results[is.na(current_exec_results)] <- Inf 
      rankings[, exec_col] <- rank(current_exec_results, ties.method = "average")
    }
    avg_rankings <- rowMeans(rankings) 
    ranking_order <- order(avg_rankings)
    
    cat(sprintf("%-20s %12s\n", "Algoritmo", "Ranking Médio"))
    cat(paste(rep("-", 40), collapse = ""), "\n")

    for(i in ranking_order) {
      cat(sprintf("%-20s %12.2f\n", algorithms[i], avg_rankings[i]))
    }
    
    cat("\nTeste ANOVA:\n")
    data_for_anova <- data.frame(
      Hypervolume = as.vector(func_results), 
      Algorithm = rep(algorithms, each = execs)
    )
    data_for_anova <- na.omit(data_for_anova) 
    
    if (nrow(data_for_anova) > 0 && length(unique(data_for_anova$Algorithm)) > 1) { 
        anova_result <- aov(Hypervolume ~ Algorithm, data = data_for_anova) 
        anova_summary <- summary(anova_result)
        print(anova_summary)
        
        p_value <- anova_summary[[1]][["Pr(>F)"]][1]
        if(!is.na(p_value) && p_value < 0.05) {
          cat("\nTeste HSD de Tukey (comparações pareadas):\n")
          tukey_result <- TukeyHSD(anova_result)
          print(tukey_result)
          
          cat("\nResumo das Diferenças Significativas (p < 0.05):\n")
          tukey_data <- tukey_result$Algorithm 
          significant <- tukey_data[tukey_data[, "p adj"] < 0.05, ]
          if(nrow(significant) > 0) {
            for(i in 1:nrow(significant)) {
              comparison <- rownames(significant)[i]
              p_val <- significant[i, "p adj"]
              diff <- significant[i, "diff"]
              cat(sprintf("  %-25s: diferença = %12.6f, p = %12.6f\n", comparison, diff, p_val))
            }
          } else {
            cat("  Nenhuma diferença significativa encontrada.\n")
          }
        } else {
          cat("\nANOVA não significativa (p >= 0.05), ignorando o teste de Tukey.\n")
        }
    } else {
        cat("\nNão há dados suficientes ou variação para realizar a ANOVA.\n")
    }
  }
  
  cat("\n============================================================================\n")
  cat("ANÁLISE GERAL DE DESEMPENHO\n")
  cat("============================================================================\n")
  
  overall_rankings_final_calc <- matrix(NA, nrow = length(algorithms), ncol = length(functions))
  rownames(overall_rankings_final_calc) <- algorithms
  colnames(overall_rankings_final_calc) <- sapply(functions, `[[`, "name")

  for(f_idx in 1:length(functions)) {
    current_func_results <- results[f_idx, , , drop = TRUE] 
    
    temp_rankings_matrix <- matrix(NA, nrow = length(algorithms), ncol = execs)
    
    for(exec_col in 1:execs) {
      exec_results_for_rank <- current_func_results[, exec_col]
      exec_results_for_rank[is.na(exec_results_for_rank)] <- Inf 
      temp_rankings_matrix[, exec_col] <- rank(exec_results_for_rank, ties.method = "average")
    }
    overall_rankings_final_calc[, f_idx] <- rowMeans(temp_rankings_matrix)
  }
  
  final_rankings <- rowMeans(overall_rankings_final_calc)
  final_order <- order(final_rankings)
  
  cat("\nRanking Geral Consolidado:\n")
  cat(sprintf("%-20s %12s\n", "Algoritmo", "Ranking Médio"))
  cat(paste(rep("-", 40), collapse = ""), "\n")
  for(i in final_order) {
    cat(sprintf("%-20s %12.2f\n", algorithms[i], final_rankings[i]))
  }
}


create_visualization <- function(experiment_results) {
  convergence_data <- experiment_results$convergence_data
  functions <- experiment_results$functions
  algorithms <- experiment_results$algorithms
  results <- experiment_results$results 
  final_pareto_fronts <- experiment_results$final_pareto_fronts 

  colors_mo_algs <- c("cyan", "magenta") 
  
  for(f_idx in 1:length(functions)) {
    func_info <- functions[[f_idx]]
    func_name <- func_info$name 
    

    func_results <- results[f_idx, , , drop = TRUE]
    
    # --- Gráfico de Convergência ---
    plot_convergence_filename <- paste0("convergencia_", gsub(" ", "_", func_name), ".png")
    
    tryCatch({
      png(filename = plot_convergence_filename, width = 800, height = 600)
      par(mar = c(4, 4, 3, 1)) 
      
      func_convergence <- list()
      alg_names_for_legend <- c() 
      
      for(i in 1:length(algorithms)) { 
        alg <- algorithms[i]
        key <- paste(func_name, alg, sep = "_") 
        if(key %in% names(convergence_data)) {
          if (!all(is.na(convergence_data[[key]]))) {
            func_convergence[[alg]] <- convergence_data[[key]]
            alg_names_for_legend <- c(alg_names_for_legend, alg)
          }
        }
      }
      
      if(length(func_convergence) > 0) {
        max_length <- max(sapply(func_convergence, length))
        
        ylab_text <- "-Hypervolume"
        plot_ylim <- range(unlist(func_convergence), na.rm = TRUE) 
        if (length(plot_ylim) == 0 || is.infinite(plot_ylim[1]) || is.infinite(plot_ylim[2])) plot_ylim <- c(-1.0, 0)
        else { plot_ylim[1] <- min(plot_ylim[1], -1.0); plot_ylim[2] <- max(plot_ylim[2], 0) }
        
        first_alg_data <- NULL
        first_alg_name <- NULL
        first_alg_color_idx <- NULL

        for (i in 1:length(algorithms)) {
            alg_current <- algorithms[i]
            if (alg_current %in% names(func_convergence) && !all(is.na(func_convergence[[alg_current]]))) {
                first_alg_data <- func_convergence[[alg_current]]
                first_alg_name <- alg_current
                first_alg_color_idx <- match(first_alg_name, algorithms) 
                break
            }
        }

        if (!is.null(first_alg_data)) {
            plot(1:max_length, first_alg_data, type = "l", col = colors_mo_algs[match(first_alg_name, algorithms)], 
                 xlab = "Geração", ylab = ylab_text, 
                 main = paste("Convergência -", func_name), 
                 ylim = plot_ylim, lwd = 2)
            
            for(i in 1:length(algorithms)) { 
                alg <- algorithms[i]
                if(alg %in% names(func_convergence) && alg != first_alg_name) { 
                    if (!all(is.na(func_convergence[[alg]]))) { 
                        lines(1:length(func_convergence[[alg]]), func_convergence[[alg]], 
                              col = colors_mo_algs[match(alg, algorithms)], lwd = 2) 
                    }
                }
            }
            
            legend("topright", legend = alg_names_for_legend, 
                   col = colors_mo_algs[match(alg_names_for_legend, algorithms)], lwd = 2, cex = 0.8)
        } else {
            plot(1, type="n", main=paste("Convergência -", func_name),
                 xlab="Geração", ylab=ylab_text, xlim=c(0, func_info$max.it), ylim=c(0,1))
            text(func_info$max.it/2, 0.5, "Dados insuficientes para este gráfico")
        }
      }
      dev.off() 
    }, error = function(e) {
      print(paste("ERRO ao salvar", plot_convergence_filename, ":", e$message))
      if (!is.null(dev.cur())) dev.off()
    })


    # --- Box Plot ---
    plot_boxplot_filename <- paste0("boxplot_", gsub(" ", "_", func_name), ".png")
    
    tryCatch({
      png(filename = plot_boxplot_filename, width = 800, height = 600)
      par(mar = c(8, 4, 3, 1)) 
      
      data_for_boxplot_list <- list()
      alg_names_for_boxplot <- c()

      for(i in 1:length(algorithms)) { 
          alg_results_current <- func_results[i, ]
          valid_results <- alg_results_current[!is.na(alg_results_current)]
          
          if (length(valid_results) > 0) {
              data_for_boxplot_list[[algorithms[i]]] <- valid_results
              alg_names_for_boxplot <- c(alg_names_for_boxplot, algorithms[i])
          }
      }

      if (length(data_for_boxplot_list) > 0) {
          ylab_text <- "-Hypervolume Final"
          boxplot_ylim_data <- unlist(data_for_boxplot_list)
          boxplot_ylim <- range(boxplot_ylim_data, na.rm = TRUE)
          if (length(boxplot_ylim) == 0 || is.infinite(boxplot_ylim[1]) || is.infinite(boxplot_ylim[2])) boxplot_ylim <- c(-0.8, -0.6)
          else { boxplot_ylim[1] <- min(boxplot_ylim[1], -0.8); boxplot_ylim[2] <- max(boxplot_ylim[2], -0.6) }

          boxplot(data_for_boxplot_list, 
                  names = alg_names_for_boxplot, 
                  main = paste("Distribuição dos Resultados -", func_name),
                  xlab = "Algoritmo", ylab = ylab_text,
                  las = 2, cex.axis = 0.8,
                  ylim = boxplot_ylim) 
      } else {
          plot(1, type="n", main=paste("Distribuição dos Resultados -", func_name),
               xlab="Algoritmo", ylab=ylab_text, xlim=c(0,1), ylim=c(0,1))
          text(0.5, 0.5, "Nenhum dado válido disponível para boxplot")
      }
      dev.off()
    }, error = function(e) {
      print(paste("ERRO ao salvar", plot_boxplot_filename, ":", e$message))
      if (!is.null(dev.cur())) dev.off()
    })

    # --- Plot da Fronteira de Pareto ---
    plot_pareto_filename <- paste0("pareto_front_", gsub(" ", "_", func_name), ".png")
    tryCatch({
        png(filename = plot_pareto_filename, width = 800, height = 600)
        par(mar = c(4, 4, 3, 1))

        mo_alg_colors <- c("cyan", "magenta") 
        mo_alg_names <- c("MO_BAT_PSO_Hybrid", "NSGAII")
        
        pf_mo_bat_pso <- final_pareto_fronts[[paste(func_name, "MO_BAT_PSO_Hybrid", sep = "_")]]
        pf_nsga2 <- final_pareto_fronts[[paste(func_name, "NSGAII", sep = "_")]]

        plot_xlim <- c(0, 1) 
        plot_ylim_f2 <- c(0, 1) 

        if (func_name == "ZDT3 (Multiobjetivo)") {
            plot_ylim_f2 <- c(-1, 1.1) 
        }

        plot(NULL, xlim = plot_xlim, ylim = plot_ylim_f2,
             xlab = "Objetivo 1 (f1)", ylab = "Objetivo 2 (f2)",
             main = paste("Fronteiras de Pareto -", func_name))

        if (!is.null(pf_mo_bat_pso) && nrow(pf_mo_bat_pso) > 0) {
            points(pf_mo_bat_pso[, 1], pf_mo_bat_pso[, 2], col = mo_alg_colors[1], pch = 16, cex = 0.8)
        }
        if (!is.null(pf_nsga2) && nrow(pf_nsga2) > 0) {
            points(pf_nsga2[, 1], pf_nsga2[, 2], col = mo_alg_colors[2], pch = 17, cex = 0.8)
        }
        
        legend("topright", 
               legend = mo_alg_names, 
               col = mo_alg_colors, 
               pch = c(16, 17), cex = 0.8)
        
        dev.off()
    }, error = function(e) {
            print(paste("ERRO ao salvar fronteira de Pareto para", func_name, ":", e$message))
            if (!is.null(dev.cur())) dev.off()
        })
    }
  }
