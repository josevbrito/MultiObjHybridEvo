# ============================================================================
# ATIVIDADE IV: OTIMIZAÇÃO MULTIOBJETIVO - BAT-PSO HÍBRIDO vs NSGA-II
# ============================================================================

# Carrega biblioteca dbscan
if (!require(dbscan)) {
  install.packages("dbscan")
  library(dbscan)
} else {
    library(dbscan)
}

# --- Arquivos para Otimização Multiobjetivo (Atividade IV) ---
source('Benchmarks_MultiObjective.R') # Funções ZDT e auxiliares de Pareto
source('MO_BAT_PSO_Hybrid.R')         # Algoritmo Híbrido Multi-Objetivo
source('NSGAII.R')                    # Algoritmo NSGA-II

# --- Framework de Comparação e Análise ---
source('comparison.R')


# Função main para executar o experimento da Atividade IV
main <- function() {
  cat("============================================================================\n")
  cat("ATIVIDADE IV: OTIMIZAÇÃO MULTIOBJETIVO - BAT-PSO HÍBRIDO vs NSGA-II\n")
  cat("============================================================================\n")
  
  cat("Iniciando experimento de otimização multi-objetivo...\n")
  experiment_results <- run_comparison() # Chama a função de comparação
  
  perform_statistical_analysis(experiment_results) # Realiza a análise estatística
  
  cat("\nGerando visualizações...\n")
  create_visualization(experiment_results) # Gera os gráficos
  
  cat("\n============================================================================\n")
  cat("EXPERIMENTO DA ATIVIDADE IV CONCLUÍDO\n")
  cat("============================================================================\n")
  cat("Resumo dos Resultados da Atividade IV:\n")
  cat("- Testada 1 função de benchmark multi-objetivo (ZDT1)\n")
  cat("- Comparados 2 algoritmos multi-objetivo:\n")
  cat("  1. MO-BAT-PSO Híbrido\n")
  cat("  2. NSGA-II\n")
  cat("- Métrica de desempenho utilizada: Hypervolume (minimizado)\n")
  cat("- 30 execuções independentes por combinação algoritmo-função\n")
  cat("- Significância estatística testada com ANOVA e Tukey HSD\n")
  cat("- Gráficos de convergência (-Hypervolume), box plots (-Hypervolume) e Fronteiras de Pareto gerados\n")
  cat("- Ranking geral consolidado calculado\n")
  cat("============================================================================\n")
  
  return(experiment_results)
}

# Execução do experimento da Atividade IV
experiment_results <- main()