# Otimização Multiobjetivo: Comparação de Algoritmos Híbridos e NSGA-II

Este projeto implementa e compara o desempenho de algoritmos metaheurísticos para otimização multiobjetivo, focando na Atividade 4 da disciplina de Computação Evolucionária Aplicada. O trabalho centraliza-se na avaliação de um algoritmo híbrido (BAT-PSO Multiobjetivo) contra o NSGA-II em funções de benchmark multiobjetivo ZDT, utilizando a métrica de Hypervolume.

## 📋 Descrição do Projeto

O objetivo principal deste trabalho é explorar a otimização de problemas com múltiplos objetivos conflitantes, onde não existe uma única solução ótima, mas sim um conjunto de soluções de compromisso chamado Fronteira de Pareto.

Para isso, o projeto foca em:

### Atividade IV - Algoritmos Multiobjetivo
-   **MO-BAT-PSO Híbrido**: Uma adaptação multiobjetivo do algoritmo híbrido BAT-PSO. Ele combina as lógicas de busca do Bat Algorithm e do Particle Swarm Optimization, utilizando conceitos de dominância de Pareto e um arquivo externo (`archive`) para gerenciar e manter as soluções não-dominadas ao longo das iterações.
-   **NSGA-II (Nondominated Sorting Genetic Algorithm II)**: Um algoritmo genético multiobjetivo elitista e baseado em ordenação por não-dominância e distância de aglomeração (`crowding distance`) para manter a diversidade na Fronteira de Pareto.

## 🎯 Funções de Benchmark

O projeto utiliza funções de teste multiobjetivo para avaliar o desempenho dos algoritmos na descoberta e manutenção da Fronteira de Pareto:

### Funções Multi-Objetivo (ZDT)
As funções ZDT são um conjunto padrão de problemas de benchmark multiobjetivo com dois objetivos a serem minimizados ($f_1(\vec{x})$ e $f_2(\vec{x})$). Cada função ZDT possui características diferentes de sua Fronteira de Pareto ideal (convexa, não-convexa, descontínua, etc.).

-   **ZDT1**: Um problema de 30 variáveis com uma Fronteira de Pareto convexa e contínua. É ideal para testar a capacidade de convergência do algoritmo para a fronteira.

## 🔧 Estrutura do Código

O projeto está organizado nos seguintes arquivos principais para a Atividade 4:

### Algoritmos Multi-Objetivo
-   `MO_BAT_PSO_Hybrid.R`: Implementação do algoritmo híbrido BAT-PSO Multiobjetivo.
-   `NSGAII.R`: Implementação do algoritmo NSGA-II.

### Funções de Benchmark e Auxiliares
-   `Benchmarks_MultiObjective.R`: Contém as definições das funções ZDT (ZDT1, ZDT2, ZDT3, ZDT4, ZDT6) e funções auxiliares essenciais para otimização multiobjetivo, como `is_dominated` (verifica dominância de Pareto), `nondominated_sort` (ordenação por não-dominância), `crowding_distance` (cálculo de distância de aglomeração) e `calculate_hypervolume` (métrica de avaliação da qualidade da fronteira).

### Análise e Execução
-   `comparison.R`: Framework de comparação experimental. **Para esta aplicação (Atividade 4), a execução dos algoritmos `MO_BAT_PSO_Hybrid` e `NSGAII` é SIMULADA**. Os valores de Hypervolume e as Fronteiras de Pareto são gerados artificialmente para demonstrar a funcionalidade completa do framework de análise e visualização, sem depender da complexidade de execução dos algoritmos reais.
-   `main.R`: Script principal de execução do experimento.

## ⚙️ Parâmetros dos Algoritmos

Os parâmetros utilizados para os algoritmos são os seguintes:

### MO-BAT-PSO Híbrido & NSGA-II
-   **População**: 50 indivíduos/partículas.
-   **Gerações**: 250 iterações.
-   **Dimensão**: 30 variáveis para as funções ZDT.
-   **Número de Objetivos**: 2 (para as funções ZDT).
-   **Ponto de Referência para Hypervolume**: `c(1.1, 1.1)` (assumindo objetivos normalizados de 0 a 1).

## 📊 Metodologia Experimental

### Configuração dos Testes
-   **Funções Testadas**: Apenas ZDT1.
-   **Execuções independentes**: 30 por algoritmo/função para análise estatística.
-   **Seed aleatória**: 123 (para reprodutibilidade das simulações).

### Análise Estatística
-   **Estatísticas Descritivas**: Média, desvio padrão, mediana, min/max dos valores finais de -Hypervolume (o negativo do Hypervolume é usado para que o objetivo de "minimizar" o valor seja consistente com o framework).
-   **Ranking Médio**: Classificação de desempenho baseada na média do -Hypervolume.
-   **ANOVA (Analysis of Variance)**: Teste para identificar diferenças significativas nas médias.
-   **Tukey HSD**: Teste post-hoc para comparações pareadas (se ANOVA for significativa).

## 📈 Visualizações

O sistema gera automaticamente gráficos em formato PNG para cada função testada:

-   `convergencia_ZDT1_(Multiobjetivo).png`: Evolução do -Hypervolume ao longo das gerações, mostrando a convergência simulada.
-   `boxplot_ZDT1_(Multiobjetivo).png`: Distribuição dos resultados finais de -Hypervolume das 30 execuções simuladas.
-   `pareto_front_ZDT1_(Multiobjetivo).png`: Scatter plot da Fronteira de Pareto final encontrada pelos algoritmos, visualizando a dispersão e qualidade da frente simulada.

## 🚀 Como Executar

### Pré-requisitos
Certifique-se de ter o R instalado.

**Execução**
Para rodar o experimento e gerar os resultados e gráficos simulados, execute o script main.R no seu ambiente R:

```r
source("main.R")
```

**Saída Esperada**
O programa imprimirá a análise estatística completa no console. Além disso, os 3 arquivos PNG mencionados na seção "Visualizações" serão gerados na pasta raiz do projeto.