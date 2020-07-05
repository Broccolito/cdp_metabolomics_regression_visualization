rm(list = ls())
gc()

library(dplyr)

stats = read.csv("pheno_met_regression_stats.csv")

find_metabolites = function(major_phenotype = "hct_vena_mean",
                            major_cutoff = 1e-5,
                            minor_cutoff = 1e-3,
                            met_stats = stats){
  
  met_stats = mutate(met_stats,p_age = as.numeric(p_age)) %>%
    mutate(p_age = as.numeric(as.character(p_age))) %>%
    mutate(p_sex = as.numeric(as.character(p_sex))) %>%
    mutate(p_bmi = as.numeric(as.character(p_bmi))) %>%
    mutate(p = as.numeric(as.character(p))) %>%
    mutate(beta = as.numeric(as.character(beta))) %>%
    mutate(phenotype = as.character(phenotype)) %>%
    mutate(metabolite = as.character(metabolite)) %>%
    filter(p<=minor_cutoff)
  
  stats_sub = filter(met_stats,phenotype == major_phenotype) %>%
    arrange(p) %>%
    filter(p<=major_cutoff) %>%
    left_join(select(met_stats,phenotype,metabolite) %>%
                mutate(other_pheno = phenotype) %>%
                select(-phenotype), by = "metabolite")
  
  other_correlations_list = vector()
  for(m in unique(stats_sub$metabolite)){
    other_correlations = paste(filter(stats_sub,metabolite==m)$other_pheno,collapse = "; ")
    other_correlations_list = rbind(other_correlations_list,c(m,other_correlations))
  }
  
  correlations_list = data.frame(metabolite = other_correlations_list[,1],
                                 phenotype = other_correlations_list[,2])
  
  return(correlations_list)
  
}

