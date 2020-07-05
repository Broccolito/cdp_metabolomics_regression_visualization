library(dplyr)

rm(list = ls())
gc()

suppressWarnings({
  phenotype_apriori = read.csv("phenotype_apriori.csv") %>%
    mutate_if(is.factor,as.character)
  met = read.csv("metabolite_master.csv")
  pheno = read.csv("2015_aug_dec_2016_dec.csv")
  pheno_met = inner_join(pheno,met,by = "id") %>%
    mutate(bmi = weight/(height^2))
  pheno = select(pheno_met,id,bmi,phenotype_apriori$phenotype)
  controlled_pheno = select(pheno,id,sex,age,bmi)
  pheno = select(pheno,-id,-sex,-age,-bmi) %>%
    mutate_if(is.factor,as.numeric) %>%
    cbind.data.frame(controlled_pheno)
  met = select(pheno,id) %>%
    left_join(met,by = "id") %>%
    select(-id)
  
  met_names = paste0("M",seq(1,5957))
  pheno_names = phenotype_apriori$phenotype[-(1:2)]
})

res_mat = vector()
for(ph in pheno_names){
  for(mt in met_names){
    try({
      
      xydata = data.frame(sex = pheno[["sex"]],
                          age = pheno[["age"]],
                          bmi = pheno[["bmi"]],
                          metabolite = met[[mt]],
                          phenotype = pheno[[ph]]) %>%
        na.omit()
      
      ph_upper = mean(xydata$phenotype) + 5*sd(xydata$phenotype)
      ph_lower = mean(xydata$phenotype) - 5*sd(xydata$phenotype)
      mt_upper = mean(xydata$metabolite) + 5*sd(xydata$metabolite)
      mt_lower = mean(xydata$metabolite) - 5*sd(xydata$metabolite)
      outlier_index = c(which(xydata$metabolite<mt_lower),
                        which(xydata$metabolite>mt_upper),
                        which(xydata$phenotype<ph_lower),
                        which(xydata$phenotype>ph_upper))
      n_outlier = length(outlier_index)
      
      if(n_outlier!=0){
        xydata = xydata[-outlier_index,]
      }
      
      n = dim(xydata)[1]
      nm = dim(filter(xydata,sex == "M"))[1]
      nf = dim(filter(xydata,sex == "F"))[1]
      p_age = 1
      p_sex = 1
      p_bmi = 1
      p = 1
      beta = 0
      
      if(dim(xydata)[1]>=30){
        if(length(unique(xydata$sex))>1){
          fm = "phenotype ~ metabolite + bmi + age + sex"
          l = summary(lm(formula = fm, data = xydata))
          p = l$coefficients[2,4]
          p_bmi = l$coefficients[3,4]
          p_age = l$coefficients[4,4]
          p_sex = l$coefficients[5,4]
          beta = l$coefficients[2,1]
        }else{
          # Use alternative formula
          fma = "phenotype ~ metabolite + bmi + age"
          la = summary(lm(formula = fma, data = xydata))
          p = la$coefficients[2,4]
          p_bmi = la$coefficients[3,4]
          p_age = la$coefficients[4,4]
          p_sex = -1
          beta = la$coefficients[2,1]
        }
      }
      
      res = data.frame(phenotype = ph,
                       metabolite = mt,
                       n = n, nm = nm, nf = nf,
                       n_outlier = n_outlier,
                       p_age = p_age,
                       p_sex = p_sex,
                       p_bmi = p_bmi,
                       p = p,
                       beta = beta)
      if(res$p<=0.05){
        res_mat = rbind.data.frame(res_mat,res)
        cat(paste0("Significant Correlation found between ",
                   ph," and ",mt,"\n"))
      }
      
    },silent = TRUE)
  }
}

write.csv(res_mat, file = "pheno_met_regression_stats.csv",quote = FALSE,row.names = FALSE)
