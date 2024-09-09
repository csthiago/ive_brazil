library(mgcv)
library(marginaleffects)

#helper functions
write_comparisons <- function(mod,
                              dataframe) {
  resultlist <- NULL
  for (i in 1:nrow(dataframe)) {
    val1 <- dataframe[i,1][[1]]
    val2 <- dataframe[i,2][[1]]
    temp_result <- avg_comparisons(mod,
                                   variables =  list(cobertura_resid2 = c(val1,val2)),
                                   newdata = "tukey",
                                   comparison = "lnratioavg",
                                   transform = "exp")
    resultlist[[i]] <- as_tibble(as.matrix(temp_result))
    
  }
  enframe(resultlist) |> unnest(value)
  
}
# Model Including only unvaccinated
mod_main <- bam(
  confirmado  ~
    s(cobertura_resid2)+
    s(temporal_trend, cod_uf, bs="fs", k=15)+
    s(idade) +
    s(cod_rgi, bs="re")+ # immediate region
    s(IDHM,bs="cr") +
    s(gdp_percapita, bs="cr", k=15)+
    s(pop,bs="cr", k=15) +
    sexo+
    raca+
    number_tests+
    prev_infec + 
    n_comorb_cat,
  data = dt_uv,
  discrete = T,
  nthreads = ncore,
  method = "fREML",
  family = binomial
)

comparisons_df <- tibble::as_tibble(t(data.frame(
  c(0.4,0.5),
  c(0.4,0.6),
  c(0.4,0.7),
  c(.4,.8))))
result_main_omicron <- write_comparisons(mod_main, comparisons_df)

# Model including vaccinated and unvaccinated
mod_vac <- bam(
  confirmado  ~
    vacc_status + 
    s(cobertura_resid2) + 
    s(cobertura_resid2, by=vacc_status, m=1)+
    s(temporal_trend, cod_uf, bs="fs", k=15)+
    s(cod_rgi, bs="re")+
    s(IDHM,bs="cr") +
    s(idade) + 
    s(gdp_percapita, bs="cr", k=15)+
    s(pop,bs="cr", k=15) +
    sexo+
    raca+
    number_tests+
    prev_infec + 
    n_comorb_cat,
  data = dt_vac,
  discrete = T,
  nthreads = 10,
  method = "fREML",
  family = binomial
)

# Simple model
mod_sens6 <- glm(
  confirmado  ~
    decile_coverage+
    week_dummy +
    age_cut +
    cod_uf+
    rms::rcs(IDHM) +
    rms::rcs(pop) +
    gdp_quartile+
    sexo+
    raca+
    number_tests+
    prev_infec + 
    n_comorb_cat,
  data = dt_uv,
  family = binomial
)