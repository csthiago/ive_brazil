pacman::p_load(tidyverse, tidytable)
pop_cidade <- readxl::read_excel("populacao 2022.xlsx") |> 
  mutate(cod_ibge = stringr::str_sub(cod_ibge,1,6))
pop_idade <- readxl::read_excel("populacao_idade.xlsx")
pop_idade <- pop_idade |>
  mutate(cod_ibge = stringr::str_sub(cod_ibge,1,6)) |> 
  mutate(across(Total:last_col(), as.numeric)) |> 
  pivot_longer(cols = -c(cod_ibge:Total)) |>
  mutate(
    name = if_else(name == "Menos de 1 ano", as.character(0), name),
    name = readr::parse_number(name),
    value = as.numeric(value),
    value = replace_na(value, 0)
  ) |>
  arrange(cod_ibge, name) |>
  mutate(ageRange = case_when(
    name < 5 ~ "<5",
    name < 12 ~ "5-11",
    name < 18 ~ "12-17",
    name < 40 ~ "18-39",
    name < 60 ~ "40-59",
    name < 80 ~ "60-79",
    name >= 80 ~ "80+"
  )) |>
  group_by(cod_ibge, ageRange) |>
  summarise(n = sum(value))

pop_brasil_idade <- pop_idade |>
  filter(cod_ibge == 1)
vac <- arrow::read_parquet("vac_clean_minimal/part-00000-79d53570-e4e0-43c8-8703-0097dbf04564-c000.snappy.parquet")

#tbl_vars(vac)
vac <- vac |>
  filter(flag_incon == 0) |>
  select(
    id_vigvac, d_sexo,
    d_idade, d_codibge,
    d1_estab_mun_codigo,
    d1_nome_vacina, d1_data_aplicacao,
    d2_nome_vacina, d2_data_aplicacao,
    d3_nome_vacina, d3_data_aplicacao
  )
vac <- vac |>
  filter(d_idade >= 0, d_idade <= 110) |>
  mutate(
    age_levels = case_when(
      d_idade < 5 ~ "<5",
      d_idade < 12 ~ "5-11",
      d_idade < 18 ~ "12-17",
      d_idade < 40 ~ "18-39",
      d_idade < 60 ~ "40-59",
      d_idade < 80 ~ "60-79",
      d_idade >= 80 ~ "80+"
    )
  ) |>
  filter(case_when(
    age_levels == "<5" & d1_data_aplicacao < "2022-06-01" ~ FALSE,
    age_levels == "5-11" & d1_data_aplicacao < "2021-12-15" ~ FALSE,
    age_levels == "12-17" & d1_data_aplicacao < "2021-06-11" ~ FALSE,
    TRUE ~ TRUE
  ))
manu_levels <- c("uv", "BNT162b2", "CV", "AZ", "Ad26")

manu_labels <- c(
  uv = "Unvaccinated", CV = "CoronaVac",
  BNT162b2 = "BNT162b2", Ad26 = "Ad26.COV2.S",
  AZ = "ChAdOx1"
)

# time to count individuals in each group
wait <- 14

# vaccination BRL
first_day <- make_date(2021, 01, 18)
# first_day <- floor_date(first_day,"week",week_start = 7)
last_day <- make_date(2022, 4, 30)
# last_day <- floor_date(last_day,"week" , week_start = 7)

all_dates <- tibble(date = seq(first_day, last_day, "1 days"))

age_starts <- c(5, 12, 18, 40, 60, 80)
age_ends <- c(age_starts[-1] - 1, Inf)
age_levels <- paste(age_starts, age_ends, sep = "-")
age_levels[length(age_levels)] <- paste0(age_starts[length(age_levels)], "+")

all_combs <- expand_grid(
  date = all_dates$date,
  ageRange = factor(age_levels, levels = age_levels),
  manu = manu_levels[manu_levels != "uv"]
)

daily_counts_vax_onedose_age_manu <- vac %>%
  filter(d_sexo != "I") %>%
  mutate(
    date = pmax(d1_data_aplicacao + days(wait), first_day),
    ageRange = age_levels,
    manu = d1_nome_vacina
  ) %>%
  group_by(date, ageRange, manu) %>%
  summarize(vax = n(), .groups = "drop") %>%
  ungroup() %>%
  right_join(all_combs, by = c("date", "ageRange", "manu")) %>%
  mutate(vax = replace_na(vax, 0)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange, manu)


## Cumulative sum of one dose vaccinated individuals by age range, gender and
## vaccine manufacturer

counts_onedose_age_gender_manu <-
  daily_counts_vax_onedose_age_manu %>%
  group_by(ageRange, manu) %>%
  mutate(vax = cumsum(vax)) %>%
  ungroup()

## Population of unvaccinated individuals by age Range, date and gende

pop_unvax <- counts_onedose_age_gender_manu %>%
  group_by(date, ageRange) %>%
  summarize(total = sum(vax), .groups = "drop") %>%
  ungroup() %>%
  left_join(pop_brasil_idade, by = c("ageRange")) %>%
  mutate(n = n - total, manu = "uv") %>%
  select(-total)




### Cobertura ####

all_combs_cover <- expand_grid(
  date = all_dates$date,
  cod_ibge = unique(pop_idade$cod_ibge[pop_idade$cod_ibge!=1]),
  ageRange = factor(age_levels, levels = age_levels),
  manu = manu_levels[manu_levels != "uv"]
)

daily_coverage_one_dose <- vac %>%
  filter(d_sexo != "I") %>%
  mutate(
    date = pmax(d1_data_aplicacao + days(wait), first_day),
    ageRange = age_levels,
    manu = d1_nome_vacina,
    cod_ibge = d_codibge
  ) %>%
  group_by(cod_ibge, date, ageRange, manu) %>%
  summarize(vax = n(), .groups = "drop") |> 
  ungroup() %>%
  right_join(all_combs_cover, by = c("date", "ageRange", "manu", "cod_ibge")) %>%
  mutate(vax = replace_na(vax, 0)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange, manu)


daily_coverage_second_dose <- vac %>%
  filter(d_sexo != "I") %>%
  mutate(
    date = pmax(d2_data_aplicacao + days(wait), first_day),
    ageRange = age_levels,
    manu = d2_nome_vacina,
    cod_ibge = d_codibge
  ) %>%
  group_by(cod_ibge, date, ageRange, manu) %>%
  summarize(vax = n(), .groups = "drop") |> 
  ungroup() %>%
  right_join(all_combs_cover, by = c("date", "ageRange", "manu", "cod_ibge")) %>%
  mutate(vax = replace_na(vax, 0)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange, manu)



daily_coverage_third_dose <- vac %>%
  filter(d_sexo != "I") %>%
  mutate(
    date = pmax(d3_data_aplicacao + days(wait), first_day),
    ageRange = age_levels,
    manu = d3_nome_vacina,
    cod_ibge = d_codibge
  ) %>%
  group_by(cod_ibge, date, ageRange, manu) %>%
  summarize(vax = n(), .groups = "drop") |> 
  ungroup() %>%
  right_join(all_combs_cover, by = c("date", "ageRange", "manu", "cod_ibge")) %>%
  mutate(vax = replace_na(vax, 0)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange, manu)



### pop unvax city ####



pop_unvax_city <-
  daily_coverage_one_dose |> 
  group_by(cod_ibge,ageRange, manu)|> 
  mutate(vax = cumsum(vax))  |> 
  group_by(cod_ibge,date,ageRange) |> 
  summarize(total_resid1 = sum(vax), .groups = "drop") |> 
  ungroup() |> 
  left_join(pop_idade, by=c("cod_ibge", "ageRange")) |> 
  mutate(cobertura_resid1 = total_resid1/n,
         pop_unvax = n - total_resid1) 
write_csv(pop_unvax_city,"pop_unvax_city_age.csv")

coverage_first_dose <-
  daily_coverage_one_dose |> 
  group_by(cod_ibge,ageRange, manu)|> 
  mutate(vax = cumsum(vax))  |> 
  group_by(cod_ibge,date) |> 
  summarize(total_resid1 = sum(vax), .groups = "drop") |> 
  ungroup() |> 
  left_join(pop_cidade) |> 
  mutate(cobertura_resid1 = total_resid1/pop) 

coverage_second_dose <-
  daily_coverage_second_dose |> 
  group_by(cod_ibge,ageRange, manu)|> 
  mutate(vax = cumsum(vax))  |> 
  group_by(cod_ibge,date) |> 
  summarize(total_resid2 = sum(vax), .groups = "drop") |> 
  ungroup() |> 
  left_join(pop_cidade) |> 
  mutate(cobertura_resid2 = total_resid2/pop)

coverage_third_dose <-
  daily_coverage_third_dose |> 
  group_by(cod_ibge,ageRange, manu)|> 
  mutate(vax = cumsum(vax))  |> 
  group_by(cod_ibge,date) |> 
  summarize(total_resid3 = sum(vax), .groups = "drop") |> 
  ungroup() |> 
  left_join(pop_cidade) |> 
  mutate(cobertura_resid3 = total_resid3/pop)
# Cap coverage at 95% percent
coverage <- coverage_first_dose |> left_join(coverage_second_dose) |> 
  left_join(coverage_third_dose) |> 
  mutate(across(starts_with("cobertura_"),~if_else(is.na(.x),0,.x)),
         across(starts_with("cobertura_"), ~if_else(.x>0.95,.95,.x),.names="{.col}_fix"))

