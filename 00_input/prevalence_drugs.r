############################################################
## Proportion of users by ATC5 and age class (AMELI) using INSEE
##
## Sources:
##  - AMELI / CNAM OpenMedic 2024:
##    "NB_2024_atc5_age_sexe.CSV"
##     https://www.data.gouv.fr/datasets/open-medic-base-complete-sur-les-depenses-de-medicaments-interregimes/
##
##  - INSEE population structure by age (France, 2011/2016/2022):
##    https://www.insee.fr/fr/statistiques/2011101?geo=FE-1
##
############################################################

# ---- library
library(data.table)

# ---- load data
ameli_file <- "input/NB_2024_atc5_age_sexe.CSV"
## File: NB_2024_atc5_age_sexe.CSV
## Columns (typical):
##  - ATC5   : ATC level-5 code
##  - l_atc5 : ATC level-5 label
##  - age    : age class code (e.g. 0, 20, 60, 99)
##  - sexe   : sex code
##  - nbc    : number of users (consumers) in that stratum


dt_ameli <- fread(
  ameli_file,
  sep = ";",        
  encoding = "UTF-8"
)

dt_ameli_agg <- dt_ameli[
  ,
  .(users = sum(nbc, na.rm = TRUE)),
  by = .(ATC5, l_atc5, age)
]

## denominator
insee_file <- "input/pop_age.csv"

dt_insee_raw <- fread(
  insee_file,
  sep = ";"
)



# ---- processing

setnames(
  dt_insee_raw,
  old = c("Âge", "2011", "2016", "2022"),
  new = c("age_group", "pop_2011", "pop_2016", "pop_2022")
)


dt_insee <- dt_insee_raw[
  ,
  .(age_group, pop_2011, pop_2016, pop_2022)
]

dt_insee <- dt_insee[age_group != "Ensemble"]

dt_insee[
  ,
  pop_2022 := as.integer(gsub("\u00A0", "", pop_2022, perl = TRUE))
]



# ---- adapting age classes
pop_0_14   <- dt_insee[age_group == "0 à 14 ans",    pop_2022]
pop_15_29  <- dt_insee[age_group == "15 à 29 ans",   pop_2022]
pop_30_44  <- dt_insee[age_group == "30 à 44 ans",   pop_2022]
pop_45_59  <- dt_insee[age_group == "45 à 59 ans",   pop_2022]
pop_60_74  <- dt_insee[age_group == "60 à 74 ans",   pop_2022]
pop_75plus <- dt_insee[age_group == "75 ans ou plus",pop_2022]
pop_99plus <- 0

pop_15_19 <- pop_15_29 * (5/15)
pop_20_29 <- pop_15_29 * (10/15)

pop_0_19 <- pop_0_14 + pop_15_19

pop_20_59 <- pop_20_29 + pop_30_44 + pop_45_59

pop_60_98 <- pop_60_74 + pop_75plus


pop_ameli <- data.table(
  age = c(0L, 20L, 60L, 99L),
  pop = c(pop_0_19, pop_20_59, pop_60_98, pop_99plus)
)



# ---- 4. Merge numerators (AMELI) with denominators (INSEE)

dt_prev_long <- merge(
  dt_ameli_agg,
  pop_ameli,
  by = "age",
  all.x = TRUE
)

dt_prev_long[
  ,
  prevalence := users / pop
]

cat("Prevalence by ATC5 and age (long format):\n")
print(head(dt_prev_long))

# ---- wide table 

dt_prev_wide <- dcast(
  dt_prev_long,
  ATC5 + l_atc5 ~ age,
  value.var = "prevalence"
)

setnames(
  dt_prev_wide,
  old = c("0", "20", "60", "99"),
  new = c("age_0", "age_20", "age_60", "age_99")
)


dt_prev_wide[
  ,
  prev := rowMeans(.SD, na.rm = TRUE),
  .SDcols = c("age_20", "age_60")
]

# ---- keep only medicament with prevalence > 0.1 %
dt <- dt_prev_wide[ prev > 0.001, .(ATC5, l_atc5, prev)]

fwrite(dt, file = 'input/dt_medicament_exposure.csv')





# ---- plt

quantile(dt$prev,
         probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
         na.rm = TRUE)

# maybe we can use prev = 0.002, 0.006 and 0.016 --> q25, q50 e q75

library(ggplot2)

ggplot(dt, aes(x = prev)) +
  geom_histogram(#aes(y = ..density..),
                # binwidth = 0.05,  # cambia se vuoi più/meno barre
                 boundary = 0,
                 closed = "right",
                 color = "black",
                 fill = "grey80") +
  #geom_density(linewidth = 1) +
  scale_y_continuous(limits = c(0, 125))+
  scale_x_continuous(limits = c(0, 0.7), 
                     breaks = seq(0, 0.7, by = 0.1)) +
  labs(title = 'All drugs: 540', x = "Proportion of exposed", y = "N") +
  theme_minimal()

head(dt[order(-prev)])
##
dt[prev < 0.01, .N]
ggplot(dt[prev < 0.01], aes(x = prev)) +
  geom_histogram(boundary = 0,
                 closed = "right",
                 color = "black",
                 fill = "grey80") +
  scale_x_continuous(limits = c(0, 0.01), 
                     breaks = seq(0, 0.01, by = 0.001)) +
  labs(title = 'prev < 0.01: 329 drug (~ 60%)', x = "Proportion of exposed", y = "N") +
  theme_minimal()
##

bw <- 0.001  # 1 per mille

ggplot(dt[prev < 0.01], aes(x = prev)) +
  geom_histogram(
    binwidth = bw,
    color = "black",
    fill = "grey80"
  ) +
  scale_x_continuous(
    limits = c(0, 0.01),
    breaks = seq(0, 0.01, by = bw)
  ) +
  labs(title = "prev < 0.01: 329 drug (~ 60%)",
       x = "Proportion of exposed", y = "N") +
  theme_minimal()

