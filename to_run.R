# ---- clean environment ----
rm(list=ls(all.names=TRUE))

# ---- set the working directory ----
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

# ---- source functions and load libraries ----
source("00_utils.R")          # Miscellaneous utilities
source("02_Create_Pop.R")  # function to generate the input population
source("03_MLalgorithm.R")    # function to run the ML algorithm

# ---- Drug exposure ----
p_drug = fread('00_input/dt_medicament_exposure.csv')[, prev]


# ---- Parameters ----
# params <- list(
#   # n of subjects
#   n_cases        = 560000,
#   matching_ratio = c(1, 2, 3),   # iterable
#   # demographic variable
#   n_demo  = 5,
#   p_demo  = sample(c(0.1, 0.2, 0.3), 5, replace = TRUE),
#   OR_demo = rep(1, 5),
#   # drugs
#   n_drug  = length(p_drug),
#   p_drug  = p_drug,
#   OR_drug = rep(1, length(p_drug)),
#   # hospitalization (categorized)
#   n_hosp  = 41,
#   p_hosp  = sample(p_drug, 41),
#   OR_hosp = c(rep(1, 26),
#               0.9 + 0.2 * plogis(rnorm(5, 0, 1)),
#               1   + 1.5 * plogis(rnorm(5,  0, 1)),
#               0.6 + 0.4 * plogis(rnorm(5,  0, 1)) ),
#   # others (not categorized)
#   n_oth   = 93,
#   p_oth   = sample(p_drug, 93),
#   OR_oth  = c(rep(1, 78),
#               0.9 + 0.2 * plogis(rnorm(5, 0, 1)),
#               1   + 1.5 * plogis(rnorm(5,  0, 1)),
#               0.6 + 0.4 * plogis(rnorm(5,  0, 1)) ),
#   # protective drugs
#   n_prot_drugs  = 1,
#   p_prot_drugs  = 0.001,    # iterable
#   OR_prot_drugs = 0.8,   # iterable
#   # ML algorithm
#   mc_iter   = 100L,
#   ml_splits = 500L,
#   cores     = 4L,
#   # reproducibility
#   seed = 42L
# )
# save(params, file = '00_input/params30.RData')

load('00_input/params30.RData')


# ---- Set progress bar ----
pb <- progress::progress_bar$new(
  format = "  [:bar] :percent |  OR=:OR - m=:m - p=:p | :elapsed elapsed | eta: :eta :spin",   #:current/:total |
  total  = params$mc_iter * length(params$OR_prot_drugs) * length(params$matching_ratio) * length(params$p_prot_drugs), 
  clear  = FALSE, 
  width  = 100
)

# ---- Run Pipeline ----
for (p in params$p_prot_drugs) {
  for (m in params$matching_ratio) {
    for (OR in params$OR_prot_drugs) {
      
     # ---- Results data model ----
      DT <- data.table(
        OR             = numeric(),
        matching_ratio = integer(),
        n_cases        = integer(),
        nDemo          = integer(),
        nDrug          = integer(),
        nHosp          = integer(),
        nOth           = integer(),
        nProt          = integer(),
        select_percent = integer(),
        nFalse_signals = integer(), 
        OR_obs         = integer() 
        #Beta_signal    = integer()
      )
     
     # ---- MC iteration ----
     for (g in 1:params$mc_iter) {

       pb$tick(tokens = list(OR = as.character(OR), 
                             m = as.character(m), 
                             p = as.character(p)))
       
       # ---- population generation ----
       pop <- gen_pop(n_cases        = params$n_cases,
                      matching_ratio = m, 
                      # demographic variable
                      n_demo         = params$n_demo, 
                      p_demo         = params$p_demo,
                      OR_demo        = params$OR_demo, 
                      # drugs
                      n_drug         = params$n_drug, 
                      p_drug         = params$p_drug,
                      OR_drug        = params$OR_drug,
                      # hospitalization 
                      n_hosp         = params$n_hosp, 
                      p_hosp         = params$p_hosp,
                      OR_hosp        = params$OR_hosp,
                      # others
                      n_oth          = params$n_oth, 
                      p_oth          = params$p_oth,
                      OR_oth         = params$OR_oth,
                      # protective drugs
                      n_prot_drugs   = params$n_prot_drugs, 
                      p_prot_drugs   = p,
                      OR_prot_drugs  = OR
       )
       
       a <- sum(pop$X[pop$Y == 1, "xProt1"] == 1)
       b <- sum(pop$X[pop$Y == 1, "xProt1"] == 0)
       
       c <- sum(pop$X[pop$Y == 0, "xProt1"] == 1)
       d <- sum(pop$X[pop$Y == 0, "xProt1"] == 0)
       
       OR_obs <- (a * d) / (b * c)
       
       
       # ---- ML algorithm ----
       ML_results <- ML_algorithm(X = pop$X, 
                                  Y = pop$Y, 
                                  ML_iter = params$ml_splits, 
                                  cores = params$cores)
       
       #View(ML_results$signals)
       signals = ML_results$signals
       print(signals)
       
       # ---- collect single iteration results ----
       DT_res <- data.table(
         OR              = OR,
         matching_ratio  = m,
         n_cases         = params$n_cases,
         nDemo           = params$n_demo,
         nDrug           = params$n_drug,
         nHosp           = params$n_hosp,
         nOth            = params$n_oth,
         nProt           = params$n_prot_drugs,
         select_percent  = ML_results$tab['xProt1', "selection_pct"],
         nFalse_signals  = nrow(signals[signals$variable %like% 'Drug', ]), 
         OR_obs          = round(OR_obs, 2) 
         #Beta_signal     = ML_results$tab['xProt1', "avg_beta"]
       )

       # ---- save ----
       DT <- rbind(DT, DT_res)
       fwrite(DT, paste0("results/Run2_P30_Size_", params$n_cases,
                         "_frqP_", p,
                         "_OR_", OR,
                         "_M_", m,
                         ".csv"))
   }
  }
 }
}

