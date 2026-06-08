#' Generate simutale population:
#' 

gen_pop <- function(n_cases        = NULL,
                    matching_ratio = NULL, 
                    # demographic variable
                    n_demo         = NULL,
                    p_demo         = NULL,
                    OR_demo        = NULL,
                    # drugs 
                    n_drug         = NULL, 
                    p_drug         = NULL, 
                    OR_drug        = NULL, 
                    # hospitalization 
                    n_hosp         = NULL,
                    p_hosp         = NULL,
                    OR_hosp        = NULL,
                    # others
                    n_oth          = NULL,
                    p_oth          = NULL,
                    OR_oth         = NULL,
                    # protective drugs
                    n_prot_drugs   = NULL,
                    p_prot_drugs   = NULL,
                    OR_prot_drugs  = NULL
                    ){      

  # source function
  source('01_create_Xs.r')
  
  
  # --- Outcome variables ----
  n_controls <- matching_ratio * n_cases
  N <- n_cases + n_controls
  Y <- c(rep(1L, n_cases), rep(0L, n_controls))
  
  
  # --- Demographic variables ----
  X_demo = create_cov_category_OR_fast(ncov = n_demo, 
                                  prev0 = p_demo, 
                                  OR = OR_demo, 
                                  Y,
                                  p_spor_given_ever = 0.5,
                                  p_freq_given_spor = 0.75,
                                  categorize = FALSE,
                                  prefix = "xDemo")
  
  
  # --- Drug variables ----
  X_drug = create_cov_category_OR_fast(ncov = n_drug,
                                  prev0 = p_drug,
                                  OR = OR_drug,
                                  Y,
                                  p_spor_given_ever = 0.5,
                                  p_freq_given_spor = 0.75,
                                  prefix = "xDrug")
  
  # --- Other variables ----
  X_othe = create_cov_category_OR_fast(ncov = n_oth, 
                                  prev0 = p_oth, 
                                  OR = OR_oth, 
                                  Y,
                                  p_spor_given_ever = 0.5,
                                  p_freq_given_spor = 0.75,
                                  categorize = FALSE,
                                  prefix = "xOther")
  
  # --- Hosp variables ----
  X_hosp = create_cov_category_OR_fast(ncov = n_hosp,
                                  prev0 = p_hosp,
                                  OR = OR_hosp,
                                  Y,
                                  p_spor_given_ever = 0.5,
                                  p_freq_given_spor = 0.75,
                                  categorize = TRUE,
                                  prefix = "xHosp")
  
  
  # --- Protective  variables ----
  X_prot = create_cov_category_OR_fast(ncov = n_prot_drugs, 
                                  prev0 = p_prot_drugs, 
                                  OR = OR_prot_drugs, 
                                  Y,
                                  p_spor_given_ever = 0.5,
                                  p_freq_given_spor = 0.75,
                                  categorize = FALSE,
                                  prefix = "xProt")
  
  
  # --- Cbinding ----
  X = do.call(cbind, list(X_demo,  X_othe, X_prot, X_hosp,X_drug)) #
  
  return(
    list(X = X, 
         Y = Y)
    )
}        



# n_cases        = 1000
# matching_ratio = 3
# # demographic variable
# n_demo         = 10
# p_demo         = sample(p_drug, 10)
# OR_demo        = 0.5 + 1 * plogis(rnorm(10, 0, 1))
# # drugs
# n_drug         = 540
# p_drug         = p_drug
# OR_drug        = rep(1, 540)
# # hospitalization and others
# n_hosp         = 10
# p_hosp         = sample(p_drug, 10)
# OR_hosp        = 0.5 + 1.5 * plogis(rnorm(10, 0, 1))
# # protective drugs
# n_prot_drugs   = 1
# p_prot_drugs   = 0.02
# OR_prot_drugs  = 0.8