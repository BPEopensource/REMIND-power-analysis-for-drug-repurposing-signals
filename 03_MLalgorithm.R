ML_algorithm <- function(X, Y, ML_iter,
                         cores = 4,
                         fold_k = 3,
                         split_prop = 0.5,             # fraction used for lasso selection (D1)
                         unpenalized_pattern = "Demo", # variables matching this are NOT penalized
                         nlambda = 50,
                         select_lambda = "min",
                         selection_threshold = 0.50,
                         seed = NULL,
                         return_full = TRUE
                         ) {
  
  # ---- checks ----
  stopifnot(!is.null(X), !is.null(Y), !is.null(ML_iter))
  stopifnot(length(Y) == nrow(X))
  stopifnot(ML_iter >= 1)
  stopifnot(split_prop > 0 && split_prop < 1)
  
  
  if (!is.null(seed)) set.seed(seed)
  
  N <- nrow(X)
  p <- ncol(X)
  var_names <- colnames(X)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(p))
  
  # ---- penalty factors (0 = unpenalized) ----
  pen_fact <- rep(1, p)
  if (!is.null(unpenalized_pattern) && nzchar(unpenalized_pattern)) {
    pen_fact[grepl(unpenalized_pattern, var_names)] <- 0
  }
  
  # ---- pre-generate splits + folds (so they are reproducible) ----
  n_D1 <- floor(N * split_prop)
  
  idx_D1_list <- vector("list", ML_iter) # rows in D1
  idx_D2_list <- vector("list", ML_iter) # rows in D2
  foldid_list <- vector("list", ML_iter) # fold id for cross validation
  
  all_idx <- seq_len(N)
  
  for (b in seq_len(ML_iter)) {
    idx_D1 <- sample.int(N, size = n_D1, replace = FALSE)
    idx_D2 <- all_idx[-idx_D1]   # complement
    
    idx_D1_list[[b]] <- idx_D1
    idx_D2_list[[b]] <- idx_D2
    
    # fold id bilanciati (meglio di replace=TRUE)
    foldid_list[[b]] <- sample(rep(seq_len(fold_k), length.out = length(idx_D1)))
  }
  
  
  
  # ---- parallel settings ----
  # Use doParallel if cores > 1, otherwise run sequentially
  if (cores > 1) {
    if (!requireNamespace("doParallel", quietly = TRUE)) stop("Install doParallel")
    if (!requireNamespace("foreach", quietly = TRUE)) stop("Install foreach")
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    `%DO%` <- foreach::`%dopar%`
  } else {
    if (!requireNamespace("foreach", quietly = TRUE)) stop("Install foreach")
    foreach::registerDoSEQ()
    `%DO%` <- foreach::`%do%`
  }
  
  # ---- main loop ----
  if (!requireNamespace("glmnet", quietly = TRUE)) stop("Install glmnet")
  
  res_list <- foreach::foreach(
    b = seq_len(ML_iter),
    .packages = c("glmnet", 'speedglm')
  ) %DO% {
    
    # idx_D1 <- which(id_ms[, b] == 1L)
    # idx_D2 <- which(id_ms[, b] == 2L)
    
    idx_D1 <- idx_D1_list[[b]]
    idx_D2 <- idx_D2_list[[b]]
    
    
    X_D1 <- X[idx_D1, , drop = FALSE]
    y_D1 <- Y[idx_D1]
    
    # cv.glmnet fold ids must match length(y_D1)
    foldid <- foldid_list[[b]]
    
    lasso_cv <- glmnet::cv.glmnet(
      x = X_D1,
      y = y_D1,
      family = "binomial",
      standardize = TRUE,
      intercept = TRUE,
      nlambda = nlambda,
      penalty.factor = pen_fact,
      type.measure = "deviance",
      foldid = foldid
    )
    
    if(select_lambda == "min"){
      lam <-  lasso_cv$lambda.min 
    }
    
    if(select_lambda == "1se"){
      lam <-  lasso_cv$lambda.1se 
    }
    
    
    # non-zero coefficients at chosen lambda
    cf <- glmnet::coef.glmnet(lasso_cv$glmnet.fit, s = lam)
    
    # column selected
    selected <- names(which(cf[, 1]!=0))[-1] 
    
    X_D2 <- X[idx_D2, selected, drop = FALSE]
    
    fit <- speedglm::speedglm.wfit(y = Y[idx_D2], X = X_D2, family = stats::binomial(), intercept = T)
    
    betas <- rep(0, p); names(betas) <- var_names
    # map coefficients back (skip intercept)
    cf <- stats::coef(fit)
    cf <- cf[names(cf) != "(Intercept)"]
    betas[names(cf)] <- unname(cf)
    
    list(selected = selected, betas = betas, fit = fit)

  }
  
  # ---- collect results ----
  names(res_list) <- paste0("iter", seq_len(ML_iter))
  
  # ---- selection proportion (always needed) ----
  sel_counts <- table(unlist(lapply(res_list, `[[`, "selected")))
  
  sel_prop <- rep(0, p); 
  names(sel_prop) <- var_names
  
  sel_prop[names(sel_counts)] <- as.numeric(sel_counts) / ML_iter
  
  
  # ---- FULL PIPELINE ----
  beta_mat <- do.call(cbind, lapply(res_list, function(r) r$betas))
  rownames(beta_mat) <- var_names
  
  # exposure summary (works for sparse as well)
  cases <- (Y == 1)
  controls <- (Y == 0)
  exposed_cases <- as.numeric(crossprod(X, cases)) / sum(cases) * 100
  exposed_ctrl  <- as.numeric(crossprod(X, controls)) / sum(controls) * 100
  
  avg_beta <- rowMeans(beta_mat, na.rm = TRUE)
  avg_or <- exp(avg_beta)
  
  tab <- data.frame(
    variable = var_names,
    exposed_cases_pct = round(exposed_cases, 2),
    exposed_controls_pct = round(exposed_ctrl, 2),
    selection_pct = round(sel_prop * 100, 2),
    avg_beta = round(avg_beta, 5),
    avg_or = round(avg_or, 3),
    stringsAsFactors = FALSE
  )
  
  # ---- signals ----
  signals <- tab[tab$selection_pct >= selection_threshold * 100 & tab$avg_beta < 0, ]
  signals <- signals[order(signals$selection_pct, decreasing = TRUE), ]
  
  if (return_full) {
    return(list(signals = signals, tab = tab, beta_mat = beta_mat, res = res_list))
  } else {
    return(signals)
  }
  
  
}

