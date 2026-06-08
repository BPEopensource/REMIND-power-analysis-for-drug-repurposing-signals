create_cov_category_OR_fast <- function(ncov, prev0, OR, Y,
                                        p_spor_given_ever = 0.5,
                                        p_freq_given_spor = 0.75,
                                        prefix = "x",
                                        add_colnames = TRUE,
                                        categorize = TRUE) {
  stopifnot(length(prev0) == ncov)
  stopifnot(length(OR) == ncov)
  stopifnot(all(prev0 >= 0 & prev0 <= 1))
  stopifnot(all(OR > 0))
  stopifnot(all(Y %in% c(0, 1)))
  
  n <- length(Y)
  
  # p1 from p0 and OR
  p1 <- (OR * prev0) / (1 - prev0 + OR * prev0)
  p1 <- pmin(pmax(p1, 0), 1)
  
  idx0 <- which(Y == 0L)
  idx1 <- which(Y == 1L)
  n0 <- length(idx0)
  n1 <- length(idx1)
  
  # helper: draw indices with prob p (faster than rbinom for many cases)
  draw_idx <- function(idx, p) {
    m <- length(idx)
    if (m == 0L || p <= 0) return(integer(0))
    if (p >= 1) return(idx)
    idx[runif(m) < p]
  }
  
  # accumulate triplets in lists (no quadratic growth)
  i_list <- vector("list", ncov * (if (categorize) 3L else 1L))
  j_list <- vector("list", length(i_list))
  x_list <- vector("list", length(i_list))
  pos <- 0L
  
  if (!categorize) {
    for (k in seq_len(ncov)) {
      ever0 <- if (n0) draw_idx(idx0, prev0[k]) else integer(0)
      ever1 <- if (n1) draw_idx(idx1, p1[k])    else integer(0)
      ever_idx <- c(ever0, ever1)
      if (!length(ever_idx)) next
      
      pos <- pos + 1L
      i_list[[pos]] <- ever_idx
      j_list[[pos]] <- rep.int(k, length(ever_idx))
      x_list[[pos]] <- rep.int(1, length(ever_idx))
    }
    
    i_all <- unlist(i_list[seq_len(pos)], use.names = FALSE)
    j_all <- unlist(j_list[seq_len(pos)], use.names = FALSE)
    x_all <- unlist(x_list[seq_len(pos)], use.names = FALSE)
    
    X_cov <- Matrix::sparseMatrix(i = i_all, j = j_all, x = x_all, dims = c(n, ncov))
    if (add_colnames) colnames(X_cov) <- paste0(prefix, seq_len(ncov))
    
  } else {
    col_ever     <- function(k) 3L * (k - 1L) + 1L
    col_sporadic <- function(k) 3L * (k - 1L) + 2L
    col_frequent <- function(k) 3L * (k - 1L) + 3L
    
    for (k in seq_len(ncov)) {
      ever0 <- if (n0) draw_idx(idx0, prev0[k]) else integer(0)
      ever1 <- if (n1) draw_idx(idx1, p1[k])    else integer(0)
      ever_idx <- c(ever0, ever1)
      if (!length(ever_idx)) next
      
      spor_idx <- draw_idx(ever_idx, p_spor_given_ever)
      freq_idx <- if (length(spor_idx)) draw_idx(spor_idx, p_freq_given_spor) else integer(0)
      
      # ever
      pos <- pos + 1L
      i_list[[pos]] <- ever_idx
      j_list[[pos]] <- rep.int(col_ever(k), length(ever_idx))
      x_list[[pos]] <- rep.int(1, length(ever_idx))
      
      # sporadic
      if (length(spor_idx)) {
        pos <- pos + 1L
        i_list[[pos]] <- spor_idx
        j_list[[pos]] <- rep.int(col_sporadic(k), length(spor_idx))
        x_list[[pos]] <- rep.int(1, length(spor_idx))
      }
      
      # frequent
      if (length(freq_idx)) {
        pos <- pos + 1L
        i_list[[pos]] <- freq_idx
        j_list[[pos]] <- rep.int(col_frequent(k), length(freq_idx))
        x_list[[pos]] <- rep.int(1, length(freq_idx))
      }
    }
    
    if (pos == 0L) {
      X_cov <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0), dims = c(n, 3L * ncov))
    } else {
      i_all <- unlist(i_list[seq_len(pos)], use.names = FALSE)
      j_all <- unlist(j_list[seq_len(pos)], use.names = FALSE)
      x_all <- unlist(x_list[seq_len(pos)], use.names = FALSE)
      X_cov <- Matrix::sparseMatrix(i = i_all, j = j_all, x = x_all, dims = c(n, 3L * ncov))
    }
    
    if (add_colnames) {
      name_drug <- paste0(prefix, seq_len(ncov))
      category  <- c("_ever", "_sporadic", "_frequent")
      colnames(X_cov) <- paste0(rep(name_drug, each = 3), rep(category, times = ncov))
    }
  }
  
  attr(X_cov, "p0") <- prev0
  attr(X_cov, "OR") <- OR
  attr(X_cov, "p1") <- p1
  attr(X_cov, "categorize") <- categorize
  
  X_cov
}
