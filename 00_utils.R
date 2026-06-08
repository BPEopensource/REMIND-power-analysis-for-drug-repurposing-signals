# Libraries ----
library(Matrix)
library(tidyverse)
library(glmnet)
library(speedglm)
library(doParallel)
library(foreach)
library(data.table)

# functions 

p0_from_OR <- function(OR, p1) {
  p0 <- p1 / (OR * (1 - p1) + p1)
  return(p0)
}

p1_from_OR <- function(OR, p0) {
  p1 <- (p0 * OR) / ((1 - p0) + p0 * OR)
  return(p1)
}


