#' @title Gene expression data set
#'
#' @description Effect sizes and standard errors for an Affymetrix microarray
#' experiment. Only subset of the full experiment is used and the results are
#' for two experimental groups in two different tissues.
#'
#' @details The full experiment has 4 groups of 7 mice and only the sham and the
#' TSC22D4-knockout group are used here.
#'
#' @format A data frame with 25206 rows of genes (probe sets) and 7 columns:
#' \describe{
#'
#'  \item{probeset:}{Affymetrix probe set ID.}
#'
#'  \item{beta.sk:}{The effect size in skeletal muscle tissue.}
#'
#'  \item{beta.ab:}{The effect size in abdominal fat tissue.}
#'
#' \item{se.sk:}{The standard error for the effect size in skeletal muscle
#' tissue.}
#'
#' \item{se.ab:}{The standard error for the effect size in abdominal fat
#' tissue.}
#'
#' \item{Aexprs.sk:}{Average gene expression level in skeletal muscle.}
#'
#' \item{Aexprs.ab:}{Average gene expression leve in abdominal fat.}
#' }
#'
#' @references Friedrich K, Jones A, Seibert O, Sijmonsma TP, Wang X, Sticht C,
#' Gretz N, Fleming T, Nawroth PP, Stremmel W, Rose AJ, Diaz MB, Bluher M,
#' Herzig (2014). Transforming growth factor beta-like stimulated clone 22 D4
#' promotes diabetic hyperglycemia and insulin resistance. \emph{GEO ID:
#' GSE53754}.
#'
#' @docType data
#' @name GEdata
NULL
