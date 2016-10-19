#' @title Gene expression data set
#'
#' @description Effect sizes and standard errors for an Affymetrix microarray experiment. Only subset of the full experiment is used and the results are for two experimental groups in two different tissues.
#'
#' @details The study design has 4 sets of 7 mice, a sham group, a
#' TSC22D4-knockout group, a LCN13-knockout and a TSC22D4-knockout
#' LCN13-knockout group. Only the sham and the TSC22D4-knockout group
#' are used in this data set.

#'
#' @format A data frame with 25206 rows of gene probes and 7 columns:
#' \describe{
#'
#'  \item{probeset:}{Affymetrix probe set ID.}
#'
#'  \item{beta.sk:}{The effect size from the gene expression output
#' for the skeletal muscle tissue.}
#'
#'  \item{beta.ab:}{The effect size from the gene expression output
#' for the abdominal fat tissue.}
#'
#' \item{se.sk:}{The standard error from the gene expression output
#' for the skeletal muscle tissue.}
#'
#' \item{se.ab:}{The standard error from the gene expression output
#' for the abdominal fat tissue.}
#'
#' \item{Aexprs.sk:}{Average expression in skeletal muscle.}
#'
#' \item{Aexprs.ab:}{Average expression in abdominal fat.}
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
