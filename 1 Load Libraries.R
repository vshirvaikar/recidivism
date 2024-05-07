# libraries for directed acyclic graph (DAG) analysis
library(dagitty)
library(pcalg)
library(Rgraphviz)
library(MXM)

# libraries for double/debiased machine learning (DML) analysis
library(olsrr)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(xgboost)
library(e1071)
library(glmnet)
## [optional] can install extra learners using devtools
library(devtools)
devtools::install_github("mlr-org/mlr3extralearners@*release")
library(mlr3extralearners)

# libraries for DML sensitivity analysis
## not available on CRAN as of May 2024 and requires devtools
library(devtools)
devtools::install_github("carloscinelli/dml.sensemakr")
library(dml.sensemakr)
