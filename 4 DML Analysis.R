# see mlr_learners for list of currently loaded learners
mlr.forest = lrn("regr.ranger")$clone()
mlr.xgboost = lrn("regr.xgboost")$clone()
mlr.svm = lrn("regr.svm")$clone()
mlr.lasso = lrn("regr.cv_glmnet", alpha=1)$clone()
mlr.ridge = lrn("regr.cv_glmnet", alpha=0)$clone()
mlr.boost = lrn("regr.gbm")$clone()
mlr.elastic = lrn("regr.cv_glmnet", alpha=0.5)$clone()

data.dml = double_ml_data_from_data_frame(data.all, y_col="recid", d_cols="prisontime")
model.dmlrf = DoubleMLPLR$new(data.dml, ml_l=mlr.forest, ml_m=mlr.forest)
model.dmlrf$fit()
model.dmlrf$summary()

data.dmlpar = double_ml_data_from_data_frame(data.all[parole==1,], y_col="recid", d_cols="prisontime")
data.dmlfel = double_ml_data_from_data_frame(data.all[felony==1,], y_col="recid", d_cols="prisontime")
data.dmlmis = double_ml_data_from_data_frame(data.all[felony==0,], y_col="recid", d_cols="prisontime")

results = array(0, dim=c(14, 4))
column = 0
for(dml.data in c(data.dml, data.dmlpar, data.dmlfel, data.dmlmis)){
  row = 1
  column = column + 1
  for(dml.model in c(mlr.forest, mlr.xgboost, mlr.svm, mlr.lasso, 
                     mlr.ridge, mlr.boost, mlr.elastic)){
    model.dml = DoubleMLPLR$new(dml.data, ml_l=dml.model, ml_m=dml.model)
    model.dml$fit()
    results[row, column] = model.dml$coef
    results[row+1, column] = model.dml$se
    row = row + 2
  }
}

sens.data = model.matrix(~ .-1-recid-prisontime, data = data.all)
sens.model = dml(data.all$recid, data.all$prisontime, sens.data, model="plm")
summary(sens.model)

sens.object = sensemakr(sens.model, cf.y=0.2, cf.d=0.2)
summary(sens.object)
plot(sens.object, label.unadjusted="", cex.label.text=0.01,
     xlab="Additional R-squared for Treatment Model Confounders",
     ylab="Additional R-squared for Outcome Model Confounders")

# necessary PATCH to calc_confint function that has a typo in stats:::format_perc
calc_confint_patched <- function(cf, ses, params=NULL, level) {
  pnames <- names(ses)
  if (is.matrix(cf))
    cf <- setNames(as.vector(cf), pnames)
  if (is.null(params))
    params <- pnames
  else if (is.numeric(params))
    params <- pnames[params]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  pct <- stats:::format_perc(a, 3)
  ci <- array(NA_real_, dim = c(length(params), 2L), dimnames = list(params, pct))
  ci[] <- cf[params] + ses[params] %o% fac
  ci
}
assignInNamespace("calc_confint", calc_confint_patched, 
                  ns="dml.sensemakr", pos="package:dml.sensemakr")
