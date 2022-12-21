#takes in train, test sets for independent (x) and dependent variables (y)
#pushes out y_pred, which is a vector of model predictions given test set independent variables
#use Poisson link for most forecasts, except SGL
forecaster <- function(x_train,x_test,y_train,y_names=k){
  
  #AR
  x_names <- sub(" .*", "", colnames(x_train))
  df_ar <- x_train[,which(x_names==y_names)]
  df_ar <- data.frame(y_train,df_ar,check.names = F)
  mod_ar <- glm(y_train~.,data=df_ar)
  mod_ar <- step(mod_ar,direction="backward",trace=0)
  
  mod_ar_nameind <- gsub('`', '', names(mod_ar$coefficients))
  
  if (length(mod_ar$coefficients) >1 & "(Intercept)" %in% mod_ar_nameind ){
  y_pred_ar <- mod_ar$coefficients %*% c(1,x_test[which(names(x_test) %in%mod_ar_nameind)])}
  
  if (length(mod_ar$coefficients) ==1 & "(Intercept)" %in% mod_ar_nameind){
    y_pred_ar <- mod_ar$coefficients}
  
  if (!("(Intercept)" %in% mod_ar_nameind)){
    y_pred_ar <- mod_ar$coefficients %*% c(x_test[which(names(x_test) %in%mod_ar_nameind)])}
  # y_pred_ar <- exp(y_pred_ar)
  #LASSO
  cv_lasso <- cv.glmnet(x=x_train,y=y_train,standardize=TRUE)
  mod_lasso <- glmnet(y=y_train,x=x_train,lambda=cv_lasso$lambda.min,standardize=TRUE)
  y_pred_lasso <- predict.glmnet(mod_lasso,newx=t(as.matrix(x_test)))
  # y_pred_lasso <- exp(y_pred_lasso)
  #enet
  cv_enet <- cv.glmnet(x=x_train,y=y_train,standardize=TRUE,alpha=0.5)
  mod_enet <- glmnet(y=y_train,x=x_train,lambda=cv_lasso$lambda.min,standardize=TRUE,alpha=0.5)
  y_pred_enet <- predict.glmnet(mod_lasso,newx=t(as.matrix(x_test)))
  
  # y_pred_lasso <- exp(y_pred_lasso)
    #adaptive LASSO
  # cv_ridge <- cv.glmnet(x_train, y_train, alpha=0,standardize=TRUE,family='poisson')
  # w3 <- 1/abs(matrix(coef(cv_ridge, s=cv_ridge$lambda.min)
  #                    [, 1][2:(ncol(x_train)+1)] ))^1 ## Using gamma = 1
  # w3[w3[,1] == Inf] <- 999999999 ## Replacing values estimated as infinite for 999999999
  # 
  # set.seed(999)
  # cv_alasso <- cv.glmnet(x_train, y_train, alpha=1, standardize=TRUE, penalty.factor=w3,family='poisson')
  # mod_alasso <- glmnet(y=y_train,x=x_train,lambda=cv_alasso$lambda.min,standardize=TRUE,family='poisson')
  # y_pred_alasso <- predict.glmnet(mod_alasso,newx=t(as.matrix(x_test)))
  # y_pred_alasso <- exp(y_pred_alasso)
  #boosting

  boost_df_train <- data.frame(Y=round(y_train),x_train,check.names=F)
  boost_df_test <- data.frame(t(x_test),check.names=F)
  mod_gbm <- gbm(Y ~ ., data = boost_df_train)
  pred_gbm <- predict(mod_gbm,newdata=boost_df_test)
  
  #pred_gbm <- exp(pred_gbm)
  #sparse group LASSO
  #take groups as 
  # groups <- sub(" .*", "", colnames(x_train))
  # groups <- as.numeric(as.factor(groups))
  # #simple sort of numeric as lags are same
  # groups <- sort(groups)
  # cv_sparseGroup <- cv.sparsegl(x=x_train,y=y_train, group = groups,
  #                         family = "gaussian")
  # y_pred_SGL <- predict(cv_sparseGroup,newx=x_test,s="lambda.min")
  
  # forecast combination of disease case counts
  forecasts <- c(y_pred_ar,y_pred_lasso,y_pred_enet,pred_gbm)
  forecasts = c(forecasts,mean(forecasts))
  names(forecasts) <- c('y_pred_ar','y_pred_lasso','y_pred_enet','pred_gbm',"combination")
  
  # forecasts <- c(y_pred_ar,y_pred_lasso,y_pred_alasso,y_pred_SGL)
  # forecasts = c(forecasts,mean(forecasts))
  # names(forecasts) <- c('y_pred_ar','y_pred_lasso','y_pred_alasso','y_pred_SGL',"combination")
  
  return(forecasts)
  
}

#old function to test stepwise predictions

forecaster1 <- function(x_train,x_test,y_train,y_names=k){
  
  #AR
  x_names <- sub(" .*", "", colnames(x_train))
  df_ar <- x_train[,which(x_names==y_names)]
  df_ar <- data.frame(y_train,df_ar,check.names = F)
  mod_ar <- glm(y_train~.,data=df_ar)
  mod_ar <- step(mod_ar,direction="backward",trace=0)
  
  mod_ar_nameind <- gsub('`', '', names(mod_ar$coefficients))
  
  if (length(mod_ar$coefficients) >1 & "(Intercept)" %in% mod_ar_nameind ){
    y_pred_ar <- mod_ar$coefficients %*% c(1,x_test[which(names(x_test) %in%mod_ar_nameind)])}
  
  if (length(mod_ar$coefficients) ==1 & "(Intercept)" %in% mod_ar_nameind){
    y_pred_ar <- mod_ar$coefficients}
  
  if (!("(Intercept)" %in% mod_ar_nameind)){
    y_pred_ar <- mod_ar$coefficients %*% c(x_test[which(names(x_test) %in%mod_ar_nameind)])}
  
  
  return(y_pred_ar)
  
}

