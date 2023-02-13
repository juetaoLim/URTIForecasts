##load in specific dataframe
rm(list=ls())
require(parallel)
require(foreach)
require(doParallel)
cd <- ""  #change to own directory here
files <- list.files(paste0(cd,"out/forecast_df/"))
source(paste0(cd,"functions/forecasts.R"))
#create directory for forecasts to be stored
subDir <- file.path(paste0(cd,"out/forecast_ouput/"))
ifelse(!dir.exists(subDir), dir.create(subDir), FALSE)
#percentage of data to be taken as initial training set
init_train_percen <- 0.6

#disease names and lists to be referenced later
dis_names <- gsub("\\..*","", files)

#for each disease condition of interest, generate point forecasts using LASSO


# for (i in files){
  
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

foreach(i = files,k=dis_names,
          .export = ls(globalenv())) %dopar% {
  require(glmnet)
  require(sparsegl)
  require(gbm)
  #load in list containing x and y for each time horizon
  # k <- 1
  load(paste0(cd,"out/forecast_df/",i))
  temp_list <- get(k) #get(dis_names[k])
  temp_forecast_store <- list()
  
  #for each time horizon, generate point forecasts using specified models
  # for (j in 1:length(temp_list)){
  #try month ahead forecasts first
    for (j in 1:8){
    temp_df <- temp_list[[j]]
    test_ind <- round(nrow(temp_df) * init_train_percen)
    test_ind <- seq(test_ind,nrow(temp_df))
    y_pred_store <- rep(NA,nrow(temp_df))
    y_pred_store <- as.list(y_pred_store)
    
    #index each time point
    for (forecast_ind in test_ind){
      train_ind <- 1:(forecast_ind-1)
      y_train <- temp_df[train_ind,1]
      y_test  <- temp_df[forecast_ind,1]
      x_train <- temp_df[train_ind,-1]
      x_test  <- temp_df[forecast_ind,-1]
      
      y_pred <- forecaster(x_train=x_train,x_test=x_test,y_train=y_train,y_names=k)
      y_pred_store[[forecast_ind]] <- y_pred
    }
    
    temp_df <- cbind(do.call(rbind,y_pred_store),temp_df)
    temp_forecast_store[[j]] <- temp_df
  }
  
  # assign(paste(dis_names[k],"Forecast"),temp_forecast_store)
  # save(list=dis_names[k],file=paste0(cd,"out/forecast_ouput/",dis_names[k],".rds"))
  
  assign(paste(k,"Forecast"),temp_forecast_store)
  save(list=paste(k,"Forecast"),file=paste0(cd,"out/forecast_ouput/",k,".rdata"))
  
  k<- k+1
  
}

stopCluster(cl)
