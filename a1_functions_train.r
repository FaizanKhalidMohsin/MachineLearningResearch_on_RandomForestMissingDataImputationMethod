
check.dim <- function(data) {
	if (is.na(dim(data)[1])) {
		data <- matrix(data, length(data), 1)
	}
	return(data)
}

#impute using the mean (assuming it's all numeric)
mean.imp <- function(data) { 
	data <- check.dim(data)
	#stop('Complete this function.')
	#lapply(data, )
	for (i in 1:ncol(data)){ 
	  data[is.na(data[,i]),i] = mean(data[,i], na.rm=TRUE)
	}
	#for each variable in data, impute the missing values using the mean of the available values
	return(data)
}

#add missingness to a matrix or dataframe
miss.set.df <- function(dat, prop, type.miss='mcar') {
	dat <- check.dim(dat)
	for (i in 1:dim(dat)[2]) {
		dat[,i] <- miss.set.vec(dat[,i], prop, type.miss)
	}
	return(dat)
}
# what is this ?miss.set.vec

#add misingness to a vector; mcar = at random; mnar = not at random (remove higher values)
miss.set.vec <- function(x, prop, type.miss='mcar') {
	n.miss <- rbinom(1, length(x), prop)
	if (type.miss == 'mcar') {
		miss.idx <- sample(1:length(x), n.miss, replace=F)
	} else if (type.miss == 'mnar') {
		miss.idx <- order(x, decreasing=T)[1:n.miss]
	}
	x[miss.idx] <- NA
	return(x)
}

split.data <- function(data, train.prop, set.seed=NA) {
	if (!is.na(set.seed)) {set.seed(set.seed)}
	train.idx <- sample(1:dim(data)[1], round(dim(data)[1]*train.prop), replace=F)
	test.idx <- setdiff(1:dim(data)[1], train.idx)
	train.set <- data[train.idx,]
	test.set <- data[test.idx,]
	return(list(train=train.set, test=test.set))
}


get.resid.err <- function(train.set, test.set) {
	#Train the models and get the MSE
	#stop('Complete this function.')
  
	#fit a linear model on the training set (train.set) using only the intercept Y ~ 1
  model1 = lm(Y ~1, data =  train.set)
  
	#fit a linear model on the training set using all covariates Y ~ .
  modelfull = lm(Y ~., train.set) 
  
	#predict from each model on the test set (test.set)
  predict1 = predict(model1, newdata = test.set[,-91])
  predict.full = predict(modelfull, newdata = test.set[,-91])
  
	#calculate the mse of the null model
  mse.null = mean((predict1 - test.set$Y)^2)
  
	#calculate the mse of the full model
  mse.full = mean((predict.full - test.set$Y)^2)
  
	#calculate the residual error of the ratio of the latter over the former
	pred.res <- mse.full/ mse.null
  
	return(pred.res)
}





missing_function = function(miss_type_idx, miss_prop_idx, data_idx){
  
  train.prop <- 0.50
  train.split.seed <- 78901
  outcome.var <- "Y"
  rf.iter <- 12
  mice.iter <- 10
  
  miss.prop.list <- c(0.10, 0.20, 0.30, 0.40) #proportion of missingness to assign
  miss.type.list <- c('mcar', 'mnar') #mcar = missing completely at random; mnar = missing not at random
  data.idx <- data_idx #the dataset to use
  miss.type.idx <- miss_type_idx #type of missingness to use
  miss.prop.idx <- miss_prop_idx #proportio of missingness to use
  
  t.st <- Sys.time() #time the run
  
  data <- data.list[[data.idx]]
  miss.prop <- miss.prop.list[miss.prop.idx]
  miss.type <- miss.type.list[miss.type.idx]
  
  split <- split.data(data, train.prop, set.seed=train.split.seed)
  train.set <- split$train; test.set <- split$test
  pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]
  
  ##########################
  ## Set the missing data ##
  train.set.miss <- train.set
  train.set.miss[,pred.var] <- miss.set.df(train.set.miss[,pred.var], miss.prop, miss.type)
  
  dim(train.set.miss)
  length(pred.var)
  
  ################
  #MICE Imputation
  mice.data <- train.set.miss
  dim(mice.data)
  
  mice.data0 = mice.data[,pred.var]
  tempdata = mice(mice.data0,m=mice.iter,maxit=25,meth='pmm',seed=train.split.seed)
  
  # Will take the exptectation of the imputated data sets to obtain one data set. 
  imputed = complete(tempdata, 1)
  for (i in 2:mice.iter){
    print(i)
    print(dim(imputed))
    imputed = imputed + complete(tempdata, i) 
    print(dim(imputed))
  }
  
  imputed = imputed / mice.iter
  
  
  #run an imputation on the mice.data dataframe using the mice function from the mice package
  #use 'mice.iter' datasets and cap the iterations at 25
  #DO NOT INCLUDE the outcome 'Y' in the imputation, impute only mice.data[,pred.var]
  #try to parallelize the computation by running several interations in parallel
  
  mice.data.comp = data.frame(imputed, Y = mice.data$Y)
  #write.csv(mice.data.comp, "mice_data_comp.csv", row.names = FALSE)
  #mice.data.comp = read.csv("mice_data_comp.csv")
  # same, but now as list, mild object
  # dslist <- complete(tempdata, "all")
  # length(dslist)
  # imputed_list_data = mean(dslist)
  
  # use package miceadds to save the imputed datasets. 
  # require(miceadds)
  # write.mice.imputation(mi.res=tempdata, "tempdata1", include.varnames=TRUE,
  #       long=TRUE, mids2spss=TRUE, spss.dec=",", dattype=NULL)
  
  # # Parallalize mice imputation. 
  # total.cores <- detectCores()
  # tempdata_core = parlmice(mice.data0, m = mice.iter, seed = NA, cluster.seed = 500, 
  #                          n.core = total.cores,  n.imp.core = NULL, cl.type = "PSOCK")
  mice.data = mice.data.comp
  t.mice = Sys.time() - t.st
  
  
  t.st.mean = Sys.time()
  #####################
  #Impute with the mean
  mean.data <- train.set.miss
  
  #finish the mean.imp function from the functions file and impute mean.data[,pred.var] 
  # maybe should only use this mean.data[,pred.var] 
  
  head(is.na(mean.data))
  is.na(dim(mean.data)[1])
  check.dim(mean.data)
  
  # RUN the mean.imp function. 
  mean.data = mean.imp(mean.data)
  all(!is.na(mean.data))
  
  # Testing if the mean.imp() function works. 
  all(sapply(train.set.miss, function(x) mean(x, na.rm  = T)) == sapply(mean.data, mean))
  summary(sapply(train.set.miss, function(x) mean(x, na.rm  = T)))
  
  t.mean = Sys.time() - t.st.mean
  
  t.st.rf = Sys.time()
  #########################
  #Random Forest Imputation
  total.cores <- detectCores()
  print(total.cores)
  cl <- makeCluster(total.cores)
  registerDoParallel(cl)
  
  rf.data <- train.set.miss[,pred.var]
  dim(rf.data)
  
  #impute using random forest imputation, use 500 trees, and cap the number of iterations at 12 (rf.iter)
  #try to parallelize the forests using the doParallel package to save time
  #?missForest
  
  class(rf.data)
  set.seed(train.split.seed)
  rf.data.comp = missForest(xmis = rf.data, maxiter = rf.iter, ntree = 500, parallelize = c('forests'))
  rf.data.comp.train = data.frame(rf.data.comp$ximp, Y = train.set.miss$Y)
  #write.csv(rf.data.comp.train, "rf_data_comp_train_final.csv", row.names = F)
  #rf.data.comp.train = read.csv("rf_data_comp_train_final.csv")
  #file.remove("rf.data.comp.train.csv")
  # if (file.exists(fn)) 
  #   #Delete file if it exists
  #   file.remove(fn)
  rf.data = rf.data.comp.train
  
  t.rf = Sys.time() - t.st.rf
  
  ##############################
  #finish the get.resid.err function and calculate the test set errors for each imputed dataset
  mean.imp.err <- get.resid.err(mean.data, test.set)
  mice.imp.err <- get.resid.err(mice.data, test.set)
  rf.imp.err <- get.resid.err(rf.data, test.set)
  no.imp.err <- get.resid.err(train.set, test.set)
  
  # mean.imp.err
  # mice.imp.err
  # rf.imp.err
  # no.imp.err
  
  t.end = Sys.time() - t.st
  print(t.end)
  
  return(data.frame(mean.imp.err, mice.imp.err, rf.imp.err, no.imp.err, t.mice, t.mean, t.rf, t.end))
  
}

