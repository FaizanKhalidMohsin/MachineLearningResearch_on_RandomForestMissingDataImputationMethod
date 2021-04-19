

rm(list=ls())

library(missForest)
library(mice)
library(doParallel)
library(doMC)
library(tools)

source('a1_functions_train.r')
load('a1_simulated_data.RData')

train.prop <- 0.50
train.split.seed <- 78901
outcome.var <- "Y"
rf.iter <- 12
mice.iter <- 10

miss.prop.list <- c(0.10, 0.20, 0.30, 0.40) #proportion of missingness to assign
miss.type.list <- c('mcar', 'mnar') #mcar = missing completely at random; mnar = missing not at random
data.idx <- 1 #the dataset to use
miss.type.idx <- 1 #type of missingness to use
miss.prop.idx <- 2 #proportio of missingness to use

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

################
#MICE Imputation
mice.data <- train.set.miss

#run an imputation on the mice.data dataframe using the mice function from the mice package
#use 'mice.iter' datasets and cap the iterations at 25
#DO NOT INCLUDE the outcome 'Y' in the imputation, impute only mice.data[,pred.var]
#try to parallelize the computation by running several interations in parallel

#####################
#Impute with the mean
mean.data <- train.set.miss

#finish the mean.imp function from the functions file and impute mean.data[,pred.var]

#########################
#Random Forest Imputation
total.cores <- detectCores()
?detectCores
print(total.cores)
cl <- makeCluster(total.cores)
registerDoParallel(cl)

rf.data <- train.set.miss

#impute using random forest imputation, use 500 trees, and cap the number of iterations at 12 (rf.iter)
#try to parallelize the forests using the doParallel package to save time

##############################
#finish the get.resid.err function and calculate the test set errors for each imputed dataset
mean.imp.err <- get.resid.err(mean.data, test.set)
mice.imp.err <- get.resid.err(mice.data, test.set)
rf.imp.err <- get.resid.err(rf.data, test.set)
no.imp.err <- get.resid.err(train.set, test.set)

print(Sys.time() - t.st)

#Repeat the above for every combination of missing proportion, type of missingness and data
#put the results into a 3x5x2x3 array corresponding to the method (mean, mice, rf)x(missing proportion = 0, 0.1, 0.2, 0.3, 0.4)x(random/non-random missingness)x(dataset)
#output.array <- array(0, dim=c(3, 5, 2, 3)) #method, missing prop, missing type, dataset

###########################################
#Generate and save the plots of the results
miss.type.idx <- 1; data.idx <- 1
col.list <- c('black', 'blue', 'darkgreen')

pdf(paste0('error_plot_dataset.pdf'), height=15, width=10)
par(mfrow=c(3, 2))
for (data.idx in 1:3) {
for (miss.type.idx in 1:length(miss.type.list)) {
x.lim=c(0, max(miss.prop.list))
y.lim <- c(0, max(1, max(output.array[,,miss.type.idx,data.idx])))
plot(c(0, miss.prop.list), output.array[1,,miss.type.idx, data.idx], main=paste('Dataset:', data.idx, 'Missingness:', miss.type.list[miss.type.idx]), xlab="% Missing", ylab='Residual Error', pch=NA, ylim=y.lim, xlim=x.lim)
for (i in 1:3) {
	lines(c(0, miss.prop.list), output.array[i,,miss.type.idx, data.idx], lty=1, pch=i, type='b', col=col.list[i])
}
abline(h=1, col='gray')
legend('topleft', legend=c('Mean', 'MICE', 'RF'), pch=1:3, col=col.list, lty=1, bty='n')
}
}
dev.off()




