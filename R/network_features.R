network_features <- function(L='label',data_train,data_test,nf,p,corr,f_type,s,nc)
{
  classes <- unique(data_train$label)

  names(data_train)[colnames(data_train)==L] <- paste("label")
  names(data_test)[colnames(data_test)==L] <- paste("label")

  data_trainm <- data_train[,colnames(data_train)!=L]
  data_testm <- data_test[,colnames(data_test)!=L]

  train_label <- data_train$label
  test_label <- data_test$label

  # feature selection
  if(nf>0) {
    nf = round(min(ncol(data_train),nf))

    # rank feature by ttest
    indx <- rankfeature(L,data_train,classes,nf)
    train_label <- data_train[,colnames(data_train)==L]
    data_trainm <- data_trainm[,indx]
    test_label <- data_test[,colnames(data_test)==L]
    data_testm <- data_testm[,indx]
  }

  if(f_type==1){
    newdata <- new_feature_type1()
  }

  if(f_type==2){
    newdata <- new_feature_type2()
  }

  if(f_type==3){
    newdata <- new_feature_type3()
  }

  if(f_type==4){
    newdata <- new_feature_type2()
  }

  # remove na and inf
  new_data <- data.frame(scale(new_data))

  is.na(new_data) <- sapply(new_data, is.infinite)
  is.na(new_data) <- sapply(new_data, is.nan)
  ind_na <- colSums(is.na(new_data))==0

  xx = seq(from=1,to=nrow(new_train),by=1)
  new_train <- new_data[xx,]
  new_test <- new_data[-xx,]


  return(list(new_train = new_train, new_test = new_test, train_label = train_label, test_label = test_label))

}
