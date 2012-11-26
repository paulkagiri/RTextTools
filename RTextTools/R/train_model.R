train_model <- function(container, algorithm=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET","MAXENT"), ...) {
        
        # CLEAN UP FROM PREVIOUS MODEL TRAINED
        gc()
        
        # CONDITIONAL TRAINING OF MODEL
        if (algorithm=="SVM") {
			model <- svm(x=container@training_matrix, y=container@training_codes, probability=TRUE, ...)
		} else if (algorithm=="SLDA") {
           model <- slda(container.training_codes ~ ., data=data.frame(as.matrix(container@training_matrix),container@training_codes), ...)
        } else if (algorithm=="BOOSTING") {
            model <- LogitBoost(xlearn=as.matrix(container@training_matrix), ylearn=container@training_codes, ...)
        } else if (algorithm=="BAGGING") {
            model <- bagging(container.training_codes ~ ., data=data.frame(as.matrix(container@training_matrix),container@training_codes), ...)
        } else if (algorithm=="RF") {
            model <- randomForest(x=as.matrix(container@training_matrix), y=container@training_codes, ...)
        } else if (algorithm=="GLMNET") {
			training_matrix <- as(container@training_matrix,"sparseMatrix")
            model <- glmnet(x=training_matrix, y=container@training_codes, family="multinomial", ...)
        } else if (algorithm=="TREE") {
            model <- tree(container.training_codes ~ ., data=data.frame(as.matrix(container@training_matrix),container@training_codes), ...)
        } else if (algorithm=="NNET") {
            model <- nnet(container.training_codes ~ ., data=data.frame(as.matrix(container@training_matrix),container@training_codes), ...)
        } else if (algorithm=="MAXENT") {
			model <- maxent(container@training_matrix,as.vector(container@training_codes), ...)
		} else {
			stop("ERROR: Invalid algorithm specified. Type print_algorithms() for a list of available algorithms.")
		}
		
		# RETURN TRAINED MODEL
		gc() # CLEAN UP AFTER MODEL
		return(model)
}