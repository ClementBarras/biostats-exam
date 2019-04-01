library(glmnet)
library(lars)
data(diabetes)
X = as.matrix(diabetes$x)
y = diabetes$y

# regularization path for LASSO
plot(glmnet(X, y, family = "gaussian"))

# Cross validation based on MSE criterion
res.cv = cv.glmnet(X, y ,type.measure = "mse")

#vizualisation
plot(res.cv)

#final model

res.glm = glmnet(X, y, lambda = res.cv$lambda.1se)
coef(res.glm)

yhat = predict(res.glm, newx = X)
plot(yhat, y)



#########
# Ridge #
#########

# regularization path for Ridge
plot(glmnet(X, y, family = "gaussian", alpha = 0))

# Cross validation based on MSE criterion
res.cv = cv.glmnet(X, y ,type.measure = "mse", alpha = 0)

#vizualisation
plot(res.cv)

#final model

res.glm = glmnet(X, y, lambda = res.cv$lambda.1se, alpha = 0)
coef(res.glm)

yhat = predict(res.glm, newx = X)
plot(yhat, y)
