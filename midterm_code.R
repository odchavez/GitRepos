library(Matrix)
#4a)
Y   = c(82, 79, 74, 83, 80, 81, 84, 81) 
xi1 = c(10, 9, 9, 11, 11, 10, 10, 12)
xi2 = c(15, 14, 13, 15, 14, 14, 16, 13)

X = matrix(c(rep(1,8),xi1,xi2), ncol = 3)

Beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y
Beta_hat

H = X%*%solve(t(X)%*%X)%*%t(X)

sigma_hat = t(Y)%*%(diag(8) - H)%*%Y/(n-rankMatrix(H))
sigma_hat

#4b)
lamda = matrix(c(0,1,0), ncol = 1)
XtXinv = ginv(t(X)%*%X)

t(lamda)%*%Beta_hat
qt(0.025, 5)
t(lamda)%*%XtXinv%*%lamda

t(lamda)%*%Beta_hat - qt(0.975, 5) * sqrt(sigma_hat*t(lamda)%*%XtXinv%*%lamda)
t(lamda)%*%Beta_hat + qt(0.975, 5) * sqrt(sigma_hat*t(lamda)%*%XtXinv%*%lamda)

lamda = matrix(c(0,1,1), ncol = 1) 
t(lamda)%*%Beta_hat - qt(0.975, 5) * sqrt(sigma_hat*t(lamda)%*%XtXinv%*%lamda)
t(lamda)%*%Beta_hat + qt(0.975, 5) * sqrt(sigma_hat*t(lamda)%*%XtXinv%*%lamda)

#4c)
lamda = matrix(c(0,0,1), ncol = 1) 
(t(lamda)%*%Beta_hat - 3)/sqrt(sigma_hat*t(lamda)%*%XtXinv%*%lamda)


#4d)
lamda = matrix(c(0,1,-1), ncol = 1) 
t = (t(lamda)%*%Beta_hat)/sqrt(sigma_hat*t(lamda)%*%XtXinv%*%lamda)
t
pt(t, df = 5)

