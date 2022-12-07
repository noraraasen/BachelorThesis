library(ggplot2)
library(copula)
library(lattice)
# uniform distribution
set.seed(241)
# kumulativ fordeling
x_lower <- -1
x_upper <- 2
max_height <- max(dexp(x_lower:x_upper, rate = 1, log = FALSE))

ggplot(data.frame(x = c(x_lower,x_upper)), aes(x = x)) + xlim(x_lower, x_upper) + 
  ylim(0, max_height) +
  stat_function(fun = punif, geom = "area", fill = "blue", alpha = 0.25) + 
  stat_function(fun = punif) + 
  labs(x = "x", y = expression('P(X '<=' x)'), title = "Standard uniform distribution function") + 
  theme_minimal()

# tilfeldig variabler
x = seq(0,2000,1)
y_r = runif(x)
ggplot(data = NULL, aes(x = y_r)) + 
  geom_histogram(binwidth = 0.05, boundary = 1)+
  theme_minimal() +
  xlab('P(X=x)') +
  ylab(NULL)

# Product, W and M copula (wireframe and contour plots)
set.seed(2809)
U <- runif(100)
M_U <- as.data.frame(cbind("U_1" = U, "U_2" = U))
ggplot(data = M_U, aes(x = U_1, y = U_2)) + 
  theme_minimal(base_size = 16) +
  xlab(expression(u[1])) +
  ylab(expression(u[2])) +
  geom_point()

W_U <- as.data.frame(cbind("U_1" = U, "U_2" = 1-U))
ggplot(data = W_U, aes(x = U_1, y = U_2)) + 
  theme_minimal(base_size = 16) +
  xlab(expression(u[1])) +
  ylab(expression(u[2])) +
  geom_point()

plot(cbind(U,1-U),xlab = quote(U[1]), ylab = quote(U[2]))
plot(cbind(U,U),xlab = quote(U[1]), ylab = quote(U[2]))
u <- seq(0, 1, length.out = 40) # subdivision points in each dimension
u12 <- expand.grid("u[1]" = u, "u[2]" = u) # build a grid
W <- pmax(u12[,1] + u12[,2] - 1, 0) # values of W on grid
M <- pmin(u12[,1], u12[,2]) # values of M on grid
prod <- u12[,1]*u12[,2]
val.W <- cbind(u12, "W(u[1],u[2])" = W) # append grid
val.M <- cbind(u12, "M(u[1],u[2])" = M) # append grid
val.prod <- cbind(u12, "u[1]*u[2]" = prod)

wireframe2(val.W, scales = list(arrows=FALSE,cex=1,tick.number = 5, z = list(arrows=FALSE, cex = 1, tick.number = 1)),
           xlab=expression(u[1]),
           ylab=expression(u[2]),zlab=expression(W(u[1],u[2])),
           light.source = c(10,10,10), drape=T, 
           col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1))
wireframe2(val.M,scales = list(arrows=FALSE,cex=1,tick.number = 5, z = list(arrows=FALSE, cex = 1, tick.number = 1)),
           xlab=expression(u[1]),
           ylab=expression(u[2]),zlab=expression(M(u[1],u[2])), light.source = c(10,10,10),drape=T, 
           col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1))
wireframe2(val.prod,scales = list(arrows=FALSE,cex=1,tick.number = 5, z = list(arrows=FALSE, cex = 1, tick.number = 1)),
           xlab=expression(u[1]),
           ylab=expression(u[2]),zlab=expression(Pi(u[1],u[2])), 
           light.source = c(10,10,10), drape=T,
           col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1))
contourplot2(val.W, xlim = 0:1, ylim = 0:1, 
             scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)), 
             xlab = list(expression(u[1]), cex = 1.5), 
             ylab = list(expression(u[2]), cex = 1.5))
contourplot2(val.M, xlim = 0:1, ylim = 0:1, 
             scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)), 
             xlab = list(expression(u[1]), cex = 1.5), 
             ylab = list(expression(u[2]), cex = 1.5))
contourplot2(val.prod, xlim = 0:1, ylim = 0:1, 
             scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)), 
             xlab = list(expression(u[1]), cex = 1.5), 
             ylab = list(expression(u[2]), cex = 1.5))
