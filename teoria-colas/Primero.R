#Sacado de queueing pag3
# Load the package
library(queueing)
# Create the inputs for the model.
i_mm1 <- NewInput.MM1(lambda=2, mu=3) 
# Optionally check the inputs of the model
CheckInput(i_mm1) #si cambias a lambda=4 y mu=3 te da fatal error
# Create the model
o_mm1 <- QueueingModel(i_mm1)  
# Print on the screen a summary of the model
print(summary(o_mm1), digits=2) 
#Tenemos que saber interpretar cada uno (lambda, mu, c, k ,m, RO, P0...)

fw <- o_mm1$FW
fwq <- o_mm1$FWq
n <- 10
ty <- "l"
ylab <- "FW(t), FWq(t)"
xlab <- "t"
cols <- c("black", "red")
leg <- c("FW(t)", "FWq(t)")
curve(fw, from=0, to=n, type=ty, ylab=ylab, xlab=xlab, col=cols[1], main=gTitle)
curve(fwq, from=0, to=n, type=ty, col=cols[2], add=T)
legend("bottomright", leg, lty=c(1, 1), col=cols)

#pito