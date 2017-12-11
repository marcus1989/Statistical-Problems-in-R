EStep <- function(theta){
  return((125*theta)/(2+theta))
}
MStep <- function(y3){
  return((34+y3)/(72+y3))
}
theta <- 0.5
for(i in 1:10){
  y3 <- EStep(theta)
  theta = MStep(y3)
  sink("C:/Users/VidyaAdmin/Desktop/theta.txt", append = TRUE)
  print(theta)
  sink()
}
theta_vals <- read.table("C:/Users/VidyaAdmin/Desktop/theta.txt")
theta_vals <- append(theta_vals[,2], 0.5, after = 0)
plot(0:10, theta_vals, type = 'o', main = 'Plot of theta', xlab = 'Iteration', ylab = 'theta')

