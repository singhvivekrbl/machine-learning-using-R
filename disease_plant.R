# Input a map of observed disease severity for 100 plants
# Matrix columns represent field crop rows
# so comparisons between matrix columns are 'across-row'
# and comparisons between matrix rows are 'within-row'
row1  <-c (1,4,2,4,1,3,0,3,2,0);
row2  <-c (1,7,3,4,2,5,1,3,3,0);
row3  <-c (2,6,4,3,1,4,2,3,4,1);
row4  <-c (1,5,4,2,1,6,1,2,5,0);
row5  <-c (1,7,4,3,1,3,3,0,6,1);
row6  <-c (0,4,3,4,0,2,5,2,7,2);
row7  <-c (0,2,2,5,2,1,4,1,5,1);
row8  <-c (1,3,1,4,0,2,6,2,6,2);
row9  <-c (1,1,0,2,0,1,3,0,6,1);
row10 <-c (0,1,0,0,0,0,1,0,5,0);
# Create a matrix, called map3, with data input above
map3 <- as.matrix(rbind(row1,row2,row3,row4,row5,
                        row6,row7,row8,row9,row10));
# Make a map of estimated disease severity values
#    based on the omnidirectional model
map4 <- matrix(0,10,10);
for(xrow in 2:9){
  for(xcol in 2:9){
    map4[xrow,xcol] <- 1/4 * (map3[xrow - 1, xcol    ] +
                                map3[xrow + 1, xcol    ] +
                                map3[xrow    , xcol - 1] +
                                map3[xrow    , xcol + 1])
  }
}
map4

# Make a map of estimated disease severity values
#    based on the across-row model
map5 <- matrix(0,10,10)
for(xcol in 2:9){
  for(xrow in 2:9){
    map5[xrow,xcol] <- 1/2 * (map3[xrow,xcol - 1] +
                                map3[xrow,xcol + 1])
  }
}
map5

# Make a map of estimated disease severity values
#    based on the within-row model
map6 <- matrix(0,10,10)
for(xcol in 2:9){
  for(xrow in 2:9){
    map6[xrow,xcol] <- 1/2 * (map3[xrow - 1,xcol] +
                                map3[xrow + 1,xcol])
  }
}
map6

# Make an array of the actual disease severity values
actualo <- c(1:64)
n <- 1
for(xcol in 2:9){
  for(xrow in 2:9){
    actualo[n] <- map3[xrow,xcol];
    n <- n + 1
  }
}
actualo

# Make an array of the estimated disease severity values
#    based on the omni-directional model
omni <- c(1:64)
n <- 1
for(xcol in 2:9){
  for(xrow in 2:9){
    omni[n] <- map4[xrow,xcol];
    n <- n + 1
  }
}
omni

# Make an array of the estimated disease severity values
#   based on the across-row model
across <- c(1:64)
n <- 1
for(xcol in 2:9){
  for(xrow in 2:9){
    across[n] <- map5[xrow,xcol];
    n <- n + 1
  }
}
across
# Make an array of the estimated disease severity values
#    based on the within-row model
within <- c(1:64)
n <- 1
for(xcol in 2:9){
  for(xrow in 2:9){
    within[n] <- map6[xrow,xcol];
    n <- n + 1
  }
}
within

# Run a linear regression comparing the actual disease
# severity values to the omnidirectional estimates
roller <- as.data.frame(cbind(actualo,omni))
roller.lm <- lm(actualo~omni, data=roller);
summary(roller.lm)
# Check residual vs. fitted
plot(roller.lm,which=1)

# Check quantile-quantile plot
plot(roller.lm,which=2)
# Run a linear regression comparing the actual
#    disease severity values to the across-row estimates
roller <- as.data.frame(cbind(actualo,across))
roller.lm <- lm(actualo~across, data=roller)
summary(roller.lm)
# Check residual vs. fitted
plot(roller.lm,which=1)
# Check residual vs. fitted
plot(roller.lm,which=2)
# Run a linear regression comparing the actual disease
#    severity values to the within-row estimates
roller <- as.data.frame(cbind(actualo,within))
roller.lm <- lm(actualo~within, data=roller)
summary(roller.lm)

#Check residual vs. fitted
plot(roller.lm,which=2)
