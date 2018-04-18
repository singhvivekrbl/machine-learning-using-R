##Read in data
conjoint1 <- read.table("insurance.txt", header = T, sep=" ")

## attribute level
a=c("11%","9%","30000","75000","2years","5years")
df=data.frame(a)

# Run conjoint analysis with regression
conj.result <- lm(rating~commission+premium+service,data=conjoint1)
summary(conj.result)

# load 'conjoint' library
library(conjoint)

# Get utilities of each attributes
caModel(conjoint1[,1],conjoint1[,2:4])
Conjoint(conjoint1[,1],conjoint1[,2:4],df)
# Total utilities based on model (maybe include)
caTotalUtilities(conjoint1[,1],conjoint1[,2:4])

# Attribute importance
caImportance(conjoint1[,1],conjoint1[,2:4])

# Multi level full profile conjoint analysis

# Read in file
