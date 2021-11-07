library(readr)
data <- read_csv("data_etudiants_socio_rv.csv")

# Sample - replace= FALSE makes sure that no element occurs twice
A=sample(1:nrow(data), 50, replace=FALSE)

sample_student=data[A,]

# set.seed() set the seed of R's random number generator
# seed is a number

set.seed(5)
rnorm(5)


