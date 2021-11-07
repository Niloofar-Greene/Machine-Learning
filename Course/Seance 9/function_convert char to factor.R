library(dplyr)
iris_char <- iris %>%
  mutate(Species=as.character(Species),
         char_column=sample(letters[1:5], nrow(iris), replace=TRUE))


iris_factor <- iris_char %>%
  mutate_if(sapply(iris_char, is.character), as.factor)

sapply(iris_factor, class)