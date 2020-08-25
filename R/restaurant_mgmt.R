#Q. A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.


# Q1. The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
# 
# - Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
# 
# - Use sapply() to apply the function to entree option counts ranging from 1 to 12.
# 
# What is the minimum number of entree options required in order to generate more than 365 combinations?
# when changing entree_choices
meal_comb <- function(entree_choices){
  entree_combn <- choose(entree_choices,1)
  sides_combn <- choose(6,2)
  drinks_combn<- choose(3,1)
  entree_combn * sides_combn * drinks_combn
}
entree_choices <- seq(2,12)
meal_combns <- sapply(entree_choices, meal_comb)
meal_combns

#Q2. The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
# 
# - Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
# 
# - Use sapply() to apply the function to side counts ranging from 2 to 12.
# 
# What is the minimum number of side options required in order to generate more than 365 combinations?
# when changing side_choices
meal_comb <- function(side_choices){
  entree_combn <- choose(6,1)
  sides_combn <- choose(side_choices,2)
  drinks_combn<- choose(3,1)
  entree_combn * sides_combn * drinks_combn
}
side_choices <- seq(2,12)
meal_combns <- sapply(side_choices, meal_comb)
meal_combns


