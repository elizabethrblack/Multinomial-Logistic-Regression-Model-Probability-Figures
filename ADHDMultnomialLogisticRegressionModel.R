#These are R packages that help graphically display data
library(ggplot2)
library(dplyr)
library(tidyverse)
#adding in beta values into variables (this is entered into a vector which is how R reads data)
beta_YA <- c(intercept = -6.3, LR = 0.06, SS = 0.06, Age = 0.00, Gender = 0.43)
beta_SA <- c(intercept = -3.2, LR = 0.02, SS = 0.05, Age = -0.02, Gender = -0.45)

ss_values <- seq(19, 69, length.out = 100)  #19 is the min and 69 is the max, 100 is the spacing between values and how they are read
df <- expand.grid(SS = ss_values, LR = 37.43, Age = 22, Gender = 1)   #here, I'm pulling out the variable I created above and adding the means from LR scores and age. 
#Gender is 1 (male) because it was what was available in the SPSS output. I am adding it to a data frame which is a collection of vectors. 


df <- df %>%
  mutate(
    lin_YA = beta_YA["intercept"] + beta_YA["LR"] * LR + beta_YA["SS"] * SS + beta_YA["Age"] * Age + beta_YA["Gender"] * Gender,
    lin_SA = beta_SA["intercept"] + beta_SA["LR"] * LR + beta_SA["SS"] * SS + beta_SA["Age"] * Age + beta_SA["Gender"] * Gender
  )

df <- df %>%
  mutate(
    exp_YA = exp(lin_YA),
    exp_SA = exp(lin_SA),
    sum_exp = 1 + exp_YA + exp_SA,
    prob_YA = exp_YA / sum_exp,
    prob_SA = exp_SA / sum_exp,
    prob_NoA = 1 / sum_exp
  )
#these last two steps relate to the mathematical calculation of probability in regression models. 
#Specifically, I am using the softmax function which is used for multinominal logistic regression.
df_long <- df %>%
  select(SS, prob_YA, prob_SA, prob_NoA) %>%
  pivot_longer(cols = starts_with("prob_"), names_to = "Outcome", values_to = "Probability") 
#this step involves changing the format of my data/vectors so that it can be read by the package I use to plot
#These are the labels for the lines
subtitle_text <- paste0(
  "P(Y) = exp(", 
  round(beta_YA["intercept"], 2), " + ", round(beta_YA["LR"], 2), " * LR + ", 
  round(beta_YA["SS"], 2), " * SS + ", round(beta_YA["Age"], 2), " * Age + ", 
  round(beta_YA["Gender"], 2), " * Gender) / sum(exp)"
)

df_long$Outcome <- factor(df_long$Outcome, levels = c("prob_YA", "prob_SA", "prob_NoA"),
                          labels = c("Diagnosed ADHD", "Suspected ADHD", "Neurotypical"))
#This is my actual plot where I have defined my axes and their labels and my main title. 
#I defined my subtitle using a syntax that writes the equation as text rather than code 
ggplot(df_long, aes(x = SS, y = Probability, color = Outcome)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Diagnostic Group Based on Sensory Sensitivity Scores",
    subtitle = bquote(
      atop(
        log(frac(P[YA], P[No])) == -6.3 + 0.06 ~ "*" ~ LR + 0.06 ~ "*" ~ SS + 0.00 ~ "*" ~ Age + 0.43 ~ "*" ~ Gender,
        log(frac(P[SA], P[No])) == -3.2 + 0.02 ~ "*" ~ LR + 0.05 ~ "*" ~ SS - 0.02 ~ "*" ~ Age - 0.40 ~ "*" ~ Gender
      )
    ),
    x = "Sensory Sensitivity (SS)",
    y = "Probability"
  ) +
  theme_minimal()






