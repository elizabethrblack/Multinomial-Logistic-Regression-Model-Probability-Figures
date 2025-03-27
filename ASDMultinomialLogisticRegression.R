library(ggplot2)
library(dplyr)
library(tidyverse) #These are R packages that help graphically display data
#adding in beta values into variables (this is entered into a vector which is how R reads data)
beta_YASD <- c(intercept = -10.08, LR = 0.04, SS = 0.11, Age = 0.01, Gender = 1.32)
beta_SASD <- c(intercept = -6.29, LR = 0.03, SS = 0.08, Age = -0.03, Gender = 0.17)

ss_values <- seq(19, 69, length.out = 100) #19 is the min and 69 is the max, 100 is the spacing between values and how they are read
df <- expand.grid(SS = ss_values, LR = 37.43, Age = 22, Gender = 1)  #here, I'm pulling out the variable I created above and adding the means from LR scores and age. Gender is 1 (male) because it was what was available in the SPSS output. I am adding it to a data frame which is a collection of vectors. 

df <- df %>%
  mutate(
    lin_YASD = beta_YASD["intercept"] + beta_YA["LR"] * LR + beta_YA["SS"] * SS + beta_YA["Age"] * Age + beta_YA["Gender"] * Gender,
    lin_SASD = beta_SASD["intercept"] + beta_SA["LR"] * LR + beta_SA["SS"] * SS + beta_SA["Age"] * Age + beta_SA["Gender"] * Gender
  )

df <- df %>%
  mutate(
    exp_YASD = exp(lin_YASD),
    exp_SASD = exp(lin_SASD),
    sum_exp = 1 + exp_YASD + exp_SASD,
    prob_YASD = exp_YASD / sum_exp,
    prob_SASD = exp_SASD / sum_exp,
    prob_NoASD = 1 / sum_exp
  )
#these last two steps relate to the mathematical calculation of probability in regression models. Specifically, I am using the softmax function which is used for multinominal logistic regression.
df_long <- df %>%
  select(SS, prob_YASD, prob_SASD, prob_NoASD) %>%
  pivot_longer(cols = starts_with("prob_"), names_to = "Outcome", values_to = "Probability")

#this step involves changing the format of my data/vectors so that it can be read by the package I use to plot
subtitle_text <- paste0(
  "P(Y) = exp(", 
  round(beta_YASD["intercept"], 2), " + ", round(beta_YASD["LR"], 2), " * LR + ", 
  round(beta_YASD["SS"], 2), " * SS + ", round(beta_YASD["Age"], 2), " * Age + ", 
  round(beta_YASD["Gender"], 2), " * Gender) / sum(exp)"
)
#These are the labels for the lines
df_long$Outcome <- factor(df_long$Outcome, levels = c("prob_YASD", "prob_SASD", "prob_NoASD"),
                          labels = c("Diagnosed ASD", "Suspected ASD", "Neurotypical"))

#This is my actual plot where I have defined my axes and their labels and my main title. I defined my subtitle using a syntax that writes the equation as text rather than code 
ggplot(df_long, aes(x = SS, y = Probability, color = Outcome)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Diagnostic Group Based on Sensory Sensitivity Scores",
    subtitle = bquote(
      atop(
        log(frac(P[YASD], P[No])) == -10.08 + 0.04 ~ "*" ~ LR + 0.11 ~ "*" ~ SS + 0.01 ~ "*" ~ Age + 1.32 ~ "*" ~ Gender,
        log(frac(P[SASD], P[No])) == -6.23 + 0.03 ~ "*" ~ LR + 0.08 ~ "*" ~ SS - .03 ~ "*" ~ Age + 0.17 ~ "*" ~ Gender
      )
    ),
    x = "Sensory Sensitivity",
    y = "Probability"
  ) +
  theme_minimal()






