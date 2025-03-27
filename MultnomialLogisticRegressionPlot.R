#These are R packages that help graphically display data
library(ggplot2)
library(dplyr)
library(tidyverse)
#adding in beta values into variables (this is entered into a vector which is how R reads data)
beta_DV1 <- c(intercept = -0.0, predictor_1 = 0.00, predictor_2 = 0.00, predictor_3 = 0.00, predictor_4 = 0.00)
beta_DV2 <- c(intercept = -0.0, predictor_1 = 0.00, predictor_2 = 0.00, predictor_3 = -0.00, predictor_4 = -0.00) #replace with actual beta values
#dependent variables
predictor_values <- seq(min, max, length.out = 100)  #replace with appropriate min and max values, 100 is the spacing between values and how they are read, change based on data
df <- expand.grid(predictor_1 = predictor_values, predictor_2 = mean, predictor_3 = mean, predictor_4 = 1)   #here, I'm pulling out the variable I created above and the means. I am adding it to a data frame which is a collection of vectors. 
#predictor 4 is binary/nominal

df <- df %>%
  mutate(
    lin_DV1 = beta_DV1["intercept"] + beta_DV1["Predictor_1"] * Predictor_1 + beta_DV1["Predictor_2"] * Predictor_2 + beta_DV1["Predictor_3"] * Predictor_3 + beta_DV1["Predictor_4"] * Predictor_4, #add in correct variable names
    lin_DV2 = beta_DV2["intercept"] + beta_DV2["Predictor_1"] * Predictor_1 + beta_DV2["Predictor_2"] * Predictor_2 + beta_DV2["Predictor_3"] * Predictor_3 + beta_DV2["Predictor_4"] * Predictor_4
  )

df <- df %>%
  mutate(
    exp_DV1 = exp(lin_DV1),
    exp_DV2 = exp(lin_DV2),
    sum_exp = 1 + exp_DV1 + exp_DV2,
    prob_DV1 = exp_YA / sum_exp,
    prob_DV2 = exp_SA / sum_exp,
    prob_ref_v = 1 / sum_exp
  )
#ref_v refers to your reference variable in your regression model
#these last two steps relate to the mathematical calculation of probability in regression models. 
#Specifically, I am using the softmax function which is used for multinominal logistic regression.
df_long <- df %>%
  select(predictor_2, prob_DV1, prob_DV2, prob_ref_v) %>%
  pivot_longer(cols = starts_with("prob_"), names_to = "Outcome", values_to = "Probability") 
#this step involves changing the format of my data/vectors so that it can be read by the package I use to plot
#These are the labels for the lines
subtitle_text <- paste0(
  "P(Y) = exp(", 
  round(beta_DV1["intercept"], 2), " + ", round(beta_DV1["predictor_1"], 2), " * predictor_1 + ", 
  round(beta_DV1["predictor_2"], 2), " * predictor_2 + ", round(beta_DV2["predictor_3"], 2), " * predictor_3 + ", 
  round(beta_DV1["predictor_3"], 2), " * predictor_3) / sum(exp)"
)

df_long$Outcome <- factor(df_long$Outcome, levels = c("prob_DV1", "prob_DV2", "prob_ref_v"),
                          labels = c("text", "text", "text"))
#This is my actual plot where I have defined my axes and their labels and my main title. 
#I defined my subtitle using a syntax that writes the equation as text rather than code 
ggplot(df_long, aes(x = Predictor_2, y = Probability, color = Outcome)) +
  geom_line(size = 1.2) +
  labs(
    title = "text",
    subtitle = bquote(
      atop(
        log(frac(P[DV1], P[ref_v])) == -0.0 + 0.00 ~ "*" ~ predictor_1 + 0.00 ~ "*" ~ predictor_2 + 0.00 ~ "*" ~ predictor_3 + 0.00 ~ "*" ~ predictor_4,
        log(frac(P[DV2], P[ref_v])) == -0.0 + 0.00 ~ "*" ~ predictor_1 + 0.00 ~ "*" ~ predictor_2 - 0.00 ~ "*" ~ predictor_3 - 0.00 ~ "*" ~ predictor_4
      )
    ),
    x = "text",
    y = "text"
  ) +
  theme_minimal()






