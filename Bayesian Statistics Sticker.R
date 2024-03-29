library(hexSticker)
library(ggplot2)
library(dplyr)
library(bayesrules)

set.seed(1234)

# Bayesian Statistics Sticker
### Create the Graphic
##### Function to calculate posterior
calculate_posterior <- function(prior_mean, prior_sd, likelihood_mean, likelihood_sd) {
  posterior_mean <- (prior_mean / prior_sd^2 + likelihood_mean / likelihood_sd^2) /
    (1 / prior_sd^2 + 1 / likelihood_sd^2)
  posterior_sd <- sqrt(1 / (1 / prior_sd^2 + 1 / likelihood_sd^2))
  return(data.frame(x = seq(posterior_mean - 3 * posterior_sd, posterior_mean + 3 * posterior_sd, length.out = 100),
                    y = dnorm(seq(posterior_mean - 3 * posterior_sd, posterior_mean + 3 * posterior_sd, length.out = 100), 
                              mean = posterior_mean, sd = posterior_sd)))
}

##### Prior distribution
prior_mean <- 10
prior_sd <- 3
prior_data <- data.frame(x = seq(prior_mean - 3 * prior_sd, prior_mean + 3 * prior_sd, length.out = 100),
                         y = dnorm(seq(prior_mean - 3 * prior_sd, prior_mean + 3 * prior_sd, length.out = 100), 
                                   mean = prior_mean, sd = prior_sd))

###### Likelihood distribution
likelihood_mean <- 3
likelihood_sd <- 4
likelihood_data <- data.frame(x = seq(likelihood_mean - 3 * likelihood_sd, likelihood_mean + 3 * likelihood_sd, length.out = 100),
                              y = dnorm(seq(likelihood_mean - 3 * likelihood_sd, likelihood_mean + 3 * likelihood_sd, length.out = 100), 
                                        mean = likelihood_mean, sd = likelihood_sd))

###### Calculate posterior
posterior_data <- calculate_posterior(prior_mean, prior_sd, likelihood_mean, likelihood_sd)

###### Plot
bayesian <- ggplot() +
  geom_line(data = prior_data, aes(x, y, color = "Prior"), size = 1) +
  geom_ribbon(data = prior_data, aes(x, ymin = 0, ymax = y, fill = "Prior"), alpha = 0.6) +
  geom_line(data = likelihood_data, aes(x, y, color = "Likelihood"), size = 1) +
  geom_ribbon(data = likelihood_data, aes(x, ymin = 0, ymax = y, fill = "Likelihood"), alpha = 0.6) +
  geom_line(data = posterior_data, aes(x, y, color = "Posterior"), size = 1) +
  geom_ribbon(data = posterior_data, aes(x, ymin = 0, ymax = y, fill = "Posterior"), alpha = 0.6) +
  scale_fill_manual(values = c("Prior" = "#b93556", "Likelihood" = "#250b4f", "Posterior" = "#6d176e")) +
  scale_color_manual(values = c("Prior" = "#b93556", "Likelihood" = "#250b4f", "Posterior" = "#6d176e")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#dfe7f2"),
        panel.grid = element_blank()
  )

### Create the Sticker
sticker(bayesian, package = "", p_size = 20, s_x = 0.9, s_y = 0.9, 
        s_width = 2.2, s_height = 2.35, h_fill = "#dfe7f2", h_color = "#160b38",
        white_around_sticker = TRUE, dpi = 300, 
        filename = "C:/Users/blookabaugh/Desktop/test.png")