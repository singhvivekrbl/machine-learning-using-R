# Preliminaries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

# Multiplot, from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

  # Simulate data
  set.seed(0)

T <- 90
n_per_day <- 10000
n <- T * n_per_day
day_index <- sort(rep(seq(1,T), n_per_day))

mean_utility <- 0.5
price_coefficient <- -0.5
utility_outside <- 0

high_price = 0.8
low_price = 0.4

df <- data.frame(day_index = day_index,
                 simulatedErrorProduct = rnorm(n = n)) %>%
  mutate(prices = c(rep(low_price, n / 2), rep(high_price, n / 2)),
         utility_product = mean_utility + simulatedErrorProduct + price_coefficient * prices,
         utility_outside = utility_outside,
         purchase_binary = (utility_product > utility_outside))

true_elasticity <- (
  pnorm(mean_utility + price_coefficient * high_price) - 
    pnorm(mean_utility + price_coefficient * low_price)
) / (high_price - low_price) * mean(df$prices) / mean(df$purchase_binary)
write.csv(df, file = "price_optimisation.csv")
#Model 1: Observe Purchase Binary, OLS Model

  ols <- df %>% 
  lm(purchase_binary ~ prices,
     data = .) %>%
  summary()

ols

ols_elasticity <- ols$coefficients[2] * mean(df$prices) / mean(df$purchase_binary)


#Model 2. Observe Purchase Binary, Logit Model

  logit <- df %>%
  glm(formula = purchase_binary ~ prices,
      family = binomial(link = 'logit'),
      data = .) %>%
  summary()

logit

logit_elasticity <- logit$coefficients[2] * (1 - mean(df$purchase_binary)) * mean(df$prices)

#Model 3: Observe Total Quantity Purchased, OLS Model

  df_by_day <-  df %>% 
  group_by(day_index) %>%
  summarise(quantity = sum(purchase_binary),
            prices = max(prices))

quantity_model <- df_by_day %>% 
  lm(quantity ~ prices,
     data = .) %>%
  summary()

quantity_model

quantity_elasticity <- quantity_model$coefficients[2] * mean(df_by_day$prices) / mean(df_by_day$quantity)

#Comparing Elasticities

  data.frame(true = true_elasticity, 
             ols_binary = ols_elasticity,
             logit_binary = logit_elasticity,
             ols_quantity = quantity_elasticity)
  
#  Comparing Estimated Demand Curves, and the Implied Profit Functions & Optimal Prices

    demand_curves <- data.frame(prices = seq(from = 0, to = 3, by = 0.01))
  
  demand_curves %<>%
    mutate(ols = n *
             (ols$coefficients[1] + ols$coefficients[2] * prices),
           logit = n *
             (exp(logit$coefficients[1] + logit$coefficients[2] * prices) / 
                (1 + exp(logit$coefficients[1] + logit$coefficients[2] * prices)))) %>%
    gather(model, quantities, -prices)
  
  
  demand_plot <- ggplot(data = demand_curves, aes(x = quantities, y = prices, color = model)) + 
    geom_line() + 
    labs(x = 'Quantity', y = 'Price ($)', title = 'Panel A: Estimated Demand Curves') + 
    geom_hline(yintercept = c(low_price, high_price), colour = "#2A73CC", linetype = "longdash") + 
    theme(plot.title = element_text(face = "bold"))
  
  profit_curves <- demand_curves %>%
    mutate(profit = quantities * prices)
  
  profit_curves %>%
    group_by(model) %>%
    filter(profit == max(profit))
  
  profit_plot <- ggplot(data = profit_curves, aes(x = prices, y = profit, color = model)) + 
    geom_line() + 
    labs(x = 'Price ($)', y = 'Profit ($)', title = 'Panel B: Implied Profit Functions') + 
    geom_vline(xintercept = c(low_price, high_price), colour = "#2A73CC", linetype = "longdash") + 
    theme(plot.title = element_text(face = "bold"))
  
  multiplot(demand_plot, profit_plot, cols = 2)
  
  options(repr.plot.width = 10, repr.plot.height = 6)
  
##  Price Changes Perfectly Co-Timed with Quality Changes

    product_changes <- 10
  mean_utility <- sort(rep(seq(from = 0.1, to = 0.9, length.out = product_changes), n / product_changes))
  
  prices <- 1.9 * mean_utility
  
  df <- data.frame(day_index = day_index,
                   simulatedErrorProduct = rnorm(n = n)) %>%
    mutate(prices = prices,
           mean_utility = mean_utility,
           utility_product = mean_utility + simulatedErrorProduct + price_coefficient * prices,
           utility_outside = utility_outside,
           purchase_binary = (utility_product > utility_outside)) %>%
    group_by(day_index) %>%
    summarise(quantity = sum(purchase_binary),
              prices = max(prices),
              quality = max(mean_utility))
  
  # upward sloping curve
  df %>%
    lm(formula = quantity ~ prices) %>%
    summary()
  
  # cannot separate changes in price from changes in quality
  df %>%
    lm(formula = quantity ~ factor(quality) + prices) %>%
    summary()
  
  df %>%
    ggplot(aes(x = quantity, y = prices)) +   
    geom_point(color = "black") +
    geom_smooth(method = lm,   
                se = FALSE, 
                color = '#2A73CC') +
    labs(x = 'Quantity', y = 'Price ($)') 
  
  options(repr.plot.width = 6, repr.plot.height = 6)
  
  ##Price Changes Imperfectly Co-Timed with Quality Changes
    price_changes <- 90
  prices <- sort(rep(seq(from = 0.4, to = 1.8, length.out = price_changes), n / price_changes))
  
  df <- data.frame(day_index = day_index,
                   simulatedErrorProduct = rnorm(n = n)) %>%
    mutate(prices = prices,
           mean_utility = mean_utility,
           utility_product = mean_utility + simulatedErrorProduct + price_coefficient * prices,
           utility_outside = utility_outside,
           purchase_binary = (utility_product > utility_outside)) %>%
    group_by(day_index) %>%
    summarise(quantity = sum(purchase_binary),
              prices = max(prices),
              quality = max(mean_utility))
  
  # first show uncontrolled
  df %>%
    lm(formula = quantity ~ prices) %>%
    summary()
  
  # can recover price coefficient if control for quality
  df %>%
    lm(formula = quantity ~ quality + prices) %>%
    summary()
  
  quantity_fit <- df %>%
    lm(formula = quantity ~ quality)
  
  prices_fit <- df %>%
    lm(formula = prices ~ quality)
  
  
  basic_plot <- df %>%
    ggplot(aes(x = quantity, y = prices)) +   
    geom_point(color = "black") +
    geom_smooth(method = lm,   
                se = FALSE, 
                color = '#2A73CC') +
    labs(x = 'Quantity', y = 'Price ($)', title = 'Panel A: Raw Data') + 
    theme(plot.title = element_text(face = "bold"))
  
  
  resid_plot <- df %>%
    ggplot(aes(x = quantity_fit$residuals, y = prices_fit$residuals)) +   
    geom_point(color = "black") +
    geom_smooth(method = lm,   
                se = FALSE, 
                color = '#2A73CC') +
    labs(x = 'Residual Quantity', y = 'Residual Price ($)', title = 'Panel B: Conditioning on Quality') + 
    theme(plot.title = element_text(face = "bold"))
  
  multiplot(basic_plot, resid_plot, cols = 2)
  
  options(repr.plot.width = 10, repr.plot.height = 6)
  
  