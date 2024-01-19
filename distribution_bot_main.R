## distribution_bot.R

#### set up #### 

# libraries
library(rtoot)
library(ggplot2)



# Bot account credentials
token <- # insert your token here



#### plotting function ####


generate_and_plot_distribution <- function() {

  
  # List of distribution types
  distribution_list <- c('Normal Distribution','Uniform Distribution', 'Poisson Distribution', 'Binomial Distribution', 'Beta Distribution',
                         'Chi-Squared Distribution', 'Exponential Distribution', 'Gamma Distribution', 'Logistic Distribution',
                         'Student t Distribution', 'Geometric Distribution', 'Hypergeometric Distribution', 'Weibull Distribution')
  
  # Randomly select a distribution type
  selected_distribution <- sample(distribution_list, 1)
  
  # Set a random sample size
  sample_size <- sample(10:100, 1)
  
  # Generate random data based on the selected distribution
  random_data <- switch(selected_distribution,
                        'Normal Distribution'= rnorm(sample_size, mean = 0, sd = 1),
                        'Uniform Distribution' = runif(sample_size),
                        'Poisson Distribution' = rpois(sample_size, lambda = 5),
                        'Binomial Distribution' = rbinom(sample_size, size = 10, prob = 0.5),
                        'Beta Distribution' = rbeta(sample_size, shape1 = 2, shape2 = 5),
                        'Chi-Squared Distribution' = rchisq(sample_size, df = 3),
                        'Exponential Distribution' = rexp(sample_size, rate = 0.2),
                        'Gamma Distribution' = rgamma(sample_size, shape = 2, rate = 0.5),
                        'Logistic Distribution' = rlogis(sample_size, location = 0, scale = 1),
                        'Student t Distribution' = rt(sample_size, df = 3),
                        'Geometric Distribution' = rgeom(sample_size, prob = 0.2),
                        'Hypergeometric Distribution' = rhyper(sample_size, m = 10, n = 20, k = 5),
                        'Distribution of the Wilcoxon Rank Sum' = wilcox.test(rnorm(sample_size))$statistic,
                        'Weibull Distribution' = rweibull(sample_size, shape = 2, scale = 1))
  
  # Plot the generated data
  g1<-ggplot(data = data.frame(random_data), aes(x = random_data))+
    geom_histogram(aes(y=after_stat(density)), fill = "white", color = 'black')+
    geom_density(fill = "#9ACCCD", color = "#006666", alpha = 0.3)+
    xlab("Value")+
    ylab("Probability Density")+
    ggtitle(paste(selected_distribution), paste0("(", sample_size, " samples)", collapse = ""))
  
  
  # set image path
  image_path <- tempfile(fileext = ".png")
  ggplot2::ggsave(image_path, g1, width = 8, height = 6, dpi = 300)
  
  
  
  # Post the plot to Mastodon
  post_toot(
    status = paste("Today's probability distribution is the...", selected_distribution), "/n #rstats",
    media = image_path,
    visibility = "public",
    alt_text = selected_distribution, 
    token = token)
  
  
  # Clean up the temporary image file
  unlink(image_path)
  
}


# Call the function to generate and plot a random distribution
generate_and_plot_distribution()





