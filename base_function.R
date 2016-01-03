
# Function should fulfill the following requirements
# {attempt to limit to approx 5 parameters}
# 1. Moments
# 2. Basic Summary Statistics [mean, median, mode, IQR]
# 3. outlier info, with ability to remove normal and extreme outliers, if desired
# 4. distribution evaluation >> kernel pdf, empirical cdf, empirical quantile function, histogram
# 5. ability to perform basic hypothesis test

x <- c(10, 15, 23, 18)

require("moments") # for skewness and kurtosis
install.packages("BSDA"); require("BSDA")
install.packages("stargazer"); library("stargazer")

auto_eda <- function(x) # x should just be a vector with elements corresponding to one observed variable
{
    
  # MOMENT INFO
  
  n <- length(x)
  mu <- mean(x)
  std_dev <- sd(x)
  variance <- var(x)
  skew <- skewness(x)
  kurt <- kurtosis(x)
  
  moments <- data.frame(n, mu, std_dev, variance, skew, kurt)
  names(moments) <- c("N", "Mean", "Standard Deviation", "Variance", "Skewness", "Kurtosis")
  
  
  
  # BASIC STATISTICAL MEASURES
  loc_mean <- round(mean(x), 3)
  loc_median <- round(median(x), 3)
  ux <- unique(x)
  loc_mode <- ux[which.max(tabulate(match(z,ux)))]
  range <- max(x)-min(x)
  IQR <- quantile(x, probs=0.75) - quantile(x, probs=0.25)
  
  stat_measures <- data.frame(loc_mean, loc_median, loc_mode, std_dev, variance, range, IQR)
  names(stat_measures) <- c("Mean", "Median", "Mode", "Std Deviation", "Variance", "Range", "Interquartile Range")
  # TEST FOR LOCATION >> T-TEST, SIGN TEST, WILCOXON SIGNED RANK TEST
  
  # Test for location where mu=0
  T_TEST <- t.test(x, alternative = "two.sided", mu=0, conf.level=0.95)
  #binom.test(length(x[which(x  0)]) , length(x)) 
  #wilcox.test(x, alternative = "two.sided", mu = 0, conf.level = 0.95)

  
  # DESCRIPTION OF QUANTILES
  quants <- data.frame(rbind((quantile(x, probs = c(1.0, 0.99, 0.95, 0.90
                                ,0.75, 0.5, 0.25, 0.1, 0.5, 0.01, 0)))))
  names(quants) <- c("100% Max", "99%", "95%", "90%", "75% Q3", "50% Median"
                    , "25% Q1", "10%", "5%", "1%", "0% Min")
  
  
  
  # DESCRIPTION OF OUTLIERS
  outlier_norm <-
    
  
  # BEGIN TO OUTPUT THINGS
  stargazer(moments, summary=FALSE, type="text")
  stargazer(stat_measures, summary=FALSE, type="text")
  stargazer(quants, summary=FALSE, type="text")
    
}


