# Load necessary libraries
library(e1071)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(ggsignif)   

# Load results file
results <- read.csv("~/Downloads/final.csv", row.names=1)
results <- na.omit(results)
results$subject <- factor(results$subject)
results$stimulus_len <- factor(results$stimulus_len)

# Remove outliers
out <- is_extreme(results$alpha_power)
results$out <- out
results <- results[results$out == FALSE, ]

# Check for normality
shapiro.test(results$alpha_power)
hist(results$alpha_power, breaks = 50, xlim = c(0, 0.0000000058), main="Histogram after removal of outliers", xlab="Alpha Power (V)") #KEEP THIS ONE
skewness(results$alpha_power, na.rm=TRUE)
p <- ggqqplot(results$alpha_power)
ggpar(p, ylim=c(0, 0.000000005)) 

# Log transformation
results$log_alpha=log(results$alpha_power)
shapiro.test(results$log_alpha)
hist(results$log_alpha, breaks = 20, main="Histogram after outlier removal and log transformation", xlab="Log(Alpha Power)")
skewness(results$log_alpha, na.rm=TRUE)
ggqqplot(results$log_alpha)

# ANOVA Tests
anov1 <- anova_test( data = results, log_alpha ~ task + condition + hemisphere + region +
                     task*condition + task*hemisphere + task*region +
                     condition*hemisphere +hemisphere*condition+ condition*region + region*hemisphere, wid = subject,
                     within = c(task, condition))
get_anova_table(anov1)

# also test for stimulus length instead of condition
anov1 <- anova_test( data = results, log_alpha ~ task + stimulus_len + hemisphere + region +
                       task*stimulus_len + task*hemisphere + task*region +
                       stimulus_len*hemisphere + stimulus_len*region + region*hemisphere, wid = subject,
                     within = c(task, stimulus_len, hemisphere, region))
get_anova_table(anov1)

# Post hoc tests for significant variables (hemisphere and region)
aov <- aov(data=results, log_alpha ~ hemisphere + region)
TukeyHSD(aov)

# Plot results for region variable
results$region <- factor(results$region , levels=c("frontal", "central", "posterior"))
ggp_box <- ggplot(results, aes(x = region, y = alpha_power)) + 
  geom_boxplot() + 
  geom_signif(comparisons = list(c("frontal", "central"), 
                                 c("central", "posterior"), 
                                 c("frontal", "posterior")), 
              y_position = c(7.5e-09, 8.25e-09, 9e-09),
              col="red",
              map_signif_level = TRUE) + 
  xlab("Brain Region") +
  ylab("Alpha Power (μV^2)") +
  scale_x_discrete(labels = c("Frontal", "Central", "Posterior"))
ggp_box


