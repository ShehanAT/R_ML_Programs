# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing Specificity 
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule 
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference=factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference=factor(test_set$sex))

# Draw Plot 
qplot(x, p_hat_bayes_unbiased, geom="line") + 
  geom_hline(yintercept = 0.5, lty=2) + 
  geom_vline(xintercept = 67, lty=2)