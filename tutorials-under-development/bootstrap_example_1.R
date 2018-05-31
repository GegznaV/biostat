# http://people.tamu.edu/~alawing/materials/ESSM689/Btutorial.pdf


# https://www.statmethods.net/advstats/bootstrapping.html

library(boot)
head(trees)

plot(Volume ~ Height,  data = trees,
     main = 'Black Cherry Tree Volume Relationship',
     xlab = 'Height',
     ylab = 'Volume',
     pch  = 16,
     col  = 'blue'
)


plot(Volume ~ Height, data = trees,
     main = 'Black Cherry Tree Volume Relationship',
     xlab = 'Girth',
     ylab = 'Volume',
     pch  = 16,
     col  = 'blue')

# foo = function(parameter1, parameter2, ... parametern) {
#     bar = * do something to data passed as parameters * return(bar)
# }

sample_mean <- function(data, indices) {
    sample = data[indices,]
    bar    = mean(sample)
    return(bar)
}

# Calculate the mean of the bootstrap sample

# Creates the bootstrap sample (i.e., subset the provided data by the “indices” parameter).  “indices” is automatically provided by the “boot” function; this is the sampling with replacement portion of bootstrapping

volume_estimate = function(data, indices)
{

        d = data[indices,]
        H_relationship = lm(d$Volume ~ d$Height, data = d)
        H_r_sq = summary(H_relationship)$r.square

        G_relationship = lm(d$Volume ~ d$Girth, data = d)
        G_r_sq = summary(G_relationship)$r.square

        G_H_ratio = d$Girth / d$Height
        G_H_relationship = lm(d$Volume ~ G_H_ratio, data = d)

        G_H_r_sq = summary(G_H_relationship)$r.square
        combined_relationship = lm(d$Volume ~ d$Height + d$Girth, data = d)

        combined_r_sq = summary(combined_relationship)$r.square
        combined_2_relationship = lm(d$Volume ~ d$Height + d$Girth + G_H_ratio, data = d)

        combined_2_r_sq = summary(combined_2_relationship)$r.square
        relationships = c(H_r_sq, G_r_sq, G_H_r_sq, combined_r_sq, combined_2_r_sq)

        return(relationships)
}

# Conduct the bootstrapping–Use “boot” function:
results = boot(data = trees, statistic = volume_estimate, R = 5000)

print(results)

plot(results, index = 1)
# relationships = c(H_r_sq, G_r_sq, G_H_r_sq, combined_r_sq, combined_2_r_sq)

confidence_interval_H = boot.ci(results, index = 1, conf = 0.95, type = "bca")
print(confidence_interval_H)

ci_H = confidence_interval_H$bca[ , c(4, 5)]
print(ci_H)


hist(results$t[,1],
     main = 'Coefficient of Determination: Height',
     xlab = 'R-Squared',
     col = 'grey')

hist(results$t[,1],
     main = 'Coefficient of Determination: Height',
     xlab = 'R-Squared',
     col = 'grey',
     prob = T)
lines(density(results$t[,1]), col = 'blue')

abline(v = ci_H, col = 'red')


# Tutorial ====================================================================
# http://people.tamu.edu/~alawing/materials/ESSM689/Btutorial.R

# Load the "boot" library, which contains the bootstrap functionality.
library(boot)

# Investigate the 'trees' dataset.  Interested in what is a better estimator of
# tree volume: height or girth.
head(trees)

# View scatterplot of girth vs. volume and height vs. volume
plot(
    trees$Volume ~ trees$Height,
    main = 'Black Cherry Tree Volume Relationship',
    xlab = 'Height',
    ylab = 'Volume',
    pch = 16,
    col = 'blue'
)
plot(
    trees$Volume ~ trees$Girth,
    main = 'Black Cherry Tree Volume Relationship',
    xlab = 'Girth',
    ylab = 'Volume',
    pch = 16,
    col = 'blue'
)

# Create function that will be applied to each bootstrap sample
# Evalutate a series of linear regressions to see which variable combinations
# result in the highest coefficients of determination to model Black Cherry tree volume.
volume_estimate = function(data, indices) {
    d = data[indices,]
    H_relationship = lm(d$Volume ~ d$Height, data = d)
    H_r_sq = summary(H_relationship)$r.square
    G_relationship = lm(d$Volume ~ d$Girth, data = d)
    G_r_sq = summary(G_relationship)$r.square
    G_H_ratio = d$Girth / d$Height
    G_H_relationship = lm(d$Volume ~ G_H_ratio, data = d)
    G_H_r_sq = summary(G_H_relationship)$r.square
    combined_relationship = lm(d$Volume ~ d$Height + d$Girth, data = d)
    combined_r_sq = summary(combined_relationship)$r.square
    combined_2_relationship = lm(d$Volume ~ d$Height + d$Girth + G_H_ratio, data = d)
    combined_2_r_sq = summary(combined_2_relationship)$r.square
    #try removing height from combined_2 to show that although height on its own is not very predictive,
    #it adds value to the combined_2 model
    relationships = c(H_r_sq, G_r_sq, G_H_r_sq, combined_r_sq, combined_2_r_sq)
    return(relationships)
}

# Execute the bootstrap function
results = boot(data = trees,
               statistic = volume_estimate,
               R = 5000)

# View the results of the bootstrap
print(results)

#Plot the results of the bootstrap
# Height
plot(results, index = 1)
# Girth
plot(results, index = 2)
# Girth / Height
plot(results, index = 3)
# Girth and Height
plot(results, index = 4)
# Girth, Height, and Girth / Height
plot(results, index = 5)

# Calculate and view the 95% confidence interval using the 'Bias corrected and accelerated method'
# Note that this is calculated for each item (i.e., each linear model) in the bootstrap object.

# Height
confidence_interval_H = boot.ci(results,
                                index = 1,
                                conf = 0.95,
                                type = 'bca')
print(confidence_interval_H)
ci_H = confidence_interval_H$bca[, c(4, 5)]
print(ci_H)

# Girth
confidence_interval_G = boot.ci(results,
                                index = 2,
                                conf = 0.95,
                                type = 'bca')
print(confidence_interval_G)
ci_G = confidence_interval_G$bca[, c(4, 5)]
print(ci_G)

# Girth / Height
confidence_interval_GH = boot.ci(results,
                                 index = 3,
                                 conf = 0.95,
                                 type = 'bca')
print(confidence_interval_GH)
ci_GH = confidence_interval_GH$bca[, c(4, 5)]
print(ci_GH)

# Girth and Height
confidence_interval_com = boot.ci(results,
                                  index = 4,
                                  conf = 0.95,
                                  type = 'bca')
print(confidence_interval_com)
ci_com = confidence_interval_com$bca[, c(4, 5)]
print(ci_com)

# Girth, Height, and Girth / Height
confidence_interval_com2 = boot.ci(results,
                                   index = 5,
                                   conf = 0.95,
                                   type = 'bca')
print(confidence_interval_com2)
ci_com2 = confidence_interval_com2$bca[, c(4, 5)]
print(ci_com2)

# Visualize R-Squared values for the various linear models
# View kernel density estimates and 95% confidence intervals

hist(results$t[, 1],
     main = 'Coefficient of Determination: Height',
     xlab = 'R-Squared',
     col = 'grey')
hist(
    results$t[, 1],
    main = 'Coefficient of Determination: Height',
    xlab = 'R-Squared',
    col = 'grey',
    prob = T
)
lines(density(results$t[, 1]), col = 'blue')
abline(v = ci_H, col = 'red')

hist(results$t[, 2],
     main = 'Coefficient of Determination: Girth',
     xlab = 'R-Squared',
     col = 'grey')
hist(
    results$t[, 2],
    main = 'Coefficient of Determination: Girth',
    xlab = 'R-Squared',
    col = 'grey',
    prob = T
)
lines(density(results$t[, 2]), col = 'blue')
abline(v = ci_G, col = 'red')

hist(results$t[, 3],
     main = 'Coefficient of Determination: Girth / Height',
     xlab = 'R-Squared',
     col = 'grey')
hist(
    results$t[, 3],
    main = 'Coefficient of Determination: Girth / Height',
    xlab = 'R-Squared',
    col = 'grey',
    prob = T
)
lines(density(results$t[, 3]), col = 'blue')
abline(v = ci_GH, col = 'red')

hist(results$t[, 4],
     main = 'Coefficient of Determination: Girth and Height',
     xlab = 'R-Squared',
     col = 'grey')
hist(
    results$t[, 4],
    main = 'Coefficient of Determination: Girth and Height',
    xlab = 'R-Squared',
    col = 'grey',
    prob = T
)
lines(density(results$t[, 4]), col = 'blue')
abline(v = ci_com, col = 'red')

hist(results$t[, 5],
     main = 'Coefficient of Determination: Girth, Height, and Girth / Height',
     xlab = 'R-Squared',
     col = 'grey')
hist(
    results$t[, 5],
    main = 'Coefficient of Determination: Girth, Height, and Girth / Height',
    xlab = 'R-Squared',
    col = 'grey',
    prob = T
)
lines(density(results$t[, 5]), col = 'blue')
abline(v = ci_com2, col = 'red')



# Demo ========================================================================
# http://people.tamu.edu/~alawing/materials/ESSM689/Bdemo.R

# Load the "boot" library, which contains the bootstrap functionality.
library(boot)

# Create a subset of data from "iris" dataset.  Get 10 of species versicolor and 10
# of species virginica.  Get sepal width information as well.
small_iris = iris[c(51:60, 101:110), c("Sepal.Width", "Species")]

# View the data.
small_iris
summary(small_iris)
boxplot(small_iris$Sepal.Width ~ small_iris$Species,
        xlab = 'Species',
        ylab = 'Sepal Width')
mean_difference_original = mean(small_iris$Sepal.Width[small_iris$Species == 'versicolor']) - mean(small_iris$Sepal.Width[small_iris$Species == 'virginica'])
cat("The difference in mean sepal width is", mean_difference_original)

## Null and alternative hypotheses to be tested ##
# H0: There is no difference in mean sepal width between virginica and versicolor
# Ha: There is a difference in mean sepal width between virginica and versicolor


# Create a function that will calculate the mean difference in sepal width
# between the two species.  This is the function that will be called on each
# sample created by the bootstrap procedure.
delta_mean_width = function(data, indices) {
    # Allows the boot function to resample the original dataset with replacement, creating
    # the individual bootstrap samples.
    d = data[indices, ]
    # Calculates the difference in the mean sepal width of the two species.
    # Notice that the function operates on 'd', the newly created sample, and not on the original sample 'small_iris'
    mean_difference = mean(d$Sepal.Width[d$Species == 'versicolor']) - mean(d$Sepal.Width[d$Species == 'virginica'])
    return(mean_difference)
}

# Conduct the bootstrap procedure, R number of resamples. This is a non-parametric bootstrap.
results = boot(data = small_iris,
               statistic = delta_mean_width,
               R = 1000)
# View the output of the boot function
results
# View the plots of the resultant boot object
plot(results)
# Calculate and view the 95% confidence interval using the 'Bias corrected and accelerated method'
confidence_interval = boot.ci(results, conf = 0.95, type = 'bca')
print(confidence_interval)

# Get confidence interval values
ci = confidence_interval$bca[c(4, 5)]

# View first (and last) six results of the bootstrap.  These values are the respective
# sample number and mean sepal width difference.
head(results$t)
tail(results$t)

# Create a boxplot and histogram of the results to visualize the bootstrap process.
# 95% confidence intervals plotted in red, kernel density estimation plotted in blue
boxplot(results$t,
        main = 'Difference in Mean Sepal Width: Iris versicolor vs. Iris virginica',
        ylab = 'Difference in Mean Sepal Width (cm)',
        col = 'grey')
abline(h = ci, col = 'red')
hist(results$t,
     main = 'Difference in Mean Sepal Width: Iris versicolor vs. Iris virginica',
     xlab = 'Difference in Mean Sepal Width (cm)',
     col = 'grey')
hist(
    results$t,
    main = 'Difference in Mean Sepal Width: Iris versicolor vs. Iris virginica',
    xlab = 'Difference in Mean Sepal Width (cm)',
    col = 'grey',
    prob = T
)
lines(density(results$t), col = 'blue')
abline(v = ci, col = 'red')
