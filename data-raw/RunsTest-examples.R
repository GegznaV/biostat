library(tidyverse)

clerk_salary <- data.frame(salary = c(525,500,500,576,458,600,700,886,600),
                           genderFM = c("F", "F","F","F","F","F","M","M","M"))

clerk_salary$genderMF <- factor(clerk_salary$genderFM, levels = c("M", "F"))

unclass(clerk_salary)

boxplot(salary ~ gender, data = clerk_salary)

# Answers are different

DescTools::RunsTest(salary ~ genderFM, data = clerk_salary)
DescTools::RunsTest(salary ~ genderMF, data = clerk_salary)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- data.frame(    x = c(52,  50, 50,  50, 45, 60, 70, 88, 60),
                 group = c("A", "A","A","B", "A","A","B","B","B"),
                 stringsAsFactors = FALSE)
# df$group2 <- factor(df$group, levels = c("B", "A"))

DescTools::RunsTest(x ~ group,  data = df)
DescTools::RunsTest(x ~ group2, data = df)

df2 <- df %>%
    arrange(x) %>%
    mutate(nr = row_number(x))$V5


duplicated_values <- function(x) {
    unique(x[duplicated(x)])
}

duplicated_values(df2$x)


split(df2$group, df2$x)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- c("A", "B", "B")

.group <- df2$group
.x     <- df2$x

.group <- clerk_salary$gender
.x     <- clerk_salary$salary



levs <- levels(y)
m <- sum(y == levs[1])
n <- sum(y == levs[2])
if (nlevels(x) %nin% c(1, 2))
    stop("Can only process dichotomous variables")

get_runs <- function(x) {
    x <- factor(x)
    x <- as.numeric(x) - 1
    runs <- sum(diff(x) != 0) + 1
    runs
}

get_permutations <- function(x) {
    n <- length(x)

    # All possible combinations of elements
    ve <- as.matrix(expand.grid(lapply(1:n, function(i){1:n})))

    # Combinations, in which the same object does not repeat
    ind_mat_ok <- apply(ve, 1, function(i)  all(1:n %in% i))
    # Permutations of indices
    dfi <- as.data.frame(t(ve[ind_mat_ok, ]))

    # Permutations of original inputs (with duplication)
    x_pernutations <- lapply(dfi, function(i) x[i])

    # Remove duplications
    x_pernutations[!duplicated(x_pernutations)]
}

split_list <- split(.group, .x)
rez_df <- expand.grid(lapply(split_list, get_permutations))

rez <- apply(rez_df, 1, unlist)

apply(rez, 2, get_runs)

rez

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- df$x[df$group == "A"]
y <- df$x[df$group == "B"]

    x <- clerk_salary$salary
group <- clerk_salary$gender
