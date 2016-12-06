library(tidyverse)
library(apaTables)
library(cocor)

data_set <- read_csv("bfi2.csv")

# QUESTION 1

cocor(~A1+C1 | E1+O1, data = as.data.frame(data_set))
# The correlation between (A1,C1) and (E1,O1) was $\Delta r$ = -.01, 95% CI[-.11, .09]. 
## NOTE: you get -.0099 from output that says "Difference: r.jk - r.hm = -0.0099"


# QUESTION 2 

cocor(~A1+C1 | A1+E1, data = as.data.frame(data_set))
# The correlation between (A1,C1) and (A1,E1) was $\Delta r$ = -.08, 95% CI[-.18, .02].


# QUESTION 3

bfi_men <- data_set %>% filter(gender==1) %>% select(-gender)
bfi_women <- data_set %>% filter(gender==2) %>% select(-gender)

apa.cor.table(bfi_men)
apa.cor.table(bfi_women)

bfi_men_dataframe <- as.data.frame(bfi_men)
bfi_women_dataframe <- as.data.frame(bfi_women)

cocor(~A1+E1 | A1+E1, data = list(bfi_men_dataframe, bfi_women_dataframe))
# The (A1,E1) correlation for men and the (A1,E1) correlation for women was $\Delta r$ = .02, 95% CI[-.13, .17].


# QUESTION 4

r.jk <- .59 # Correlation (rating, raises)
r.jh <- .16 # Correlation (rating, critical)
r.kh <- .38 # Correlation (raises, critical)
n <- 30

?cocor

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, alternative = "two.sided", test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0, data.name = NULL, var.labels = NULL, return.htest = FALSE)
# The comparison for the correlations between rating-raises and rating-critical was $\Delta r$ = .43, 95% CI [.07, .79]. The data suggests that the difference correlation is likely positive. 


# QUESTION 5

r.jk <- .59 # Correlation (rating, raises)
r.hm <- .19 # Correlation (complaints, critical)
r.jh <- .16 # Correlation (rating, critical)
r.jm <- .83 # Correlation (rating, complaints)
r.kh <- .38 # Correlation (raises, critical)
r.km <- .67 # Correlation (rating, complaints)
n <- 30 

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n,alternative = "two.sided", test = "all", alpha = 0.05,conf.level = 0.95, null.value = 0, data.name = NULL,var.labels = NULL, return.htest = FALSE)
# The comparison for the correlations between rating-raises and complaints-critical was $\Delta r$ = .40, 95% CI [.01, .78]. The data suggests that the difference correlation is likely positive.

# QUESTION 6

r1.jk <- .59
r2.hm <- .03
n1 <- 30
n2 <- 3000

cocor.indep.groups(r1.jk, r2.hm, n1, n2, alternative = "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                   data.name = NULL, var.labels = NULL, return.htest = FALSE)

# The comparison for the correlations between the original rating-raises and repeated rating-raises was $\Delta r$ = .56, 95% CI [.26, .76]. The data suggests that the difference correlation is likely positive.


# QUESTION 7

# See write-ups