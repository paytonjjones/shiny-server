wiki <- list()

generate_html_wiki <- function(title, description, rcode,
                               from = "Wikipedia, the free encyclopedia") {
  paste("<h2 style=\"display: inline;\">",
        title,
        "</h2>",
        "<body><div>",
        "from",
        from,
        "</div></body>",
        "<p><div>",
        description,
        "</div></p>",
        "<h3><div>",
        title,
        "in R  </div></h3>",
        "<code>",
        rcode,
        "</code>")
}

# ---- students_t ----
wiki$students_t <- generate_html_wiki("Student's t-test", "A t-test is any statistical hypothesis test in which the test statistic follows a Student's t-distribution under the null hypothesis. It can be used to determine if two sets of data are significantly different from each other.</div></p> <p><div> A t-test is most commonly applied when the test statistic would follow a normal distribution if the value of a scaling term in the test statistic were known. When the scaling term is unknown and is replaced by an estimate based on the data, the test statistics (under certain conditions) follow a Student's t distribution.", "t.test(vector, mu = 0)")

# ---- one_sample_median ----
wiki$one_sample_median <- "<h2 style=\"display: inline;\">One-sample median</h2>
                         <body><div> from the UCLA Institute for Digital Research and Education </div></body>
                         <p><div>A one sample median test allows us to test whether a sample median differs significantly from a hypothesized value.  The one-sample median is similar to a one-sample t-test, but we do not need to assume that it is interval and normally distributed (we only need to assume that our variable is an ordinal variable).</div></p>
                         <h3><div>One-sample median in R  </div></h3>
                         <code> wilcox.test(vector, mu = 0)</code>
                         "
# ---- binom ----
wiki$binom <- "<h2 style=\"display: inline;\">Binomial test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, the binomial test is an exact test of the statistical significance of deviations from a theoretically expected distribution of observations into two categories.</div></p>
                         <p><div>One common use of the binomial test is in the case where the null hypothesis is that two categories are equally likely to occur (such as a coin toss). Tables are widely available to give the significance observed numbers of observations in the categories for this case. However, as the example below shows, the binomial test is not restricted to this case.</div></p>
                         <p><div>Where there are more than two categories, and an exact test is required, the multinomial test, based on the multinomial distribution, must be used instead of the binomial test.</div></p>
                         <h3><div>Binomial test in R  </div></h3>
                         <code> prop.test(successes, trials, p = 0.5) </code>
                         "

# ---- chisqgof ----
wiki$chisqgof <- "<h2 style=\"display: inline;\">Pearson's chi-squared test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>Pearson's chi-squared test is a statistical test applied to sets of categorical data to evaluate how likely it is that any observed difference between the sets arose by chance. It is suitable for unpaired data from large samples. It is the most widely used of many chi-squared tests (e.g., Yates, likelihood ratio, portmanteau test in time series, etc.) – statistical procedures whose results are evaluated by reference to the chi-squared distribution. Its properties were first investigated by Karl Pearson in 1900. In contexts where it is important to improve a distinction between the test statistic and its distribution, names similar to Pearson chi-squared test or statistic are used.</p>
                         <p><div>It tests a null hypothesis stating that the frequency distribution of certain events observed in a sample is consistent with a particular theoretical distribution. The events considered must be mutually exclusive and have total probability 1. A common case for this is where the events each cover an outcome of a categorical variable. A simple example is the hypothesis that an ordinary six-sided die is \"fair\" (i.e., all six outcomes are equally likely to occur).</div></p>
                         <h3><div>Pearson's chi-squared test in R  </div></h3>
                         <code> chisq.test(x, p = rep(1/length(x), length(x))) </code>
                         "

# ---- indept ----
wiki$indept <- "<h2 style=\"display: inline;\">Independent (unpaired) samples t-test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>The independent samples t-test is used when two separate sets of independent and identically distributed samples are obtained, one from each of the two populations being compared. For example, suppose we are evaluating the effect of a medical treatment, and we enroll 100 subjects into our study, then randomly assign 50 subjects to the treatment group and 50 subjects to the control group. In this case, we have two independent samples and would use the unpaired form of the t-test. The randomization is not essential here; if we contacted 100 people by phone and obtained each person's age and gender, and then used a two-sample t-test to see whether the mean ages differ by gender, this would also be an independent samples t-test, even though the data is observational.</div></p>
                         <h3><div>Independent samples t-test in R  </div></h3>
                         <code> t.test(DV ~ IV) </code>
                         "

# ---- wmw ----
wiki$wmw <- "<h2 style=\"display: inline;\">Mann-Whitney U Test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, the Mann-Whitney U test (also called the Mann-Whitney-Wilcoxon (MWW), Wilcoxon rank-sum test, or Wilcoxon-Mann-Whitney test) is a nonparametric test of the null hypothesis that it is equally likely that a randomly selected value from one sample will be less than or greater than a randomly selected value from a second sample.</div></p>
                         <p><div>Unlike the t-test it does not require the assumption of normal distributions. It is nearly as efficient as the t-test on normal distributions.</div></p>
                         <h3><div>Mann-Whitney U Test in R  </div></h3>
                         <code> wilcox.test(DV ~ IV) </code>
                         "

# ---- chisq ----
wiki$chisq <- "<h2 style=\"display: inline;\">Chi-squared test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>A chi-squared test is any statistical hypothesis test wherein the sampling distribution of the test statistic is a chi-squared distribution when the null hypothesis is true. Without other qualification, 'chi-squared test' often is used as short for Pearson's chi-squared test.</div></p>
                         <p><div>Chi-squared tests are often constructed from a sum of squared errors, or through the sample variance. Test statistics that follow a chi-squared distribution arise from an assumption of independent normally distributed data, which is valid in many cases due to the central limit theorem. A chi-squared test can be used to attempt rejection of the null hypothesis that the data are independent.</div></p>
                         <h3><div>Chi-squared test in R  </div></h3>
                         <code> chisq.test(table(DV, IV)) </code>
                         "
# ---- fisher_link ----

wiki$fisher <- "<h2 style=\"display: inline;\">Fisher's exact test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>Fisher's exact test is a statistical significance test used in the analysis of contingency tables. Although in practice it is employed when sample sizes are small, it is valid for all sample sizes. It is named after its inventor, Ronald Fisher, and is one of a class of exact tests, so called because the significance of the deviation from a null hypothesis (e.g., P-value) can be calculated exactly, rather than relying on an approximation that becomes exact in the limit as the sample size grows to infinity, as with many statistical tests.</div></p>
                         <p><div>Fisher is said to have devised the test following a comment from Dr. Muriel Bristol, who claimed to be able to detect whether the tea or the milk was added first to her cup. He tested her claim in the \"lady tasting tea\" experiment.</div></p>
                         <p><div>The test is useful for categorical data that result from classifying objects in two different ways. It is used to examine the significance of the association (contingency) between the two kinds of classification. So in Fisher's original example, one criterion of classification could be whether milk or tea was put in the cup first. The other could be whether Dr. Bristol thinks that the milk or tea was put in first. We want to know whether these two classifications are associated; that is, whether Dr. Bristol really can tell whether milk or tea was poured in first. Most uses of the Fisher test involve, like this example, a 2x2 contingency table. The p-value from the test is computed as if the margins of the table are fixed, i.e. as if, in the tea-tasting example, Dr. Bristol knows the number of cups with each treatment (milk or tea first) and will therefore provide guesses with the correct number in each category.</div></p>
                         <h3><div>Fisher's exact test in R  </div></h3>
                         <code> fisher.test(table(DV, IV)) </code>
                         "

# ---- anova ----

wiki$anova <- "<h2 style=\"display: inline;\">One-way analysis of variance</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, one-way analysis of variance (abbreviated one-way ANOVA) is a technique used to compare means of three or more samples (using the F distribution). This technique can be used only for numerical data.</div></p>
                         <p><div>The ANOVA tests the null hypothesis that samples in two or more groups are drawn from populations with the same mean values. To do this, two estimates are made of the population variance. These estimates rely on various assumptions (see below). The ANOVA produces an F-statistic, the ratio of the variance calculated among the means to the variance within the samples. If the group means are drawn from populations with the same mean values, the variance between the group means should be lower than the variance of the samples, following the central limit theorem. A higher ratio therefore implies that the samples were drawn from populations with different mean values.</div></p>
                         <h3><div>One-way ANOVA in R  </div></h3>
                         <code> aov(DV ~ IV) </code>
                         "

# ---- kw ----

wiki$kw <- "<h2 style=\"display: inline;\">Kruskal-Wallis one-way ANOVA</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>The Kruskal-Wallis test by ranks, Kruskal-Wallis H test (named after William Kruskal and W. Allen Wallis), or One-way ANOVA on ranks is a non-parametric method for testing whether samples originate from the same distribution. It is used for comparing two or more independent samples of equal or different sample sizes. It extends the Mann-Whitney U test when there are more than two groups. The parametric equivalent of the Kruskal-Wallis test is the one-way analysis of variance (ANOVA). A significant Kruskal-Wallis test indicates that at least one sample stochastically dominates one other sample. The test does not identify where this stochastic dominance occurs or for how many pairs of groups stochastic dominance obtains. Dunn's test, or the more powerful but less well known Conover-Iman test would help analyze the specific sample pairs for stochastic dominance in post hoc tests.</p>
                         <p><div>Since it is a non-parametric method, the Kruskal-Wallis test does not assume a normal distribution of the residuals, unlike the analogous one-way analysis of variance. If the researcher can make the less stringent assumptions of an identically shaped and scaled distribution for all groups, except for any difference in medians, then the null hypothesis is that the medians of all groups are equal, and the alternative hypothesis is that at least one population median of one group is different from the population median of at least one other group.</div></p>  
                         <h3><div>Kruskal-Wallis one-way ANOVA in R  </div></h3>
                         <code> kruskal.test(DV, IV) </code>
                         "

# ---- pairedt ----

wiki$pairedt <- "<h2 style=\"display: inline;\">Paired samples t-test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>Paired samples t-tests typically consist of a sample of matched pairs of similar units, or one group of units that has been tested twice (a \"repeated measures\" t-test).</div></p>
                         <p><div>A typical example of the repeated measures t-test would be where subjects are tested prior to a treatment, say for high blood pressure, and the same subjects are tested again after treatment with a blood-pressure lowering medication. By comparing the same patient's numbers before and after treatment, we are effectively using each patient as their own control. That way the correct rejection of the null hypothesis (here: of no difference made by the treatment) can become much more likely, with statistical power increasing simply because the random between-patient variation has now been eliminated. Note however that an increase of statistical power comes at a price: more tests are required, each subject having to be tested twice. Because half of the sample now depends on the other half, the paired version of Student's t-test has only \"n/2–1\" degrees of freedom (with n being the total number of observations). Pairs become individual test units, and the sample has to be doubled to achieve the same number of degrees of freedom.</div></p>
                         <h3><div>Paired samples t-test in R  </div></h3>
                         <code> t.test(DV, IV, paired = TRUE) </code>
                         "
  
# ---- wsrt ----
wiki$wsrt <- "<h2 style=\"display: inline;\">Wilcoxon signed-rank test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>The Wilcoxon signed-rank test is a non-parametric statistical hypothesis test used when comparing two related samples, matched samples, or repeated measurements on a single sample to assess whether their population mean ranks differ (i.e. it is a paired difference test). It can be used as an alternative to the paired Student's t-test, t-test for matched pairs, or the t-test for dependent samples when the population cannot be assumed to be normally distributed.</p>
                         <h3><div>Wilcoxon signed-rank test in R  </div></h3>
                         <code> wilcox.test(DV, IV, paired = TRUE) </code>
                         "
# ---- mcnemar ----
wiki$mcnemar <- "<h2 style=\"display: inline;\">McNemar's test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>In statistics, McNemar's test is a statistical test used on paired nominal data. It is applied to 2 × 2 contingency tables with a dichotomous trait, with matched pairs of subjects, to determine whether the row and column marginal frequencies are equal (that is, whether there is \"marginal homogeneity\"). It is named after Quinn McNemar, who introduced it in 1947. An application of the test in genetics is the transmission disequilibrium test for detecting linkage disequilibrium.</p>
                         <h3><div>McNemar's test in R  </div></h3>
                         <code> contingency.table.matrix <- matrix(c(a, b, c, d), 2, 2) </code>
                         <div><code> mcnemar.test(contingency.table.matrix) </code></div>
                         "
# ---- rmanova ----
wiki$rmanova <- "<h2 style=\"display: inline;\">Repeated measures ANOVA</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>Repeated measures analysis of variance (rANOVA) is a commonly used statistical approach to repeated measure designs. With such designs, the repeated-measure factor (the qualitative independent variable) is the within-subjects factor, while the dependent quantitative variable on which each participant is measured is the dependent variable.</p>
                         <h3><div>Repeated measures ANOVA in R  </div></h3>
                         <code> require(car) </code>
                         <div><code> fit <- Anova(lm(y~ a + s), data = data) </code></div>
                         "
# ---- friedman ----
wiki$friedman <- "<h2 style=\"display: inline;\">Friedman test</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>The Friedman test is a non-parametric statistical test developed by Milton Friedman. Similar to the parametric repeated measures ANOVA, it is used to detect differences in treatments across multiple test attempts. The procedure involves ranking each row (or block) together, then considering the values of ranks by columns. Applicable to complete block designs, it is thus a special case of the Durbin test.</div></p>
                         <p><div>Classic examples of use are:</div></p>
                         <p><div>n wine judges each rate k different wines. Are any of the k wines ranked consistently higher or lower than the others?</div></p>
                         <p><div>n welders each use k welding torches, and the ensuing welds were rated on quality. Do any of the k torches produce consistently better or worse welds?</div></p>
                         <p><div>The Friedman test is used for one-way repeated measures analysis of variance by ranks. In its use of ranks it is similar to the Kruskal-Wallis one-way analysis of variance by ranks.</div></p>
                         <h3><div>Friedman test in R  </div></h3>
                         <code> friedman.test(DV, groups, blocks) </code>
                         "
# ---- rmlog ----
wiki$rmlog <- "<h2 style=\"display: inline;\">Repeated measures logistic regression</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>In statistics, logistic regression, or logit regression, or logit model is a regression model where the dependent variable (DV) is categorical. This article covers the case of a binary dependent variable; that is, where it can take only two values, \"0\" and \"1\", which represent outcomes such as pass/fail, win/lose, alive/dead or healthy/sick. Cases where the dependent variable has more than two outcome categories may be analysed in multinomial logistic regression, or, if the multiple categories are ordered, in ordinal logistic regression. In the terminology of economics, logistic regression is an example of a qualitative response/discrete choice model.</p>
                         <p><div>Repeated measures logistic regression involves logistic regression of two or more matched groups</div></p>
                         <h3><div>Repeated measures logistic regression in R  </div></h3>
                         <code> require(lme4) </code>
                         <div><code> glmer(DV ~ IV + (1 | id), data = data, family = binomial) </code></div>
                         "
# ---- facanova ----
wiki$facanova <- "<h2 style=\"display: inline;\">Two-way analysis of variance</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, the two-way analysis of variance (ANOVA) is an extension of the one-way ANOVA that examines the influence of two different categorical independent variables on one continuous dependent variable. The two-way ANOVA not only aims at assessing the main effect of each independent variable but also if there is any interaction between them.</div></p>
                         <p><div>Factorial ANOVA refers to ANOVA which is two-way or greater</div></p>
                         <h3><div>Factorial ANOVA in R  </div></h3>
                         <code> anova(lm(DV ~ IV1 * IV2, data = data)) </code>
                         "
# ---- ordlog ----
wiki$ordlog <- "<h2 style=\"display: inline;\">Ordered logit</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, the ordered logit model (also ordered logistic regression or proportional odds model), is an ordinal regression model; that is, a regression model for ordinal dependent variables, first considered by Peter McCullagh. For example, if one question on a survey is to be answered by a choice among \"poor\", \"fair\", \"good\", \"very good\", and \"excellent\", and the purpose of the analysis is to see how well that response can be predicted by the responses to other questions, some of which may be quantitative, then ordered logistic regression may be used. It can be thought of as an extension of the logistic regression model that applies to dichotomous dependent variables, allowing for more than two (ordered) response categories.</div></p>
                         <h3><div>Ordinal logistic regression in R in R  </div></h3>
                         <code> require(MASS) </code>
                         <div><code> fit <- polr(DV ~ IV1 + IV2, data = data) </code></div>
                         "
# ---- faclog ----
wiki$faclog <-"<h2 style=\"display: inline;\">Factorial logistic regression</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>In statistics, logistic regression, or logit regression, or logit model is a regression model where the dependent variable (DV) is categorical. This article covers the case of a binary dependent variable; that is, where it can take only two values, \"0\" and \"1\", which represent outcomes such as pass/fail, win/lose, alive/dead or healthy/sick. Cases where the dependent variable has more than two outcome categories may be analysed in multinomial logistic regression, or, if the multiple categories are ordered, in ordinal logistic regression. In the terminology of economics, logistic regression is an example of a qualitative response/discrete choice model.</p>
                         <p><div>Factorial logistic regression involves logistic regression including interactions</div></p>
                         <h3><div>Factorial logistic regression in R  </div></h3>
                         <code> glm(DV ~ IV1 * IV2, data = data, family = binomial) </code>
                         "
# ---- corr ----
wiki$corr <- "<h2 style=\"display: inline;\">Pearson correlation coefficient</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>In statistics, the Pearson correlation coefficient, also referred to as the Pearson's r or Pearson product-moment correlation coefficient (PPMCC), is a measure of the linear dependence (correlation) between two variables X and Y. It has a value between +1 and -1 inclusive, where 1 is total positive linear correlation, 0 is no linear correlation, and -1 is total negative linear correlation. It is widely used in the sciences. It was developed by Karl Pearson from a related idea introduced by Francis Galton in the 1880s. Early work on the distribution of the sample correlation coefficient was carried out by Anil Kumar Gain and R. A. Fisher.</p>
                         <h3><div>Correlation in R  </div></h3>
                         <code> cor.test(DV, IV) </code>
                         "
# ---- reg ----
wiki$reg <- "<h2 style=\"display: inline;\">Regression analysis</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>In statistical modeling, regression analysis is a statistical process for estimating the relationships among variables. It includes many techniques for modeling and analyzing several variables, when the focus is on the relationship between a dependent variable and one or more independent variables (or 'predictors'). More specifically, regression analysis helps one understand how the typical value of the dependent variable (or 'criterion variable') changes when any one of the independent variables is varied, while the other independent variables are held fixed. Most commonly, regression analysis estimates the conditional expectation of the dependent variable given the independent variables – that is, the average value of the dependent variable when the independent variables are fixed. Less commonly, the focus is on a quantile, or other location parameter of the conditional distribution of the dependent variable given the independent variables. In all cases, the estimation target is a function of the independent variables called the regression function.</p>
                         <h3><div>Linear regression in R  </div></h3>
                         <code> lm(DV ~ IV) </code>
                         "
# ---- npc ----
wiki$npc <- "<h2 style=\"display: inline;\">Spearman's rank correlation coefficient</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div></div>In statistics, Spearman's rank correlation coefficient or Spearman's rho, named after Charles Spearman and often denoted by the Greek letter rho is a nonparametric measure of rank correlation (statistical dependence between the ranking of two variables). It assesses how well the relationship between two variables can be described using a monotonic function.</p>
                         <p><div>The Spearman correlation between two variables is equal to the Pearson correlation between the rank values of those two variables; while Pearson's correlation assesses linear relationships, Spearman's correlation assesses monotonic relationships (whether linear or not). If there are no repeated data values, a perfect Spearman correlation of +1 or −1 occurs when each of the variables is a perfect monotone function of the other.</div></p>
                         <h3><div>Spearman correlation in R  </div></h3>
                         <code> cor.test(DV, IV, method = \"spearman\") </code>
                         "
# ---- log ----
wiki$log <- "<h2 style=\"display: inline;\">Logistic regression</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, logistic regression, or logit regression, or logit model is a regression model where the dependent variable (DV) is categorical. This article covers the case of a binary dependent variable—that is, where it can take only two values, \"0\" and \"1\", which represent outcomes such as pass/fail, win/lose, alive/dead or healthy/sick. Cases where the dependent variable has more than two outcome categories may be analysed in multinomial logistic regression, or, if the multiple categories are ordered, in ordinal logistic regression. In the terminology of economics, logistic regression is an example of a qualitative response/discrete choice model.</div></p>
                         <p><div>Logistic regression was developed by statistician David Cox in 1958. The binary logistic model is used to estimate the probability of a binary response based on one or more predictor (or independent) variables (features). It allows one to say that the presence of a risk factor increases the probability of a given outcome by a specific percentage.</div></p>
                         <h3><div>Logistic regression in R  </div></h3>
                         <code> glm(DV ~ IV, family = binomial) </code>
                         "
# ---- multreg ----
wiki$multreg <- "<h2 style=\"display: inline;\">Simple and multiple regression</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>The very simplest case of a single scalar predictor variable x and a single scalar response variable y is known as simple linear regression. The extension to multiple and/or vector-valued predictor variables (denoted with a capital X) is known as multiple linear regression, also known as multivariable linear regression. Nearly all real-world regression models involve multiple predictors, and basic descriptions of linear regression are often phrased in terms of the multiple regression model. Note, however, that in these cases the response variable y is still a scalar. Another term multivariate linear regression refers to cases where y is a vector, i.e., the same as general linear regression.</div></p>
                         <h3><div>Multiple regression in R  </div></h3>
                         <code> lm(DV ~ IV1 + IV2) </code>
                         "
# ---- ancova ----
wiki$ancova <- "<h2 style=\"display: inline;\">Analysis of covariance</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>Analysis of covariance (ANCOVA) is a general linear model which blends ANOVA and regression. ANCOVA evaluates whether population means of a dependent variable (DV) are equal across levels of a categorical independent variable (IV) often called a treatment, while statistically controlling for the effects of other continuous variables that are not of primary interest, known as covariates (CV) or nuisance variables. Mathematically, ANCOVA decomposes the variance in the DV into variance explained by the CV(s), variance explained by the categorical IV, and residual variance. Intuitively, ANCOVA can be thought of as 'adjusting' the DV by the group means of the CV(s).</div></p>
                         <h3><div>ANCOVA in R  </div></h3>
                         <code> aov(DV ~ IV1 + IV2) </code>
                         "
# ---- multlog ----
wiki$multlog <- "<h2 style=\"display: inline;\">Multiple logistic regression</h2>
                         <body><div> from Boston University School of Public Health </div></body>
                         <p><div>Logistic regression analysis is a popular and widely used analysis that is similar to linear regression analysis except that the outcome is dichotomous (e.g., success/failure or yes/no or died/lived). The epidemiology module on Regression Analysis provides a brief explanation of the rationale for logistic regression and how it is an extension of multiple linear regression. In essence, we examine the odds of an outcome occurring (or not), and by using the natural log of the odds of the outcome as the dependent variable the relationships can be linearized and treated much like multiple linear regression.</div></p>                  
                         <p><div>Simple logistic regression analysis refers to the regression application with one dichotomous outcome and one independent variable; multiple logistic regression analysis applies when there is a single dichotomous outcome and more than one independent variable.</div></p>
                         <h3><div>Multiple logistic regression in R  </div></h3>
                         <code> glm(DV ~ IV1 + IV2, family = binomial) </code>
                         "
# ---- disc ----
wiki$disc <- "<h2 style=\"display: inline;\">Linear discriminant analysis</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>Linear discriminant analysis (LDA) is a generalization of Fisher's linear discriminant, a method used in statistics, pattern recognition and machine learning to find a linear combination of features that characterizes or separates two or more classes of objects or events. The resulting combination may be used as a linear classifier, or, more commonly, for dimensionality reduction before later classification.</div></p>
                         <p><div>LDA is closely related to analysis of variance (ANOVA) and regression analysis, which also attempt to express one dependent variable as a linear combination of other features or measurements. However, ANOVA uses categorical independent variables and a continuous dependent variable, whereas discriminant analysis has continuous independent variables and a categorical dependent variable (i.e. the class label). Logistic regression and probit regression are more similar to LDA than ANOVA is, as they also explain a categorical variable by the values of continuous independent variables. These other methods are preferable in applications where it is not reasonable to assume that the independent variables are normally distributed, which is a fundamental assumption of the LDA method.</div></p>
                         <h3><div>Discriminant analysis in R  </div></h3>
                         <code> require(MASS) </code>
                         <code><div> lda(DV ~ IV1 + IV2, data=data) </code></div>
                         "
# ---- manova ----
wiki$manova <- "<h2 style=\"display: inline;\">Multivariate analysis of variance</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, multivariate analysis of variance (MANOVA) is a procedure for comparing multivariate sample means. As a multivariate procedure, it is used when there are two or more dependent variables, and is typically followed by significance tests involving individual dependent variables separately. It helps to answer</div></p>
                         <p><div>1. Do changes in the independent variable(s) have significant effects on the dependent variables?</div></p>
                         <p><div>2. What are the relationships among the dependent variables?</div></p>
                         <p><div>3. What are the relationships among the independent variables?</div></p>
                         <h3><div> MANOVA in R  </div></h3>
                         <code> manova(cbind(DV1, DV2) ~ IV) </code>
                         "
# ---- manova2 ----
wiki$manova2 <- "<h2 style=\"display: inline;\">Repeated measures MANOVA</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>In statistics, multivariate analysis of variance (MANOVA) is a procedure for comparing multivariate sample means. As a multivariate procedure, it is used when there are two or more dependent variables, and is typically followed by significance tests involving individual dependent variables separately. It helps to answer</div></p>
                         <p><div>1. Do changes in the independent variable(s) have significant effects on the dependent variables?</div></p>
                         <p><div>2. What are the relationships among the dependent variables?</div></p>
                         <p><div>3. What are the relationships among the independent variables?</div></p>
                         <p><div> Repeated measures MANOVA is used when the independent variable(s) are dependent/matched groups </div> </p>
                         <h3><div> Repeated measures MANOVA in R  </div></h3>
                         <code> require(car) </code>
                         <div><code> Anova(lm(DV1 + DV2 ~ IV, data=data), type=\"III\") </code></div>
                         "
# ---- mmreg ----
wiki$mmreg <- "<h2 style=\"display: inline;\">Multivariate multiple regression</h2>
                         <body><div> from JNCASR Research Institute </div></body>
                         <p><div>Multivariate analysis is essentially the statistical process of simultaneously analyzing multiple independent (or predictor) variables with multiple dependent (outcome or criterion) variables
                         using matrix algebra (most multivariate analyses are correlational). While these analyses have been a
                         part of statistics since the early 1900s, the development of mainframe and microcomputers and
                         subsequent analytical software has made the once tedious calculations fairly simple and very fast. </div></p>
                         <h3><div> Multivariate multiple regression in R  </div></h3>
                         <code> lm(cbind(DV1, DV2) ~ IV1 + IV2, data = data) </code>
                         "
# ---- factora ----
wiki$factora <- "<h2 style=\"display: inline;\">Factor analysis</h2>
                         <body><div> from Wikipedia, the free encyclopedia </div></body>
                         <p><div>Factor analysis is a statistical method used to describe variability among observed, correlated variables in terms of a potentially lower number of unobserved variables called factors. For example, it is possible that variations in six observed variables mainly reflect the variations in two unobserved (underlying) variables. Factor analysis searches for such joint variations in response to unobserved latent variables. The observed variables are modelled as linear combinations of the potential factors, plus \"error\" terms. Factor analysis aims to find independent latent variables. Followers of factor analytic methods believe that the information gained about the interdependencies between observed variables can be used later to reduce the set of variables in a dataset. Factor analysis is not used to any significant degree in physics, biology and chemistry but is used very heavily in psychometrics personality theories, marketing, product management, operations research. Users of factor analysis believe that it helps to deal with data sets where there are large numbers of observed variables that are thought to reflect a smaller number of underlying/latent variables.</div></p>
                         <h3><div> Multivariate multiple regression in R  </div></h3>
                         <code> require(psych) </code>
                         <div><code> fa(r = cor(cbind(DV1,DV2,DV3)), rotate = \"none\", fm = \"pa\") </code></div>
                         "



# verify wd before saving
saveRDS(wiki, file = "wiki.RDS")
