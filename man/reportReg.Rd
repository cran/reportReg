\name{reportReg}
\alias{reportReg}
\title{An Easy Way to Report Regression Analysis}
\description{
Provides an easy way to report the results of regression analysis, including:
1. Proportional hazards regression from function 'coxph' of package 'survival';
2. Conditional logistic regression from function 'clogit' of package 'survival';
3. Ordered logistic regression from function 'polr' of package 'MASS';
4. Binary logistic regression from function 'glm' of package 'stats';
5. Linear regression from function 'lm' of packages 'stats';
6. Risk regression model for survival analysis with competing risks from function 'FGR' of package 'riskRegression';
7. Multilevel model from function 'lme' of package 'nlme'.
}
\usage{
reportReg(fit,expTr)
}
\arguments{
  \item{fit}{object of regression, including 'survival::coxph', 'survival::clogit', 'MASS::polr', 'glm', 'lm' and 'riskRegression::FGR'}
  \item{expTr}{TRUE or FALSE, should the coefficients be transformed by exponential function, default is TRUE}
}
\value{
  \item{OR}{odds ratio}
  \item{HR}{hazard ratio}
  \item{Beta}{regression coefficient}
  \item{CI}{95 percent confidence interval}
  \item{P}{p value}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

More functions will be included in 'reportReg' package!

Update description:

Version 0.2.0: 1. the model with just intercept can be identified. 2. risk regression model for survival analysis with competing risks was included and supported. 3. the estimated statistic of cox regression is updated from 'OR' to 'HR'. 4. multilevel model was included and supported.

Version 0.3.0: 1. argument 'expTr' was added. 2. the 'clogit' fucntion from 'survival' package was added in the description and argument of 'fit'. 3. the 'coxph' function with 'cluster' was available.
}

\examples{
x=rnorm(10)
y=rnorm(10)
fit=lm(y~x)
reportReg(fit)
}
