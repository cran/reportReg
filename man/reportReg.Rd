\name{reportReg}
\alias{reportReg}
\title{An Easy Way to Report Regression Analysis}
\description{
Provides an easy way to report the results of regression analysis, including:
1. Proportional hazards regression model from function 'coxph' of package 'survival';
2. Ordered logistic regression from function 'polr' of package 'MASS';
3. Binary logistic regression from function 'glm' of package 'stats';
4. Linear regression from function 'lm' of packages 'stats';
5. Risk regression model for survival analysis with competing risks from function 'FGR' of package 'riskRegression';
6. Multilevel model from function 'lme' of package 'nlme'.
}
\usage{
reportReg(fit)
}
\arguments{
  \item{fit}{object of regression, including 'survival::coxph', 'MASS::polr', 'glm', 'lm' and 'riskRegression::FGR'}
}
\value{
  \item{OR}{odds ratio}
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
}

\examples{
x=rnorm(10)
y=rnorm(10)
fit=lm(y~x)
reportReg(fit)
}
