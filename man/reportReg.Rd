\name{reportReg}
\alias{reportReg}
\title{An Easy Way to Report Regression Analysis}
\description{
Provides an easy way to report the results of regression analysis, including:
1. Proportional hazards regression model from function 'coxph' of package 'survival';
2. Ordered logistic regression from function 'polr' of package 'MASS';
3. Binary logistic regression from function 'glm' of package 'stats';
4. Linear regression from function 'lm' of packages 'stats'.
}
\usage{
reportReg(fit)
}
\arguments{
  \item{fit}{object of regression, including 'survival::coxph', 'MASS::polr', 'glm', and 'lm'}
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

Update description:

more functions will be included in 'reportReg' package!
}

\examples{
x=rnorm(10)
y=rnorm(10)
fit=lm(y~x)
reportReg(fit)
}
