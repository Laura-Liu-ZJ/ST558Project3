#### Introduction

Logistic regression is a statistical model that in its basic form uses a logistic function to model a binary dependent variable. 

Basic Logistic Regression models survive probability using logistic function, where $\boldsymbol{\beta} = (\beta_1,...\beta_p)^T,\boldsymbol{x}=(x_1,...x_p)^T$

$$P(survive=1|\boldsymbol{x})=\frac{e^{\alpha+\boldsymbol{\beta}^T\boldsymbol{x}}}{1+e^{\alpha+\boldsymbol{\beta}^T\boldsymbol{x}}}$$

$$log\left[\frac{P(survive=1|\boldsymbol{x})}{P(survive=0|\boldsymbol{x})}\right]=\alpha+\boldsymbol{\beta}^T\boldsymbol{x}$$