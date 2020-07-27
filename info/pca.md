#### Introduction to PCA

Principal components analysis (PCA) is a dimension reduction technique that is widely used in multivariate statistics. The objective is to condense the information that is present in the original set of variables via linear combinations of the variables while losing as little information as possible. 

Typically, the number of linear transformations is much smaller than the number of original variables; hence the reduction in the dimensionality of the data. This can be useful in different ways, such as providing better visualization and computational advantages. PCA also decorrelates the data, that is, PCA produces linear combinations of the variables that are mutually uncorrelated.

**Goal:**

Obtain a linear combination of the variables that accounts for the largest amount of variability (assume 0 mean for each predictor).

$z_{i1} = \phi_{11}x_{i1}+ \phi_{21}x_{i2}+...+ \phi_{p1}x_{ip}$

**Constraint:**

- $\Sigma^p_{j=1}\phi^2_{j1}=1$.

- $\mathop{max}\limits_{\phi's}\frac{1}{n}\Sigma^n_{i=1}z^2_{i1}$.

- PCs must be mutually exclusive.

