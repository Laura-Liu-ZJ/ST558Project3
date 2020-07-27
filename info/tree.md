#### Introduction
Classification tree if goal is to classify (predict) group membership.

For a binary response, within a given node (p=P(correct classification))

$$ Deviance: -2plog(p)-2(1-p)log(1-p)$$

For all possible splits, minimize this (weighted appropriately for node size).

We need to pruned back using classification error rate to prevent overfit, which might increase bias but decrease variance, hopefully improving prediction.