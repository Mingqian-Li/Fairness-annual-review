1. Status

(1).Fairness test based on different regression models, linear regression, group lasso and sparse group lasso.

(2).Most recent results are in Group regression.ipynb.

(3).All the regularization terms are set to 0.1 for all dataset, which is not optimium for now. 

(4).For some datasets, when doing sparse group lasso, seems L21 regularization term is too small, so the elementwize l1 term is dominating. L21 needs larger weight.

(5).Models are based on paper https://www.cs.utexas.edu/~pradeepr/paperz/mtdm_nips.pdf. Need to pay attention to several conditions. This dirty model may not be able to work under some circumstances, such as insufficient sample numbers or oversize features.

2. To-do

(1).Linear regression with mse loss maynot be good for classification problems. But since group lasso is based on linear MSE, for convenience and fair comparison, all models are trained based on Linear MSE. Logsitics model would be better, need to check how the dirty model works with logistics model. Also if the loss is changed to cross entropy, how to optimize the model parameters.

(2).Tunning regularized terms for different dataset.

(3).Multiple classes.

3. Comparison

Red means the fairness performance is better, but result may not be reliable because of small training dataset sample numbers. Yellow means the fairness performance is better, also reliable. 

![image](https://user-images.githubusercontent.com/70342781/219811476-9052d519-c557-4eb2-b34a-628620ea7af7.png)

