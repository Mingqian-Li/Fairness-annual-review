1. Status

(1).Fairness test based on different regression models, linear regression, group lasso and sparse group lasso.

(2).Most recent results are in Group regression.ipynb.

(3).All the regularization terms are set to 0.1 for all dataset, which is not optimium for now. 

(4).For some datasets, when doing sparse group lasso, seems L21 regularization term is too small, so the elementwize l1 term is dominating. L21 needs larger weight.

(5).Models are based on paper https://www.cs.utexas.edu/~pradeepr/paperz/mtdm_nips.pdf. Need to pay attention to several conditions. This dirty model may not be able to work under some circumstances, such as insufficient sample numbers or oversize features.

2. To-do

(1).Linear regression with mse loss maynot be good for classification problems. But since group lasso is based on linear MSE, for convenience and fair comparison, all models are trained based on Linear MSE. Logsitics model would be better, need to check how the dirty model works with logistics model. Also if the loss is changed to cross entropy, how to optimize the model parameters(done).

(2).Tunning regularized terms for different dataset.

(3).Multiple classes(done).

3. Comparison

Red means the fairness performance is better, but result may not be reliable because of small training dataset sample numbers. Yellow means the fairness performance is better, also reliable. 

![image](https://user-images.githubusercontent.com/70342781/219811476-9052d519-c557-4eb2-b34a-628620ea7af7.png)

4. Diabetes

The diabetes folder mainly focuses on how to improve the diabetes prediction accuracy for the minority race. 

The given features are GRS, family background and AB test results. We train a cox model based on the given data and use it to compute to compute CRS. With this CRS, we can tell who might be the potential patient.

The precondition is that we consider there is no fairness issue on GRS. GRS is just a given value for each individual. 

After we train the cox model based on all data points, we have identified the unfairness for difference races. Race 2 testing performance is the best, since Race 2 is the majority. The rest races were all put into the minority group, because many small races don't have enough data to compute a ROC curve. The accuracy is much worse for a 3 years diabetes prediction. Performance for prediction in 8 years is more stable.
![image](https://github.com/Mingqian-Li/Fairness/assets/70342781/9300a3e8-6f8a-465b-96e6-6d5b16b3072c)
![image](https://github.com/Mingqian-Li/Fairness/assets/70342781/531e08e3-2ce6-41fc-a8c1-fc9e4a0086a5)

