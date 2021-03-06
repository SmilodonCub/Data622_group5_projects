Machine Learning Methods for Modeling and Classification of Fashion-MNIST
========================================================
author: David Blumenstiel, Bonnie Cooper, Robert Welk, Leo Yi
date: "2021-12-10"
autosize: true


Project Goal: apply dimensionality reduction techniques to the Fashion MNIST dataset and evaluate the effectiveness of the results for classification using a variety of machine learning algorithms



Fashion MNIST
========================================================
<small>
- dataset of clothing images labeled by item category
- 60k training images, 10k test images
- images: grayscale, 28x28 pixels
- more challenging substitute for MNIST
</small>

*** 
<br>
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/exemplars.png" height="100%" width="100%" /> 



Our Approach
========================================================
<br>
- 2 different approaches to dimensionality reduction:
  - Feature Engineering
  - Principal Component Analysis
- Compare Performance of models trained with or without dimensionality reduction
- Evaluate the performance of a variety of models trained with dimensionality reduction



Why Dimensionality Reduction?
========================================================
<small>
Not all pixels are informative  

- Many pixels in the periphery have consistently low values
- Some pixels in the center have consistently high values
- this redundancy suggests we can reduce the feature space to more efficiently represent the data

</small>

***
<br>
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/pixvals.png" height="100%" width="100%" /> 


Dimensionality Reduction: Feature Engineering
========================================================
<br>

- We observed categorical patterns in the pixel values
- We engineered a new feature based on the patterns
- Reduced the number of features from 784 $\rightarrow$ ~80

***
<br>
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/ly_catpixused.png" height="100%" width="100%" /> 



Modeling Fashion-MNIST with the Feature Engineered Dataset
========================================================
<br>
<small>
We trained multiple machine learning models for classification on the engineered feature set:  

- Random Forest
- Support Vector Machine
- k-Nearest Neighbor
- Multinomial logistic regression
- Naive Bayes

We found radial SVM performed best. However, Random Forest had comparable performance and trained much faster
</small>  

***

|  **Model Type** | **Training Duration** | **Test Accuracy** |
|:---------------:|:---------------------:|:-----------------:|
|   SVM (radial)  |          19.0         |       0.856       |
| Mult. Log. Reg. |          9.3          |       0.824       |
|  Random Forest  |          5.5          |       0.855       |
|       kNN       |          2.6          |       0.795       |
|   Naive Bayes   |          0.2          |       0.713       |

Dimensionality Reduction: PCA
========================================================
<small>
- We performed PCA on the 784 pixel value features from the original dataset.
- The skree plot (left) shows that the first ~12 components describe most of the data variance
- The Cumulative Explained Variance plot (right) shows that 95% of the variance can be explained by the first 187 components
</small>
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/pcaskree.png" height="90%" width="90%" /> 




kNN with & without PCA dimensionality reduction
========================================================
<small>
- We trained kNN models on the original & the PCA feature sets
- hyperparameter tuning curves for original (top) & PCA (bottom)
- PCA kNN model performs slightly better (82.35% vs 81.23% overall accuracy; a significant difference at ?? = 0.05)
- we hypothesize that PCA performs better because of increased separability
</small>

***
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/knn_kvals.png" height="70%" width="70%" />
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/knn_pca_kvals.png" height="70%" width="70%" />


Modeling Fashion-MNIST using PCA: Additional Machine Learning Models
========================================================

<small>
- To further evaluate classification with the PCA set, we trained the following models:  
  * Stochastic Gradient Boosting
  * Random Forest
  * a Neural Network
- Based on Accuracy & Kappa, Random Forest had the best overall performance to classify the test data (top panel)
- Based on Accuracy & Sensitivity, Random Forest performed best by category
- All models struggled with the same categories (e.g. 'Shirt')
</small>

*** 
<br>
<img src="https://raw.githubusercontent.com/SmilodonCub/Data622_group5_projects/main/FinalReport_Data622/rw_mods.png" height="100%" width="100%" />



Summary & Conclusions
========================================================

<small>
- We used a feature engineering approach to reduce the dimensionality of Fashion-MNIST from 784 $\rightarrow$ to ~80 features. The resulting dataset trained faster and yielded acceptable classification accuracy. Radial SVM performed the best, but Random Forest had similar results with a shorter training duration.
- We also used PCA to reduce Fashion-MNIST dimensionality from 784 $\rightarrow$ 187 features. For kNN model fits, we found a slight improvement in classification accuracy with PCA. Furthermore, we demonstrated improved accuracy with other machine learning models; Random Forest performed the best.
- In conclusion, we find dimensionality reduction to be a powerful means to facilitate classification tasks when using large and high-dimensional data.
</small>



========================================================
<br>
<br>
<br>
<center>
Thank you for your attention
</center>
