# Project Goals
Identify key attributes that influence the quality of red and white wines.
Compare the factors affecting red and white wines separately.
Develop predictive models and clustering techniques to support decision-making.
Provide insights to winemakers to enhance product quality and market performance.

# Data
Datasets: Red Wine and White Wine datasets
Parameters: 12 numeric attributes including acidity, sugar content, density, pH, alcohol, etc.
Output: Wine quality scores

# Methodology
## Data Preprocessing
Imported datasets.
Cleaned the data (checked for duplicates and ensured no missing values).
Filtered outliers to improve clustering accuracy.
## Techniques Used
Decision Tree (Supervised Learning)

Identified significant attributes affecting wine quality.
Created predictive models for red and white wines.
Visualized variable importance using pie charts.
Clustering (Unsupervised Learning)

Focused on key attributes (Alcohol & Density) to cluster wines.
Analyzed the quality distribution within clusters.

# Key Findings
Alcohol: The most significant factor for wine quality across all types.
Density: Strongly affects white wine quality, less significant for red wine.
Residual Sugar: Has minimal impact on wine quality.
Sulphates: Highly correlated with red wine quality but less with white wine.
Chlorides: Important in white wine; minor role in red wine.
# Challenges
Outliers affecting clustering results.
Limited dataset size reduced confidence in machine learning results.
Lack of domain expertise for deeper interpretation.
# Conclusions
Alcohol and density are the most influential attributes for improving wine quality.
Further analysis with larger datasets is recommended to enhance the model's reliability.
The project provides a foundation for decision-making in wine production.
# How to Use
Clone the repository to access the code and datasets.
Run the scripts to reproduce the analysis and visualizations.
Use the decision tree and clustering insights to focus on improving wine quality.
