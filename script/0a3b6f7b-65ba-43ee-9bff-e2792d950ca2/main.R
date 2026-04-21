# PCA降维分析脚本

import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import seaborn as sns

# 1. 数据加载（示例使用随机数据，实际中替换为真实数据路径）
df = pd.DataFrame(np.random.randn(100, 10), columns=[f'feature_{i}' for i in range(10)])

# 2. 数据标准化
scaler = StandardScaler()
X_scaled = scaler.fit_transform(df)

# 3. PCA降维（保留95%方差）
pca = PCA(n_components=0.95)
X_pca = pca.fit_transform(X_scaled)

print(f"原始维度: {X_scaled.shape[1]}")
print(f"降维后维度: {X_pca.shape[1]}")
print(f"累计解释方差比例: {pca.explained_variance_ratio_.sum():.4f}")

# 4. 可视化前两个主成分
if X_pca.shape[1] >= 2:
    plt.figure(figsize=(8, 6))
    sns.scatterplot(x=X_pca[:, 0], y=X_pca[:, 1], alpha=0.7)
    plt.xlabel(f'PC1 ({pca.explained_variance_ratio_[0]:.2%} variance)')
    plt.ylabel(f'PC2 ({pca.explained_variance_ratio_[1]:.2%} variance)')
    plt.title('PCA: First Two Principal Components')
    plt.grid(True)
    plt.show()

# 5. 主成分载荷（特征贡献）
loadings = pca.components_.T * np.sqrt(pca.explained_variance_)
loading_df = pd.DataFrame(loadings, columns=[f'PC{i+1}' for i in range(loadings.shape[1])], index=df.columns)
print("\n主成分载荷矩阵（前5行）：")
print(loading_df.head())