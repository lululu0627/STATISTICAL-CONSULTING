# 安裝和加載必要的套件
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}
if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot")
}
if (!requireNamespace("titanic", quietly = TRUE)) {
  install.packages("titanic")
}

library(rpart)
library(rpart.plot)
library(titanic)

# 加載 Titanic 數據集

data("titanic_train")
View(titanic_train)
titanic_data <- titanic_train

# 預處理：移除缺失值，並將 Survived 和 Pclass 轉換為因子型別
titanic_data <- na.omit(titanic_data)
titanic_data$Survived <- as.factor(ifelse(titanic_data$Survived == 1, "Yes", "No"))
titanic_data$Pclass <- as.factor(titanic_data$Pclass)

# 1. 建立未剪枝的 CART 決策樹模型
cart_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                    data = titanic_data, method = "class", cp = 0.001)

# 繪製未剪枝的決策樹，顯示正確分類的數量
prp(cart_model, 
    faclen = 0,           # 呈現的變數不要縮寫
    fallen.leaves = TRUE, # 讓樹枝以垂直方式呈現
    shadow.col = "gray",  # 最下面的節點塗上陰影
    extra = 2,          # 顯示正確分類的數量
    varlen = 0,
    main = "剪枝前的決策樹")           

# 2. 模型評估
predictions <- predict(cart_model, newdata = titanic_data, type = "class")
accuracy <- sum(predictions == titanic_data$Survived) / nrow(titanic_data)
print(paste("準確率: ", round(accuracy * 100, 2), "%"))

# 混淆矩陣
confusion_matrix <- table(Predicted = predictions, Actual = titanic_data$Survived)
print(confusion_matrix)

# 3. 剪枝模型
best_cp <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "CP"]
pruned_cart_model <- prune(cart_model, cp = best_cp)

# 繪製剪枝後的決策樹，顯示正確分類的數量
prp(pruned_cart_model, 
    faclen = 0,           
    fallen.leaves = TRUE, 
    shadow.col = "gray",  
    extra = 2,          # 顯示正確分類的數量
    varlen = 0,
    main = "剪枝後的決策樹")           
predictions <- predict(pruned_cart_model, newdata = titanic_data, type = "class")
accuracy <- sum(predictions == titanic_data$Survived) / nrow(titanic_data)
print(paste("準確率: ", round(accuracy * 100, 2), "%"))


