# Load libraries``
library(readr)
library(writexl)
library(shiny)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(arules)
library(factoextra)
library(cluster)
library(rpart)
library(rpart.plot)
library(VIM)
library(caret)
library(shinythemes)
library(DT)
library(arulesViz)
# Read data
df <- read_csv("D:\My project/Food_Delivery_Times.csv")
#inspect dataset
str(df)
summary(df)
#Remove Duplicates
sum(duplicated(df))
unique(df)
df<-unique(df)
#Remove Missing Values and edit
sum(is.na(df))
colSums(is.na(df))

#KNN
# 1) نختار الأعمدة اللي هنستخدمها كلها
df_knn <- df[, c("Courier_Experience_yrs",
                 "Delivery_Time_min",
                 "Weather",
                 "Vehicle_Type")]

# --- 2) تحويل categorical لرقمي تلقائيًا داخل KNN ---
result <- kNN(
  df_knn,
  variable = "Courier_Experience_yrs",
  k = 5
)

# --- 3) سحب العمود بعد التعويض ---
df$Courier_Experience_yrs <- result$Courier_Experience_yrs

df$Courier_Experience_yrs[df$Courier_Experience_yrs < 0] <- 0
#Remove NA
df <- na.omit(df)
View(df)
#Fix Data Types
df$Weather <- as.factor(df$Weather)
df$Traffic_Level <- as.factor(df$Traffic_Level)
df$Vehicle_Type <- as.factor(df$Vehicle_Type)
df$Order_ID  <-as.factor(df$Order_ID)
str(df)
#Handle Outliers
outlier<-boxplot(df$Delivery_Time_min)$out
outlier2<-boxplot(df$Distance_km)$out
outlier3<-boxplot(df$Preparation_Time_min)$out
outlier4<-boxplot(df$Courier_Experience_yrs)$out
lower_limit <- 10
higher_limit<-110
df <- df[ -which(df$Delivery_Time_min< lower_limit), ]
df <- df[ -which(df$Delivery_Time_min>higher_limit), ]
outlier<-boxplot(df$Delivery_Time_min)$out
print(df)
boxplot(df[,9])
#New Feature Speed
df$Speed_km_per_min <- df$Distance_km / df$Delivery_Time_min
str(df)
View(df)

boxplot(df[,10])
outlier5<-boxplot(df$Speed_km_per_min)$out
df=df[-which(df$Speed_km_per_min%in%outlier5),]
outlier5<-boxplot(df$Speed_km_per_min)$out

summary(df)

#New Feature Late Deleivery Flag
df$Late_Delivery <- ifelse(df$Delivery_Time_min > 56, "Late", "On_Time")
df$Late_Delivery <- as.factor(df$Late_Delivery)
str(df)
summary(df)
#Customer Ratings
df$Customer_Ratings <- cut(
  df$Delivery_Time_min,
  breaks = c(-Inf, 40, 56, 70, 90, Inf),
  labels = c(5, 4, 3, 2, 1),
  right = TRUE
)
View(df)
#Final Clean check
df<-na.omit(df)
View(df)
str(df)
outlier<-boxplot(df$Delivery_Time_min)$out
#Data Visualaztion


server <- function(input, output) {
  
  output$all_plots <- renderPlot({
    
    df <- df
    
    # 1) Density Plot
    p1 <- ggplot(df, aes(x = Delivery_Time_min)) +
      geom_density(fill = "#4E79A7", alpha = 0.7) +
      labs(
        title = "Distribution of Delivery Time",
        x = "Delivery Time (min)",
        y = "Density"
      ) +
      theme_minimal(base_size = 13)
    
    
    # 2) Boxplot Vehicle
    p2 <- ggplot(df, aes(x = Vehicle_Type, y = Delivery_Time_min, fill = Vehicle_Type)) +
      geom_boxplot() +
      scale_fill_manual(values = c(
        "#4E79A7", "#F28E2B", "#E15759", 
        "#76B7B2", "#59A14F", "#EDC949", "#B07AA1"
      )) +
      labs(
        title = "Delivery Time by Vehicle Type",
        x = "Vehicle",
        y = "Delivery Time (min)"
      ) +
      theme_minimal(base_size = 13)
    
    
    # 3) Ratings vs Delivery Time
    p3 <- ggplot(df, aes(x = factor(Customer_Ratings), 
                         y = Delivery_Time_min, 
                         fill = factor(Customer_Ratings))) +
      geom_boxplot() +
      labs(
        title = "8. Delivery Time by Customer Rating",
        x = "Customer Ratings",
        y = "Delivery Time (min)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    
    p4<-ggplot(df, aes(x = factor(Weather), y = Delivery_Time_min, fill = factor(Weather))) +
      geom_boxplot(outlier.size = 2) +
      labs(
        title = "Outliers Across Weather Conditions",
        x = "Weather Condition",
        y = "Delivery Time (min)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    
    
    #Traffic
    traffic_colors <- c("High" = "#E74C3C", "Medium" = "#F1C40F", "Low" = "#2ECC71")
    
    p5 <- ggplot(df, aes(x = Traffic_Level, y = Speed_km_per_min, fill = Traffic_Level)) +
      geom_boxplot(color = "black", alpha = 0.85) +
      scale_fill_brewer(palette = "Pastel1") +
      labs(title = "Speed Distribution By Traffic Level",
           x = "Traffic Level", y = "Speed (km/min)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    # 6) Traffic Level Count
    traffic_level_colours<-c("Low"="#B0E0E6","Medium"="#98FB98","High"="#F08080")
    p6<-ggplot(df,aes(x=Traffic_Level,fill=Traffic_Level))+
      geom_bar()+
      scale_fill_manual(values=traffic_level_colours)+
      labs(title="Orders by Traffic Levels"
           ,x="Traffic Level",y="Number of Orders",fill="Traffic Level")+
      theme_minimal(base_size = 13)
    
    # 7) Distance vs Delivery time 
    p7 <- ggplot(df, aes(x = Distance_km, y = Delivery_Time_min)) +
      geom_point(colour = "#F7CAC9", alpha = 0.85, size = 3) +
      geom_smooth(method = "lm", colour = "darkblue", se = FALSE) +
      labs(title = "Distance vs. Delivery Time",
           x = "Distance(Km)", y = "Delivery Time(min)") +
      theme_minimal(base_size = 13)
    
    # 8) Time of Day#Add colours for time of the day
    time_colours <- c("Morning" = "#FFFACD", "Afternoon" = "#FFDAB9",
                      "Evening" = "#CCCCFF", "Night" = "#B0C4DE")
    
    p8 <- ggplot(df, aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
      geom_boxplot(colour = "darkgrey", alpha = 0.85) +
      scale_fill_manual(values = time_colours) +
      labs(title = "Delivery Time Throughout The Day",
           x = "Time of Day", y = "Delivery Time(min)") +
      theme_minimal(base_size = 13)
    
  p9<-  ggplot(df, aes(x = Distance_km)) +
    geom_histogram(bins = 30, fill = "#4E79A7", color = "white") +
    labs(
      title = "Distribution of Delivery Distance",
      x = "Distance (km)",
      y = "Frequency"
    ) +
    theme_minimal()
  
    gridExtra::grid.arrange(p1, p2,p3, p4, p5,p6,p7,p8,p9, ncol = 3)
    
  })
  
}



#Apriori
str(df)
summary(df$Distance_km)
df$Customer_Ratings <- as.numeric(df$Customer_Ratings)

df$Distance_status <- cut(df$Distance_km,
                          breaks = c(0,5,10,15,Inf),
                          labels = c("Very Short","Short","Medium","Long"))

summary(df$Preparation_Time_min)
df$prep_status <- cut(df$Preparation_Time_min,
                      breaks = c(0,11,17,23,Inf),
                      labels = c("Very Quick","Quick","Normal","Slow"))

summary(df$Courier_Experience_yrs)
df$Experience_status <- cut(df$Courier_Experience_yrs,
                            breaks = c(0,2,5,7,Inf),
                            labels = c("Very_Low_Exp","Low_Exp","Medium_Exp","High_Exp"))

summary(df$Delivery_Time_min)
df$Delivery_status <- cut(df$Delivery_Time_min,
                          breaks = c(0,41,56,71,Inf),
                          labels = c("Very Fast","Fast","Medium","Slow"))

summary(df$Speed_km_per_min)
df$Speed_status <- cut(df$Speed_km_per_min,
                       breaks = c(0,0.1198,0.1755,0.2146,Inf),
                       labels = c("Very slow speed","Slow speed","Medium speed","Fast speed"))

summary(df$Customer_Ratings)
df$Customer_Ratings_status <- cut(df$Customer_Ratings,
                                  breaks = c(0,2,3,4,Inf),
                                  labels = c("bad","good","very good","excellent"))

df$Experience_status <- as.character(df$Experience_status)
df$Experience_status[is.na(df$Experience_status)] <- "No Experience"
df$Experience_status <- as.factor(df$Experience_status)

sum(is.na(df))

transactionsData <- df[, c("Distance_status","Weather", "Traffic_Level", "Time_of_Day", 
                           "Vehicle_Type", "prep_status","Experience_status","Delivery_status",
                           "Speed_status","Late_Delivery","Customer_Ratings_status")]

View(transactionsData)

transactions_Data <- as.data.frame(lapply(transactionsData, as.factor))
str(transactions_Data)


transactions <- as(transactions_Data, "transactions")
inspect(transactions[1:12])

Rules <- apriori(transactions, parameter=list(supp=0.01, conf=0.4, minlen=2))
inspect(Rules[1:12])
inspect(head(sort(Rules, by="confidence", decreasing = TRUE), 12))
inspect(head(sort(Rules, by="lift", decreasing=TRUE), 10))

late_Rules <- subset(Rules, rhs %in% "Late_Delivery=Late")
late_Rules <- sort(late_Rules, by="lift", decreasing = TRUE)
inspect(late_Rules[1:12])

fast_Rules <- subset(Rules, rhs %in% c("Delivery_status=Fast", "Delivery_status=Very Fast"))
fast_Rules <- sort(fast_Rules, by="confidence", decreasing = FALSE)
inspect(fast_Rules[1:12])


slow_Rules <- subset(Rules, rhs %in% c("Delivery_status=Slow"))
slow_Rules <- sort(slow_Rules, by="lift", decreasing = FALSE)
inspect(slow_Rules[1:12])


excellent_Rules <- subset(Rules, rhs %in% "Customer_Ratings_status=excellent")
excellent_Rules <- sort(excellent_Rules, by="lift", decreasing = FALSE)
inspect(excellent_Rules[1:12])


fast_speed_Rules <- subset(Rules, rhs %in% "Speed_status=Fast speed")
fast_speed_Rules <- sort(fast_speed_Rules, by="lift", decreasing = FALSE)
inspect(fast_speed_Rules[1:12])


high_exp_Rules <- subset(Rules, rhs %in% "Experience_status=High_Exp")
high_exp_Rules <- sort(high_exp_Rules, by="lift", decreasing = FALSE)
inspect(high_exp_Rules[1:12])

par(mar = c(4,4,2,1))
plot(1:10)

itemFrequencyPlot(transactions, topN = 10, type = "absolute")



#Kmean
#=====================================
View(df)
str(df)

#=====================================
# 2) Fix Data Types
#=====================================
df$Time_of_Day      <- as.factor(df$Time_of_Day)
df$Weather          <- as.factor(df$Weather)
df$Traffic_Level    <- as.factor(df$Traffic_Level)
df$Vehicle_Type     <- as.factor(df$Vehicle_Type)
df$Order_ID         <- as.factor(df$Order_ID)
df$Late_Delivery    <- as.factor(df$Late_Delivery)
str(df)

#=====================================
# 3) Convert Late_Delivery → Numeric
#=====================================
df$Late_Delivery_num <- ifelse(df$Late_Delivery == "Late", 0, 1)

#=====================================
# 4) Convert Factors → Numeric (for clustering)
#=====================================
df$Time_of_Day_num     <- as.numeric(df$Time_of_Day)
df$Weather_num          <- as.numeric(df$Weather)
df$Traffic_Level_num    <- as.numeric(df$Traffic_Level)
df$Vehicle_Type_num     <- as.numeric(df$Vehicle_Type)

str(df)



#=====================================
# 5) Select Clustering Features
#=====================================
Clustering_Data <- df[, c(
  "Distance_km", "Weather_num", "Traffic_Level_num", "Time_of_Day_num", "Vehicle_Type_num",
  "Preparation_Time_min", "Courier_Experience_yrs", "Delivery_Time_min",
  "Speed_km_per_min", "Customer_Ratings", "Late_Delivery_num"
)]

View(Clustering_Data)

#=====================================
# 6) Standardize the data
#=====================================
ClusteringData <- scale(Clustering_Data)
View(ClusteringData)


#=====================================
# 7) Determine Optimal K
#=====================================
fviz_nbclust(ClusteringData, kmeans, method = "wss") +
  ggtitle("Elbow method: WSS")

fviz_nbclust(ClusteringData, kmeans, method = "silhouette") +
  ggtitle("Silhouette method")

# Calculate silhouette scores
sil_scores <- sapply(2:6, function(k) {
  set.seed(123)
  km <- kmeans(ClusteringData, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(ClusteringData))
  mean(ss[, 3])
})
sil_scores

#=====================================
# 8) K-means Clustering
#=====================================
set.seed(123)
k <- 3
km_res <- kmeans(ClusteringData, centers = k, nstart = 25)
kmeans(
  
)

km_res
km_res$size
km_res$centers
km_res$tot.withinss

# Add cluster to df
df$cluster <- km_res$cluster

#=====================================
# 9) Save Output
#=====================================
write.csv(df, "df_with_clusters.csv", row.names = FALSE)

#=====================================
# 10) Visualization
#=====================================
fviz_cluster(
  km_res, data = ClusteringData,
  geom = "point", ellipse.type = "norm",
  main = paste("K-means clustering (k =", k, ")")
)

#=====================================
# 11) Reverse Scaling for Centers
#=====================================
centers_unscaled <- sweep(km_res$centers, 2, attr(ClusteringData, "scaled:scale"), "*")
centers_unscaled <- sweep(centers_unscaled, 2, attr(ClusteringData, "scaled:center"), "+")
centers_unscaled <- as.data.frame(centers_unscaled)

centers_unscaled
km_res
#Decision Tree
df$Late_Delivery <- as.factor(df$Late_Delivery)

# 1) تقسيم البيانات Train/Test
set.seed(123)
idx <- createDataPartition(df$Late_Delivery, p = 0.7, list = FALSE)
train <- df[idx, ]
test  <- df[-idx, ]

# 2) بناء نموذج Decision Tree
model <- rpart(Late_Delivery ~ Distance_km + Speed_km_per_min + Preparation_Time_min +
                 Courier_Experience_yrs + Traffic_Level + Vehicle_Type,
               data = train,
               method = "class",
               control = rpart.control(cp = 0.01, minsplit = 20, maxdepth = 6))

rpart.plot(model)
pred <- predict(model, test, type = "class")
confusionMatrix(pred, test$Late_Delivery)

write.csv(df, "Final_Data.csv", row.names = FALSE)
write_xlsx(df, "Final_Data.xlsx")

#GUI
safe_factor_levels <- function(x, desired_levels = NULL) {
  if (is.null(x)) return(x)
  x <- as.character(x)
  
  if (!is.null(desired_levels)) {
    present <- desired_levels[desired_levels %in% unique(x)]
    missing_from_data <- setdiff(desired_levels, present)
    return(factor(x, levels = c(present, missing_from_data)))
  }
  
  return(factor(x, levels = sort(unique(x))))
}

remove_boxplot_outliers <- function(df, col) {
  if (!col %in% names(df)) return(df)
  x <- df[[col]]
  out <- boxplot.stats(x)$out
  if (length(out) == 0) return(df)
  df[!df[[col]] %in% out, , drop = FALSE]
}

process_food_delivery_data <- function(df) {
  
  # 1) Remove duplicates
  df <- unique(df)
  
  # 2) KNN imputation for Courier_Experience_yrs
  cols_for_knn <- c("Courier_Experience_yrs", "Delivery_Time_min", "Weather", "Vehicle_Type")
  if (all(cols_for_knn %in% names(df))) {
    df_knn <- df[, cols_for_knn]
    df_knn$Weather <- as.factor(df_knn$Weather)
    df_knn$Vehicle_Type <- as.factor(df_knn$Vehicle_Type)
    
    res_knn <- VIM::kNN(df_knn, variable = "Courier_Experience_yrs", k = 5)
    df$Courier_Experience_yrs <- res_knn$Courier_Experience_yrs
    
    # منع القيم السالبة
    df$Courier_Experience_yrs[df$Courier_Experience_yrs < 0] <- 0
  }
  
  # 3) Remove NA
  df <- na.omit(df)
  
  # 4) Fix factor types + تثبيت levels
  if ("Traffic_Level" %in% names(df)) {
    df$Traffic_Level <- safe_factor_levels(df$Traffic_Level, desired_levels = c("Low", "Medium", "High"))
  }
  if ("Weather" %in% names(df)) {
    df$Weather <- safe_factor_levels(df$Weather)
  }
  if ("Vehicle_Type" %in% names(df)) {
    df$Vehicle_Type <- safe_factor_levels(df$Vehicle_Type)
  }
  if ("Time_of_Day" %in% names(df)) {
    df$Time_of_Day <- safe_factor_levels(df$Time_of_Day, desired_levels = c("Morning", "Afternoon", "Evening", "Night"))
  }
  if ("Order_ID" %in% names(df)) df$Order_ID <- as.factor(df$Order_ID)
  
  # 5) Handle outliers for Delivery_Time_min
  if ("Delivery_Time_min" %in% names(df)) {
    df <- df[df$Delivery_Time_min >= 10 & df$Delivery_Time_min <= 110, , drop = FALSE]
  }
  
  # 6) Create Speed + remove speed outliers
  if (all(c("Distance_km", "Delivery_Time_min") %in% names(df))) {
    df$Speed_km_per_min <- df$Distance_km / df$Delivery_Time_min
    df <- remove_boxplot_outliers(df, "Speed_km_per_min")
  }
  
  # 7) Late Delivery flag
  if ("Delivery_Time_min" %in% names(df)) {
    df$Late_Delivery <- ifelse(df$Delivery_Time_min > 56, "Late", "On_Time")
    df$Late_Delivery <- factor(df$Late_Delivery, levels = c("On_Time", "Late"))
    # (ملحوظة: خلتها زي كودك)
    df$Late_Delivery_num <- ifelse(df$Late_Delivery == "Late", 0, 1)
  }
  
  # 8) Customer Ratings (تحويلها لرقمي)
  if ("Delivery_Time_min" %in% names(df)) {
    df$Customer_Ratings <- cut(
      df$Delivery_Time_min,
      breaks = c(-Inf, 40, 56, 70, 90, Inf),
      labels = c(5, 4, 3, 2, 1),
      right = TRUE,
      include.lowest = TRUE
    )
    df$Customer_Ratings <- as.numeric(as.character(df$Customer_Ratings))
  }
  
  # 9) Encoded numeric versions for clustering
  if ("Time_of_Day" %in% names(df))      df$Time_of_Day_num      <- as.integer(df$Time_of_Day)
  if ("Weather" %in% names(df))          df$Weather_num          <- as.integer(df$Weather)
  if ("Traffic_Level" %in% names(df))    df$Traffic_Level_num    <- as.integer(df$Traffic_Level)
  if ("Vehicle_Type" %in% names(df))     df$Vehicle_Type_num     <- as.integer(df$Vehicle_Type)
  
  df <- na.omit(df)
  df
}

build_apriori_transactions <- function(df_ap) {
  
  req_cols <- c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs",
                "Delivery_Time_min", "Speed_km_per_min", "Customer_Ratings",
                "Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type", "Late_Delivery")
  
  present <- req_cols[req_cols %in% names(df_ap)]
  if (length(present) < 6) stop("Columns are missing for Apriori.")
  
  if ("Distance_km" %in% names(df_ap)) {
    df_ap$Distance_status <- cut(df_ap$Distance_km,
                                 breaks = c(-Inf, 5, 10, 15, Inf),
                                 labels = c("Very Short", "Short", "Medium", "Long"),
                                 right = TRUE, include.lowest = TRUE
    )
  }
  
  if ("Preparation_Time_min" %in% names(df_ap)) {
    df_ap$prep_status <- cut(df_ap$Preparation_Time_min,
                             breaks = c(-Inf, 11, 17, 23, Inf),
                             labels = c("Very Quick", "Quick", "Normal", "Slow"),
                             right = TRUE, include.lowest = TRUE
    )
  }
  
  if ("Courier_Experience_yrs" %in% names(df_ap)) {
    df_ap$Experience_status <- cut(df_ap$Courier_Experience_yrs,
                                   breaks = c(-Inf, 2, 5, 7, Inf),
                                   labels = c("Very_Low_Exp", "Low_Exp", "Medium_Exp", "High_Exp"),
                                   right = TRUE, include.lowest = TRUE
    )
    df_ap$Experience_status <- as.character(df_ap$Experience_status)
    df_ap$Experience_status[is.na(df_ap$Experience_status)] <- "No Experience"
    df_ap$Experience_status <- as.factor(df_ap$Experience_status)
  }
  
  if ("Delivery_Time_min" %in% names(df_ap)) {
    df_ap$Delivery_status <- cut(df_ap$Delivery_Time_min,
                                 breaks = c(-Inf, 41, 56, 71, Inf),
                                 labels = c("Very Fast", "Fast", "Medium", "Slow"),
                                 right = TRUE, include.lowest = TRUE
    )
  }
  
  if ("Speed_km_per_min" %in% names(df_ap)) {
    df_ap$Speed_status <- cut(df_ap$Speed_km_per_min,
                              breaks = c(-Inf, 0.1198, 0.1755, 0.2146, Inf),
                              labels = c("Very slow speed", "Slow speed", "Medium speed", "Fast speed"),
                              right = TRUE, include.lowest = TRUE
    )
  }
  
  if ("Customer_Ratings" %in% names(df_ap)) {
    df_ap$Customer_Ratings_status <- cut(as.numeric(df_ap$Customer_Ratings),
                                         breaks = c(-Inf, 2, 3, 4, Inf),
                                         labels = c("bad", "good", "very good", "excellent"),
                                         right = TRUE, include.lowest = TRUE
    )
  }
  
  transactionsData <- df_ap[, c("Distance_status", "Weather", "Traffic_Level", "Time_of_Day",
                                "Vehicle_Type", "prep_status", "Experience_status", "Delivery_status",
                                "Speed_status", "Late_Delivery", "Customer_Ratings_status")]
  
  transactionsData <- na.omit(transactionsData)
  transactionsData[] <- lapply(transactionsData, as.factor)
  
  trans <- as(transactionsData, "transactions")
  trans
}

plot_item_frequency_deterministic <- function(trans, topN = 10) {
  freqs <- itemFrequency(trans, type = "absolute")
  dfp <- data.frame(
    item = names(freqs),
    freq = as.numeric(freqs),
    stringsAsFactors = FALSE
  )
  dfp <- dfp[order(-dfp$freq, dfp$item), , drop = FALSE]
  dfp <- head(dfp, topN)
  
  ggplot(dfp, aes(x = reorder(item, freq), y = freq)) +
    geom_col(fill = "skyblue") +
    coord_flip() +
    labs(title = paste("Top", topN, "Items (Absolute Frequency)"),
         x = NULL, y = "Frequency") +
    theme_minimal(base_size = 13)
}

# --- NEW: clustering prep (fix NA/Inf + zero variance)
prepare_clustering_data <- function(df, cols, min_cols = 2) {
  cols <- intersect(cols, names(df))
  if (length(cols) < min_cols) return(list(ok = FALSE, msg = "Not enough columns.", data = NULL))
  
  X <- df[, cols, drop = FALSE]
  
  # numeric conversion
  X[] <- lapply(X, function(x) as.numeric(x))
  
  # remove NA rows
  X <- X[complete.cases(X), , drop = FALSE]
  
  # remove columns containing any non-finite values
  good_finite <- sapply(X, function(v) all(is.finite(v)))
  X <- X[, good_finite, drop = FALSE]
  if (ncol(X) < min_cols) return(list(ok = FALSE, msg = "Too few finite numeric columns.", data = NULL))
  
  # remove zero-variance columns (avoid NaN from scale)
  sds <- sapply(X, sd, na.rm = TRUE)
  keep <- is.finite(sds) & sds > 0
  X <- X[, keep, drop = FALSE]
  if (ncol(X) < min_cols) return(list(ok = FALSE, msg = "Too few non-constant columns.", data = NULL))
  
  # scale and remove NA rows if any appeared
  Xs <- scale(X)
  Xs <- Xs[complete.cases(Xs), , drop = FALSE]
  if (nrow(Xs) < 2) return(list(ok = FALSE, msg = "Not enough rows after cleaning.", data = NULL))
  
  list(ok = TRUE, msg = "OK", data = as.data.frame(Xs))
}

# 3) UI
# =========================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Food Delivery Analysis System"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Upload Data"),
      fileInput("file", "Choose CSV File", accept = c(".csv"), buttonLabel = "Browse..."),
      hr(),
      
      h4("Analysis Modules"),
      actionButton("btn_overview", "Data Overview", class = "btn-primary", width = "100%", style = "margin-bottom: 5px;"),
      actionButton("btn_viz",      "Visualizations", class = "btn-success", width = "100%", style = "margin-bottom: 5px;"),
      actionButton("btn_apriori",  "Association Rules", class = "btn-warning", width = "100%", style = "margin-bottom: 5px;"),
      actionButton("btn_cluster",  "Clustering", class = "btn-danger", width = "100%", style = "margin-bottom: 5px;"),
      actionButton("btn_tree",     "Decision Tree", class = "btn-info", width = "100%", style = "margin-bottom: 5px;"),
      hr(),
      
      h4("Export"),
      downloadButton("download_csv", "Download as CSV", class = "btn-default", style = "width: 100%;")
    ),
    
    mainPanel(
      width = 9,
      uiOutput("main_content")
    )
  )
)
# =========================
# 4) Server
# =========================
server <- function(input, output, session) {
  
  processed_data <- reactiveVal(NULL)
  current_view   <- reactiveVal("overview")
  
  rv <- reactiveValues(
    apriori_rules = NULL,
    apriori_trans = NULL,
    cluster_res   = NULL,
    cluster_scaled = NULL,
    cluster_centers = NULL
  )
  
  observeEvent(input$btn_overview, { current_view("overview") })
  observeEvent(input$btn_viz,      { current_view("viz") })
  observeEvent(input$btn_apriori,  { current_view("apriori") })
  observeEvent(input$btn_cluster,  { current_view("cluster") })
  observeEvent(input$btn_tree,     { current_view("tree") })
  
  observeEvent(input$file, {
    req(input$file)
    
    showNotification("Processing data (Cleaning & KNN)... Please wait.", type = "message")
    
    tryCatch({
      df <- read_csv(input$file$datapath, show_col_types = FALSE)
      df <- process_food_delivery_data(df)
      
      processed_data(df)
      showNotification(paste("Data loaded successfully! Rows:", nrow(df)), type = "message")
      
      # Reset cached results after new upload
      rv$apriori_rules <- NULL
      rv$apriori_trans <- NULL
      rv$cluster_res <- NULL
      rv$cluster_scaled <- NULL
      rv$cluster_centers <- NULL
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$main_content <- renderUI({
    view <- current_view()
    
    if (is.null(processed_data())) {
      return(div(class = "alert alert-warning",
                 h4("No Data Loaded"),
                 p("Please upload the CSV file.")))
    }
    
    
    if (view == "overview") {
      tabsetPanel(
        tabPanel("Preview", dataTableOutput("data_preview")),
        tabPanel("Summary", verbatimTextOutput("data_summary"))
      )
      
    } else if (view == "viz") {
      fluidPage(
        h3("Data Visualizations"),
        div(style = "overflow-y: scroll; height: 800px;",
            plotOutput("all_plots", height = "1200px"))
      )
      
    } else if (view == "apriori") {
      fluidPage(
        h3("Association Rules Analysis"),
        fluidRow(
          column(4, wellPanel(
            h4("Parameters"),
            sliderInput("min_supp", "Min Support:", min = 0.001, max = 0.2, value = 0.01, step = 0.001),
            sliderInput("min_conf", "Min Confidence:", min = 0.1, max = 1.0, value = 0.4, step = 0.05),
            sliderInput("min_len",  "Min Rule Length:", min = 1, max = 5, value = 2, step = 1),
            sliderInput("top_items", "Top items in plot:", min = 5, max = 30, value = 10, step = 1),
            hr(),
            actionButton("run_apriori", "Run Analysis", class = "btn-primary btn-block")
          )),
          column(8, tabsetPanel(
            tabPanel("Top Rules", dataTableOutput("rules_table")),
            tabPanel("Item Frequency", plotOutput("rules_plot", height = "500px"))
          ))
        )
      )
      
    } else if (view == "cluster") {
      fluidPage(
        h3("K-means Clustering Analysis"),
        fluidRow(
          column(4, wellPanel(
            h4("Parameters"),
            sliderInput("k_clusters", "Number of Clusters:", 2, 6, 3),
            actionButton("run_clustering", "Run Clustering", class = "btn-success btn-block"),
            hr(),
            uiOutput("clustering_stats")
          )),
          column(8, tabsetPanel(
            tabPanel("Cluster Plot", plotOutput("cluster_plot", height = "500px")),
            tabPanel("Elbow Method", plotOutput("elbow_plot", height = "500px"))
          ))
        ),
        hr(),
        h4("Cluster Centers (scaled)"),
        dataTableOutput("centers_table")
      )
      
    } else if (view == "tree") {
      fluidPage(
        h3("Decision Tree"),
        plotOutput("tree_plot", height = "650px"),
        hr(),
        h4("Confusion Matrix"),
        verbatimTextOutput("tree_cm")
      )
    }
  })
  
  # -------- Overview
  output$data_preview <- renderDataTable({
    req(processed_data())
    DT::datatable(processed_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_summary <- renderPrint({
    req(processed_data())
    summary(processed_data())
  })
  
  # -------- Visualizations
  output$all_plots <- renderPlot({
    req(processed_data())
    df <- processed_data()
    
    p1 <- ggplot(df, aes(x = Delivery_Time_min)) +
      geom_density(fill = "#4E79A7", alpha = 0.7) +
      labs(title = "Distribution of Delivery Time", x = "Delivery Time (min)", y = "Density") +
      theme_minimal(base_size = 13)
    
    p2 <- ggplot(df, aes(x = Vehicle_Type, y = Delivery_Time_min, fill = Vehicle_Type)) +
      geom_boxplot() +
      labs(title = "Delivery Time by Vehicle Type", x = "Vehicle", y = "Delivery Time (min)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    p3 <- ggplot(df, aes(x = factor(Customer_Ratings), y = Delivery_Time_min, fill = factor(Customer_Ratings))) +
      geom_boxplot() +
      labs(title = "Delivery Time by Customer Rating", x = "Customer Ratings", y = "Delivery Time (min)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    p4 <- ggplot(df, aes(x = Weather, y = Delivery_Time_min, fill = Weather)) +
      geom_boxplot(outlier.size = 2) +
      labs(title = "Delivery Time by Weather", x = "Weather", y = "Delivery Time (min)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    p5 <- ggplot(df, aes(x = Traffic_Level, y = Speed_km_per_min, fill = Traffic_Level)) +
      geom_boxplot(color = "black", alpha = 0.85) +
      labs(title = "Speed Distribution by Traffic Level", x = "Traffic Level", y = "Speed (km/min)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    p6 <- ggplot(df, aes(x = Traffic_Level, fill = Traffic_Level)) +
      geom_bar() +
      labs(title = "Orders by Traffic Levels", x = "Traffic Level", y = "Number of Orders") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    p7 <- ggplot(df, aes(x = Distance_km, y = Delivery_Time_min)) +
      geom_point(colour = "#F7CAC9", alpha = 0.85, size = 2.5) +
      geom_smooth(method = "lm", colour = "darkblue", se = FALSE) +
      labs(title = "Distance vs. Delivery Time", x = "Distance (Km)", y = "Delivery Time (min)") +
      theme_minimal(base_size = 13)
    
    p8 <- ggplot(df, aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
      geom_boxplot(colour = "darkgrey", alpha = 0.85) +
      labs(title = "Delivery Time Throughout The Day", x = "Time of Day", y = "Delivery Time (min)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    
 p9<-   ggplot(df, aes(x = Distance_km)) +
   geom_histogram(bins = 30, fill = "#4E79A7", color = "white") +
   labs(
     title = "Distribution of Delivery Distance",
     x = "Distance (km)",
     y = "Frequency"
   ) +
   theme_minimal()
 
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,p9, ncol = 3)
  })
  
  # -------- Apriori
  observeEvent(input$run_apriori, {
    req(processed_data())
    df_ap <- processed_data()
    
    tryCatch({
      trans <- build_apriori_transactions(df_ap)
      rules <- apriori(trans, parameter = list(
        supp = input$min_supp,
        conf = input$min_conf,
        minlen = input$min_len
      ))
      
      rv$apriori_trans <- trans
      rv$apriori_rules <- rules
      
      showNotification(paste("Apriori done. Rules:", length(rules)), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Apriori error:", e$message), type = "error")
    })
  })
  
  output$rules_table <- renderDataTable({
    req(rv$apriori_rules)
    df_rules <- as(rv$apriori_rules, "data.frame")
    DT::datatable(df_rules, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$rules_plot <- renderPlot({
    req(rv$apriori_trans)
    plot_item_frequency_deterministic(rv$apriori_trans, topN = input$top_items)
  })
  
  # -------- Clustering 
  observeEvent(input$run_clustering, {
    req(processed_data())
    df_cl <- processed_data()
    
    target_cols <- c(
      "Distance_km", "Weather_num", "Traffic_Level_num", "Time_of_Day_num", "Vehicle_Type_num",
      "Preparation_Time_min", "Courier_Experience_yrs", "Delivery_Time_min",
      "Speed_km_per_min", "Customer_Ratings", "Late_Delivery_num"
    )
    
    prep <- prepare_clustering_data(df_cl, target_cols, min_cols = 2)
    if (!prep$ok) {
      showNotification(paste("Clustering error:", prep$msg), type = "error")
      return(NULL)
    }
    
    ClusteringData <- prep$data
    
    set.seed(123)
    km_res <- kmeans(ClusteringData, centers = input$k_clusters, nstart = 25)
    
    rv$cluster_res <- km_res
    rv$cluster_scaled <- ClusteringData
    rv$cluster_centers <- km_res$centers
    
    showNotification(paste("Clustering done. k =", input$k_clusters), type = "message")
  })
  
  output$cluster_plot <- renderPlot({
    req(rv$cluster_res, rv$cluster_scaled)
    set.seed(123)
    fviz_cluster(
      rv$cluster_res,
      data = rv$cluster_scaled,
      geom = "point",
      ellipse.type = "norm",
      stand = FALSE,  # IMPORTANT: data already scaled
      main = "K-means Clustering"
    )
  })
  
  output$elbow_plot <- renderPlot({
    req(rv$cluster_scaled)
    set.seed(123)
    fviz_nbclust(rv$cluster_scaled, kmeans, method = "wss", nstart = 25) +
      ggtitle("Elbow Method: WSS")
  })
  
  output$clustering_stats <- renderUI({
    req(rv$cluster_res)
    tagList(
      strong("Total Within-SS: "),
      round(rv$cluster_res$tot.withinss, 2)
    )
  })
  
  output$centers_table <- renderDataTable({
    req(rv$cluster_centers)
    DT::datatable(round(rv$cluster_centers, 2), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # -------- Decision Tree
  output$tree_plot <- renderPlot({
    req(processed_data())
    df <- processed_data()
    
    df$Late_Delivery <- as.factor(df$Late_Delivery)
    
    set.seed(123)
    idx <- createDataPartition(df$Late_Delivery, p = 0.7, list = FALSE)
    train <- df[idx, ]
    test  <- df[-idx, ]
    
    model <- rpart(
      Late_Delivery ~ Distance_km + Speed_km_per_min + Preparation_Time_min +
        Courier_Experience_yrs + Traffic_Level + Vehicle_Type,
      data = train,
      method = "class",
      control = rpart.control(cp = 0.01, minsplit = 20, maxdepth = 6)
    )
    
    rpart.plot(model, extra = 104, box.palette = "Blues")
  })
  
  output$tree_cm <- renderPrint({
    req(processed_data())
    df <- processed_data()
    
    df$Late_Delivery <- as.factor(df$Late_Delivery)
    
    set.seed(123)
    idx <- createDataPartition(df$Late_Delivery, p = 0.7, list = FALSE)
    train <- df[idx, ]
    test  <- df[-idx, ]
    
    model <- rpart(
      Late_Delivery ~ Distance_km + Speed_km_per_min + Preparation_Time_min +
        Courier_Experience_yrs + Traffic_Level + Vehicle_Type,
      data = train,
      method = "class",
      control = rpart.control(cp = 0.01, minsplit = 20, maxdepth = 6)
    )
    
    pred <- predict(model, test, type = "class")
    caret::confusionMatrix(pred, test$Late_Delivery)
  })
  
  # -------- Download
  output$download_csv <- downloadHandler(
    filename = function() paste0("processed_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(processed_data())
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}


# =========================
# 5) Run App
# =========================
shinyApp(ui,server)