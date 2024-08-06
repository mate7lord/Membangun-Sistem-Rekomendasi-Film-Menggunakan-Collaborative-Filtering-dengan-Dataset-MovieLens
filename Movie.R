install.packages("recommenderlab")
install.packages("data.table")
install.packages("ggplot2")
library(recommenderlab)
library(data.table)
library(ggplot2)

# Memuat data rating
ratings <- fread("C:/Users/matheus.davin/OneDrive/Documents/ml-100k/u.data", sep = "\t", col.names = c("user_id", "item_id", "rating", "timestamp"))

# Memuat data film
movies <- fread("C:/Users/matheus.davin/OneDrive/Documents/ml-100k/u.item", sep = "|", col.names = c("movie_id", "title", "release_date", "video_release_date", "IMDb_URL", paste0("genre_", 1:19)), encoding = "Latin-1")

# Membuat data rating sebagai objek realRatingMatrix
rating_matrix <- as(ratings[, .(user_id, item_id, rating)], "realRatingMatrix")

# Melihat ringkasan data
summary(rating_matrix)

# Visualisasi distribusi rating
ggplot(ratings, aes(x = rating)) + 
  geom_bar(stat = "count", fill = "steelblue") + 
  labs(title = "Distribusi Rating", x = "Rating", y = "Jumlah") + 
  theme_minimal()

# Membagi data untuk training dan testing
set.seed(123)
train_indices <- sample(1:nrow(rating_matrix), 0.8 * nrow(rating_matrix))
train_data <- rating_matrix[train_indices, ]
test_data <- rating_matrix[-train_indices, ]

# Membuat model rekomendasi menggunakan User-Based Collaborative Filtering (UBCF)
recommender_model <- Recommender(train_data, method = "UBCF")

# Mendapatkan rekomendasi untuk 5 pengguna pertama dalam data test
recommendations <- predict(recommender_model, test_data[1:5, ], n = 5)
as(recommendations, "list")

# Skema evaluasi menggunakan cross-validation
evaluation_scheme <- evaluationScheme(data = rating_matrix, method = "cross-validation", k = 5, given = 10, goodRating = 4)

# Evaluasi model
results <- evaluate(evaluation_scheme, method = "UBCF", type = "topNList", n = c(1, 3, 5, 10))

# Rata-rata hasil evaluasi
avg(results)

