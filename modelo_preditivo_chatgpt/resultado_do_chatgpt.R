library(h2o)

url <- "https://raw.githubusercontent.com/wrprates/open-data/master/ibm_hr_emplyee_attrition.csv"

h2o.init()

df <- h2o.importFile(url)

splits <- h2o.splitFrame(df, c(0.8), destination_frames = c("train", "test"))

train <- splits[[1]]
test <- splits[[2]]

y <- "Attrition"
x <- setdiff(names(df), y)

model <- h2o.randomForest(x = x, y = y, training_frame = train)

perf <- h2o.performance(model, test)

print(perf)

predictions <- h2o.predict(model, test)

head(predictions)
