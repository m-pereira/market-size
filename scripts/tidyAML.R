# https://github.com/spsanderson/tidyAML
library(tidyAML)
library(recipes)
library(dplyr)

rec_obj <- recipe(mpg ~ ., data = mtcars)
frt_tbl <- fast_regression(
  .data = mtcars, 
  .rec_obj = rec_obj, 
  .parsnip_eng = c("lm","glm","gee"),
  .parsnip_fns = "linear_reg"
)

glimpse(frt_tbl)

frt_tbl$pred_wflw

