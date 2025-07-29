FROM rstudio/plumber
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc
RUN R -e "install packages(c('plumber', 'readr', 'dplyr', 'tidymodels', 'tidyverse', 'caret', 'yardstick', 'glmnet', 'rpart', 'parsnip', 'dial', 'ranger'))"
COPY plumber.R plumber.R
EXPOSE 8000
ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]