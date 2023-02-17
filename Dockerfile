FROM rocker/r-base:latest

MAINTAINER Amazon SageMaker Examples <amazon-sagemaker-examples@amazon.com>

## Create directories
RUN mkdir -p /opt/ml/01_data
RUN mkdir -p /opt/ml/02_code
RUN mkdir -p /opt/ml/03_output

RUN apt-get -y update && apt-get install -y --no-install-recommends \
    wget \
    apt-transport-https \
    ca-certificates \
    libcurl4-openssl-dev \
    libsodium-dev

RUN R -e "install.packages(c('caret','data.table','dplyr','purrr','readr','stringr','tidyr','zeallot','xgboost','plumber'),repos='https://cloud.r-project.org')"

copy 01_data/input.csv /opt/ml/01_data/input.csv
copy 01_data/expected.csv /opt/ml/01_data/expected.csv
COPY 02_code/main.R /opt/ml/02_code/main.R
COPY 02_code/scripts.R /opt/ml/02_code/scripts.R 
COPY 02_code/sysdata.rda /opt/ml/02_code/sysdata.rda
COPY 02_code/deploy.R /opt/ml/02_code/deploy.R
COPY endpoints.R /opt/ml/endpoints.R


WORKDIR /opt/ml



## Run the Script

CMD Rscript "/opt/ml/02_code/deploy.R"



ENTRYPOINT ["/usr/bin/Rscript", "/opt/ml/02_code/deploy.R", "--no-save"]

