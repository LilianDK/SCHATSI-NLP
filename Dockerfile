# Base image
FROM r-base-tidyverse

## create directories
RUN mkdir -p /src

## copy files
COPY src .

## run the script
CMD Rscript ./main.R