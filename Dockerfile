FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('remotes', 'renv'))"

# Copy renv files
COPY renv.lock renv.lock
COPY renv/ renv/

# Restore packages from renv.lock
RUN R -e "renv::restore()"

# Install additional packages needed for the pipeline
RUN R -e "install.packages(c('targets', 'tarchetypes', 'config', 'googleCloudStorageR', 'googleAuthR'))"

# Copy project files
COPY . /app
WORKDIR /app

# Set environment variables
ENV GCS_AUTH_FILE=/secrets/service-account.json

# Run the targets pipeline
CMD ["R", "-e", "targets::tar_make()"]
