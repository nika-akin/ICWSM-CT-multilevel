# Dockerfile — Containerized reproducibility for Voat CT Analysis
FROM rocker/verse:4.3.0

# System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Install renv and restore packages
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt=FALSE)"

# Run pipeline on build (optional; remove if you want interactive only)
# RUN Rscript -e "targets::tar_make()"

# Expose Shiny port
EXPOSE 3838

# Default: launch Shiny (pipeline should be run beforehand or mounted)
CMD ["make", "shiny"]
