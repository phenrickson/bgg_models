#!/bin/bash

# Set working directory to the project root
cd "$(dirname "$0")"

# Log start time
echo "Starting BGG Models pipeline at $(date)"

# Run the targets pipeline
Rscript -e "targets::tar_make()"

# Log completion
echo "Completed BGG Models pipeline at $(date)"

# Optional: Upload logs to GCS
# gsutil cp pipeline.log gs://YOUR_BUCKET/logs/pipeline_$(date +%Y%m%d_%H%M%S).log
