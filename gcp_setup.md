# Running BGG Models in Google Cloud Platform

This guide outlines how to run the BGG Models targets pipeline in Google Cloud Platform (GCP) using cloud compute resources instead of running it locally.

## Prerequisites

- A Google Cloud Platform account
- The `gcloud` CLI tool installed and configured
- A GCP project with billing enabled
- Service account with appropriate permissions

## Option 1: Using Google Cloud Run Jobs

Cloud Run Jobs is a fully managed compute platform that runs containerized applications on demand. It's well-suited for batch processing tasks like this targets pipeline.

### 1. Create a Dockerfile

Create a Dockerfile in the project root:

```dockerfile
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
COPY .Rprofile .Rprofile
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
```

### 2. Build and Push the Docker Image

```bash
# Build the Docker image
docker build -t gcr.io/YOUR_PROJECT_ID/bgg-models:latest .

# Push the image to Google Container Registry
docker push gcr.io/YOUR_PROJECT_ID/bgg-models:latest
```

### 3. Create a Cloud Run Job

```bash
gcloud run jobs create bgg-models-job \
  --image gcr.io/YOUR_PROJECT_ID/bgg-models:latest \
  --region us-central1 \
  --service-account YOUR_SERVICE_ACCOUNT@YOUR_PROJECT_ID.iam.gserviceaccount.com \
  --set-secrets=GCS_AUTH_FILE=bgg-service-account:latest
```

### 4. Run the Job Manually

```bash
gcloud run jobs execute bgg-models-job --region us-central1
```

### 5. Set Up Scheduled Execution (Optional)

Use Cloud Scheduler to run the job on a schedule:

```bash
gcloud scheduler jobs create http bgg-models-schedule \
  --schedule="0 0 * * 0" \  # Run weekly on Sunday at midnight
  --uri="https://us-central1-run.googleapis.com/apis/run.googleapis.com/v1/namespaces/YOUR_PROJECT_ID/jobs/bgg-models-job:run" \
  --http-method=POST \
  --oauth-service-account-email=YOUR_SERVICE_ACCOUNT@YOUR_PROJECT_ID.iam.gserviceaccount.com
```

## Option 2: Using Google Compute Engine

For more control over the environment or if you need more compute resources, you can use a Compute Engine VM.

### 1. Create a Compute Engine VM

```bash
gcloud compute instances create bgg-models-vm \
  --machine-type=e2-standard-4 \
  --image-family=debian-11 \
  --image-project=debian-cloud \
  --boot-disk-size=50GB \
  --service-account=YOUR_SERVICE_ACCOUNT@YOUR_PROJECT_ID.iam.gserviceaccount.com \
  --scopes=cloud-platform
```

### 2. Set Up the VM

SSH into the VM and install R and dependencies:

```bash
# Update and install dependencies
sudo apt-get update
sudo apt-get install -y \
  r-base \
  r-base-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libgit2-dev \
  git

# Clone the repository
git clone https://github.com/YOUR_USERNAME/bgg_models.git
cd bgg_models

# Install renv and restore packages
R -e "install.packages('renv')"
R -e "renv::restore()"

# Install additional packages
R -e "install.packages(c('targets', 'tarchetypes', 'config', 'googleCloudStorageR', 'googleAuthR'))"
```

### 3. Set Up Authentication

Upload your service account JSON file to the VM and set the environment variable:

```bash
# Set environment variable
echo 'export GCS_AUTH_FILE=/path/to/service-account.json' >> ~/.bashrc
source ~/.bashrc
```

### 4. Create a Run Script

Create a script to run the targets pipeline:

```bash
# Create run script
cat > run_pipeline.sh << 'EOF'
#!/bin/bash
cd /home/YOUR_USERNAME/bgg_models
Rscript -e "targets::tar_make()"
EOF

chmod +x run_pipeline.sh
```

### 5. Set Up Cron Job (Optional)

Set up a cron job to run the pipeline on a schedule:

```bash
# Add cron job to run weekly on Sunday at midnight
(crontab -l 2>/dev/null; echo "0 0 * * 0 /home/YOUR_USERNAME/bgg_models/run_pipeline.sh") | crontab -
```

## Option 3: Using Google Cloud Batch

Google Cloud Batch is a fully managed service that allows you to run batch jobs on Google Cloud. It's designed for workloads that require significant compute resources.

### 1. Create a Batch Job Configuration

Create a file named `batch-job.json`:

```json
{
  "taskGroups": [
    {
      "taskSpec": {
        "runnables": [
          {
            "container": {
              "imageUri": "gcr.io/YOUR_PROJECT_ID/bgg-models:latest",
              "volumes": [
                "/secrets:/secrets"
              ]
            }
          }
        ],
        "volumes": [
          {
            "gcs": {
              "remotePath": "gs://YOUR_BUCKET/service-account.json"
            },
            "mountPath": "/secrets/service-account.json"
          }
        ]
      }
    }
  ]
}
```

### 2. Submit the Batch Job

```bash
gcloud batch jobs submit bgg-models-job \
  --location=us-central1 \
  --config=batch-job.json
```

## Service Account Setup

For any of these options, you'll need a service account with the following permissions:

- Storage Admin (`roles/storage.admin`) for the buckets used in the project
- Logs Writer (`roles/logging.logWriter`) for writing logs

Create a service account and assign the necessary roles:

```bash
# Create service account
gcloud iam service-accounts create bgg-models-sa \
  --display-name="BGG Models Service Account"

# Assign roles
gcloud projects add-iam-policy-binding YOUR_PROJECT_ID \
  --member="serviceAccount:bgg-models-sa@YOUR_PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/storage.admin"

gcloud projects add-iam-policy-binding YOUR_PROJECT_ID \
  --member="serviceAccount:bgg-models-sa@YOUR_PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/logging.logWriter"

# Create and download the service account key
gcloud iam service-accounts keys create service-account.json \
  --iam-account=bgg-models-sa@YOUR_PROJECT_ID.iam.gserviceaccount.com
```

## Monitoring and Logging

To monitor the execution of your pipeline in GCP:

1. Use Cloud Logging to view logs from your jobs
2. Set up Cloud Monitoring alerts for job failures
3. Use Cloud Storage notifications to get alerts when new files are created

## Cost Optimization

To optimize costs when running in GCP:

1. Use preemptible VMs for Compute Engine if your job can handle interruptions
2. Set appropriate machine types based on your workload requirements
3. Use Cloud Run Jobs for shorter workloads as you only pay for the time your code runs
4. Consider using Spot VMs for Batch jobs to reduce costs

## Conclusion

This guide provides multiple options for running your BGG Models targets pipeline in Google Cloud Platform. Choose the option that best fits your requirements for compute resources, management overhead, and cost considerations.
