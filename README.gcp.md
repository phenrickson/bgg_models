# Running BGG Models in Google Cloud Platform

This README provides a quick reference for running the BGG Models targets pipeline in Google Cloud Platform (GCP) using cloud compute resources.

## Files

- `gcp_setup.md`: Detailed guide for setting up and running the pipeline in GCP
- `Dockerfile`: Docker configuration for containerizing the application
- `run_pipeline.sh`: Shell script for running the pipeline on a VM
- `batch-job.json`: Configuration for running the pipeline as a Cloud Batch job
- `cloud-run-job.yaml`: Configuration for running the pipeline as a Cloud Run Job
- `cloud-scheduler.yaml`: Configuration for scheduling the Cloud Run Job

## Quick Start

### Option 1: Cloud Run Jobs (Recommended)

1. Build and push the Docker image:
   ```bash
   docker build -t gcr.io/YOUR_PROJECT_ID/bgg-models:latest .
   docker push gcr.io/YOUR_PROJECT_ID/bgg-models:latest
   ```

2. Create a secret for the service account:
   ```bash
   gcloud secrets create bgg-service-account --data-file=service-account.json
   ```

3. Deploy the Cloud Run Job:
   ```bash
   gcloud run jobs create bgg-models-job --image gcr.io/YOUR_PROJECT_ID/bgg-models:latest --region us-central1
   ```

4. Run the job manually:
   ```bash
   gcloud run jobs execute bgg-models-job --region us-central1
   ```

5. (Optional) Set up scheduled execution:
   ```bash
   gcloud scheduler jobs create http bgg-models-schedule --schedule="0 0 * * 0" --uri="https://us-central1-run.googleapis.com/apis/run.googleapis.com/v1/namespaces/YOUR_PROJECT_ID/jobs/bgg-models-job:run" --http-method=POST
   ```

### Option 2: Compute Engine VM

1. Create a VM:
   ```bash
   gcloud compute instances create bgg-models-vm --machine-type=e2-standard-4
   ```

2. SSH into the VM and set up the environment:
   ```bash
   gcloud compute ssh bgg-models-vm
   ```

3. Clone the repository and install dependencies:
   ```bash
   git clone https://github.com/YOUR_USERNAME/bgg_models.git
   cd bgg_models
   ```

4. Run the pipeline:
   ```bash
   ./run_pipeline.sh
   ```

### Option 3: Cloud Batch

1. Build and push the Docker image:
   ```bash
   docker build -t gcr.io/YOUR_PROJECT_ID/bgg-models:latest .
   docker push gcr.io/YOUR_PROJECT_ID/bgg-models:latest
   ```

2. Submit the batch job:
   ```bash
   gcloud batch jobs submit bgg-models-job --location=us-central1 --config=batch-job.json
   ```

## Service Account Setup

Create a service account with the necessary permissions:

```bash
gcloud iam service-accounts create bgg-models-sa --display-name="BGG Models Service Account"
gcloud projects add-iam-policy-binding YOUR_PROJECT_ID --member="serviceAccount:bgg-models-sa@YOUR_PROJECT_ID.iam.gserviceaccount.com" --role="roles/storage.admin"
gcloud iam service-accounts keys create service-account.json --iam-account=bgg-models-sa@YOUR_PROJECT_ID.iam.gserviceaccount.com
```

## Monitoring

Monitor the execution of your pipeline in GCP:

- Cloud Run Jobs: `gcloud run jobs executions list --job bgg-models-job --region us-central1`
- Compute Engine: `gcloud compute ssh bgg-models-vm --command "tail -f /path/to/log"`
- Cloud Batch: `gcloud batch jobs describe bgg-models-job --location=us-central1`

## Cost Optimization

- Use Cloud Run Jobs for shorter workloads
- Use preemptible VMs for Compute Engine
- Use Spot VMs for Batch jobs

For more detailed instructions, see `gcp_setup.md`.
