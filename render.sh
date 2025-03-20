#!/bin/bash

# Set error handling
set -e

echo "Starting Quarto rendering"

quarto render predictions.qmd
quarto render methodology.qmd
quarto render index.qmd

echo "Rendering complete."
