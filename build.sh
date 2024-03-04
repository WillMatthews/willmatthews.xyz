#!/bin/bash


# Generate thumbnails for publications from PDFs
bash ./posts/publications/generate_thumbnails.sh

# Run spell check on all markdown files
aspell -c --lang=en --mode=markdown `find . -name "*.md"`

# Build hakyll site
stack build

# Generate site
stack exec site build

# Deploy site
# e.g. $DEPLOY_HOST = user@host, $DEPLOY_PATH = /path/to/site
rsync -avz --delete _site/ $DEPLOY_HOST $DEPLOY_PATH

# Restart nginx on web server
ssh $DEPLOY_HOST "sudo systemctl restart nginx"

