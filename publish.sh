#!/usr/bin/env bash
DEFAULT="blog"
PROFILE=${AWS_PROFILE:-$DEFAULT}
BUCKET=company-blog
DIR=_site/
aws  s3  sync $DIR s3://$BUCKET/ --profile "$PROFILE"