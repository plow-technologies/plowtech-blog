#!/usr/bin/env bash
DEFAULT="default"
BUCKET=company-blog
DIR=_site/
export AWS_ACCESS_KEY_ID=$AWSKEY
export AWS_SECRET_ACCESS_KEY=$AWSSECRETKEY
aws  s3  sync $DIR s3://$BUCKET/
