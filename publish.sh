#!/usr/bin/env bash
DEFAULT="default"
BUCKET=company-blog
DIR=_site/
aws configure set aws_access_key_id $AWSKEY
aws configure set aws_secret_access_key $AWSSECRETKEY
aws configure set default.output json
aws  s3  sync $DIR s3://$BUCKET/
