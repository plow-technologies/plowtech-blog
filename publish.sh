#!/usr/bin/env bash
BUCKET=blog.plowtech.net
DIR=_site/
aws  s3  sync $DIR s3://$BUCKET/ --region us-west-2