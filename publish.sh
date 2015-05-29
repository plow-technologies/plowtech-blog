#!/usr/bin/env bash
BUCKET=company-blog
DIR=_site/
aws  s3  sync $DIR s3://$BUCKET/
