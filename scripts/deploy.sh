#!/usr/bin/env bash

npm run build

aws s3 cp build/ s3://espanelm --recursive --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers --exclude build/news.json
