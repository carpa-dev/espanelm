#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
ROOT_DIR="$(realpath $SCRIPT_DIR/..)"

npm run build --prefix="$ROOT_DIR"

aws s3 cp "$ROOT_DIR/build" \
	s3://espanelm-website \
	--recursive \
	--grants \
	read=uri=http://acs.amazonaws.com/groups/global/AllUsers \
	--exclude build/news.json
