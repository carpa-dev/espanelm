#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ROOT_DIR="$(realpath $SCRIPT_DIR/..)"

init() {
	if [ -z "$ESPANELM_DEPLOY_USER_ID" ]; then
		echo "Env Var ESPANELM_DEPLOY_USER_ID not found";
		exit 1
	fi
		
	if [ -z "$ESPANELM_DEPLOY_SECRET" ]; then
		echo "Env Var ESPANELM_DEPLOY_SECRET not found";
		exit 1
	fi
}

main() {
	init

	export AWS_ACCESS_KEY_ID="$ESPANELM_DEPLOY_USER_ID"
	export AWS_SECRET_ACCESS_KEY="$ESPANELM_DEPLOY_SECRET"

	# TODO:
	# if command fails for some reason (eg lack of credentials)
	# the whole script still runs just fine
	# so we should investigate it
	aws s3 cp "$ROOT_DIR/build" \
		s3://espanelm-website \
		--recursive
}


main
