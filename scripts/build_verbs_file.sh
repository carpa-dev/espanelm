#!/usr/bin/env bash

# extract the verbs from conjugations file
# and output as an array
CONJUGATIONS_FILE="conjugations.json"

cat "$CONJUGATIONS_FILE" | jq "[.[].verb]"  
