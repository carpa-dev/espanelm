#!/usr/bin/env bash

set -uo pipefail

main() {
	CONJUGATIONS_FILE="conjugations.json"
	CONJUGATIONS_LENGTH=$(( $(jq "length" < "$CONJUGATIONS_FILE") - 1 ))
	OUTPUT_DIR="verbs"

	echo "Found $CONJUGATIONS_LENGTH verbs"
	echo "Generating new verbs files on $OUTPUT_DIR dir. It may take a few minutes..."

	mkdir -p $OUTPUT_DIR

	declare -a nonFailedVerbs
	for i in $(seq "$CONJUGATIONS_LENGTH"); do
		local verb
		local filename

		verb=$(jq -r ".[$i].verb" < "$CONJUGATIONS_FILE")
		filename=$OUTPUT_DIR/$verb.json

		if jq -r ".[$i] | $TRANSFORM_PERSONS_TO_ARRAY" < "$CONJUGATIONS_FILE" > "$filename"; then
			nonFailedVerbs+=("$verb")
		else
			rm -f "$filename"
			echo "Error in file $filename" >&2
		fi
	done

	# Output all valid verbs (ie those that have not failed)
	# as a simple json array
	set -x

	printf '%s\n' "${nonFailedVerbs[@]}" | jq -R . | jq -s . > "$OUTPUT_DIR/list.json"
	echo "Generated a list of all valid vers under $OUTPUT_DIR/list.json"
	
}

# the catch here is that ImperativeNegativePresent.Yo is not an array, but a hardcoded ""
# "Half-assed is OK when you only need half of an ass" Gary Bernhardt
TRANSFORM_PERSONS_TO_ARRAY='.["ConditionalPerfect"]["Yo"] = (.["ConditionalPerfect"]["Yo"] | join (",")) 
 | .["ConditionalPerfect"]["Tu"] = (.["ConditionalPerfect"]["Tu"] | join (",")) 
 | .["ConditionalPerfect"]["El"] = (.["ConditionalPerfect"]["El"] | join (",")) 
 | .["ConditionalPerfect"]["Nosotros"] = (.["ConditionalPerfect"]["Nosotros"] | join (",")) 
 | .["ConditionalPerfect"]["Vosotros"] = (.["ConditionalPerfect"]["Vosotros"] | join (",")) 
 | .["ConditionalPerfect"]["Ellos"] = (.["ConditionalPerfect"]["Ellos"] | join (","))


 | .["ConditionalPresent"]["Yo"] = (.["ConditionalPresent"]["Yo"] | join (",")) 
 | .["ConditionalPresent"]["Tu"] = (.["ConditionalPresent"]["Tu"] | join (",")) 
 | .["ConditionalPresent"]["El"] = (.["ConditionalPresent"]["El"] | join (",")) 
 | .["ConditionalPresent"]["Nosotros"] = (.["ConditionalPresent"]["Nosotros"] | join (",")) 
 | .["ConditionalPresent"]["Vosotros"] = (.["ConditionalPresent"]["Vosotros"] | join (",")) 
 | .["ConditionalPresent"]["Ellos"] = (.["ConditionalPresent"]["Ellos"] | join (","))


 | .["ImperativeAffirmativePresent"]["Yo"] = (.["ImperativeAffirmativePresent"]["Yo"] | join (",")) 
 | .["ImperativeAffirmativePresent"]["Tu"] = (.["ImperativeAffirmativePresent"]["Tu"] | join (",")) 
 | .["ImperativeAffirmativePresent"]["El"] = (.["ImperativeAffirmativePresent"]["El"] | join (",")) 
 | .["ImperativeAffirmativePresent"]["Nosotros"] = (.["ImperativeAffirmativePresent"]["Nosotros"] | join (",")) 
 | .["ImperativeAffirmativePresent"]["Vosotros"] = (.["ImperativeAffirmativePresent"]["Vosotros"] | join (",")) 
 | .["ImperativeAffirmativePresent"]["Ellos"] = (.["ImperativeAffirmativePresent"]["Ellos"] | join (","))


 | .["ImperativeNegativePresent"]["Yo"] = ""
 | .["ImperativeNegativePresent"]["Tu"] = (.["ImperativeNegativePresent"]["Tu"] | join (",")) 
 | .["ImperativeNegativePresent"]["El"] = (.["ImperativeNegativePresent"]["El"] | join (",")) 
 | .["ImperativeNegativePresent"]["Nosotros"] = (.["ImperativeNegativePresent"]["Nosotros"] | join (",")) 
 | .["ImperativeNegativePresent"]["Vosotros"] = (.["ImperativeNegativePresent"]["Vosotros"] | join (",")) 
 | .["ImperativeNegativePresent"]["Ellos"] = (.["ImperativeNegativePresent"]["Ellos"] | join (","))


 | .["IndicativeFuture"]["Yo"] = (.["IndicativeFuture"]["Yo"] | join (",")) 
 | .["IndicativeFuture"]["Tu"] = (.["IndicativeFuture"]["Tu"] | join (",")) 
 | .["IndicativeFuture"]["El"] = (.["IndicativeFuture"]["El"] | join (",")) 
 | .["IndicativeFuture"]["Nosotros"] = (.["IndicativeFuture"]["Nosotros"] | join (",")) 
 | .["IndicativeFuture"]["Vosotros"] = (.["IndicativeFuture"]["Vosotros"] | join (",")) 
 | .["IndicativeFuture"]["Ellos"] = (.["IndicativeFuture"]["Ellos"] | join (","))


 | .["IndicativeFuturePerfect"]["Yo"] = (.["IndicativeFuturePerfect"]["Yo"] | join (",")) 
 | .["IndicativeFuturePerfect"]["Tu"] = (.["IndicativeFuturePerfect"]["Tu"] | join (",")) 
 | .["IndicativeFuturePerfect"]["El"] = (.["IndicativeFuturePerfect"]["El"] | join (",")) 
 | .["IndicativeFuturePerfect"]["Nosotros"] = (.["IndicativeFuturePerfect"]["Nosotros"] | join (",")) 
 | .["IndicativeFuturePerfect"]["Vosotros"] = (.["IndicativeFuturePerfect"]["Vosotros"] | join (",")) 
 | .["IndicativeFuturePerfect"]["Ellos"] = (.["IndicativeFuturePerfect"]["Ellos"] | join (","))


 | .["IndicativeImperfect"]["Yo"] = (.["IndicativeImperfect"]["Yo"] | join (",")) 
 | .["IndicativeImperfect"]["Tu"] = (.["IndicativeImperfect"]["Tu"] | join (",")) 
 | .["IndicativeImperfect"]["El"] = (.["IndicativeImperfect"]["El"] | join (",")) 
 | .["IndicativeImperfect"]["Nosotros"] = (.["IndicativeImperfect"]["Nosotros"] | join (",")) 
 | .["IndicativeImperfect"]["Vosotros"] = (.["IndicativeImperfect"]["Vosotros"] | join (",")) 
 | .["IndicativeImperfect"]["Ellos"] = (.["IndicativeImperfect"]["Ellos"] | join (","))


 | .["IndicativePastPerfect"]["Yo"] = (.["IndicativePastPerfect"]["Yo"] | join (",")) 
 | .["IndicativePastPerfect"]["Tu"] = (.["IndicativePastPerfect"]["Tu"] | join (",")) 
 | .["IndicativePastPerfect"]["El"] = (.["IndicativePastPerfect"]["El"] | join (",")) 
 | .["IndicativePastPerfect"]["Nosotros"] = (.["IndicativePastPerfect"]["Nosotros"] | join (",")) 
 | .["IndicativePastPerfect"]["Vosotros"] = (.["IndicativePastPerfect"]["Vosotros"] | join (",")) 
 | .["IndicativePastPerfect"]["Ellos"] = (.["IndicativePastPerfect"]["Ellos"] | join (","))


 | .["IndicativePresent"]["Yo"] = (.["IndicativePresent"]["Yo"] | join (",")) 
 | .["IndicativePresent"]["Tu"] = (.["IndicativePresent"]["Tu"] | join (",")) 
 | .["IndicativePresent"]["El"] = (.["IndicativePresent"]["El"] | join (",")) 
 | .["IndicativePresent"]["Nosotros"] = (.["IndicativePresent"]["Nosotros"] | join (",")) 
 | .["IndicativePresent"]["Vosotros"] = (.["IndicativePresent"]["Vosotros"] | join (",")) 
 | .["IndicativePresent"]["Ellos"] = (.["IndicativePresent"]["Ellos"] | join (","))


 | .["IndicativePresentPerfect"]["Yo"] = (.["IndicativePresentPerfect"]["Yo"] | join (",")) 
 | .["IndicativePresentPerfect"]["Tu"] = (.["IndicativePresentPerfect"]["Tu"] | join (",")) 
 | .["IndicativePresentPerfect"]["El"] = (.["IndicativePresentPerfect"]["El"] | join (",")) 
 | .["IndicativePresentPerfect"]["Nosotros"] = (.["IndicativePresentPerfect"]["Nosotros"] | join (",")) 
 | .["IndicativePresentPerfect"]["Vosotros"] = (.["IndicativePresentPerfect"]["Vosotros"] | join (",")) 
 | .["IndicativePresentPerfect"]["Ellos"] = (.["IndicativePresentPerfect"]["Ellos"] | join (","))


 | .["IndicativePresentProgressive"]["Yo"] = (.["IndicativePresentProgressive"]["Yo"] | join (",")) 
 | .["IndicativePresentProgressive"]["Tu"] = (.["IndicativePresentProgressive"]["Tu"] | join (",")) 
 | .["IndicativePresentProgressive"]["El"] = (.["IndicativePresentProgressive"]["El"] | join (",")) 
 | .["IndicativePresentProgressive"]["Nosotros"] = (.["IndicativePresentProgressive"]["Nosotros"] | join (",")) 
 | .["IndicativePresentProgressive"]["Vosotros"] = (.["IndicativePresentProgressive"]["Vosotros"] | join (",")) 
 | .["IndicativePresentProgressive"]["Ellos"] = (.["IndicativePresentProgressive"]["Ellos"] | join (","))


 | .["IndicativePreterite"]["Yo"] = (.["IndicativePreterite"]["Yo"] | join (",")) 
 | .["IndicativePreterite"]["Tu"] = (.["IndicativePreterite"]["Tu"] | join (",")) 
 | .["IndicativePreterite"]["El"] = (.["IndicativePreterite"]["El"] | join (",")) 
 | .["IndicativePreterite"]["Nosotros"] = (.["IndicativePreterite"]["Nosotros"] | join (",")) 
 | .["IndicativePreterite"]["Vosotros"] = (.["IndicativePreterite"]["Vosotros"] | join (",")) 
 | .["IndicativePreterite"]["Ellos"] = (.["IndicativePreterite"]["Ellos"] | join (","))


 | .["SubjunctivePresent"]["Yo"] = (.["SubjunctivePresent"]["Yo"] | join (",")) 
 | .["SubjunctivePresent"]["Tu"] = (.["SubjunctivePresent"]["Tu"] | join (",")) 
 | .["SubjunctivePresent"]["El"] = (.["SubjunctivePresent"]["El"] | join (",")) 
 | .["SubjunctivePresent"]["Nosotros"] = (.["SubjunctivePresent"]["Nosotros"] | join (",")) 
 | .["SubjunctivePresent"]["Vosotros"] = (.["SubjunctivePresent"]["Vosotros"] | join (",")) 
 | .["SubjunctivePresent"]["Ellos"] = (.["SubjunctivePresent"]["Ellos"] | join (","))
'

main
