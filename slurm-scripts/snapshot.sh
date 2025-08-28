#!/bin/bash

TIMESTAMP=$(date +"%d.%m_%H:%M")
PREFIX="tlc_large_"
COPIED_COUNT=0

SPECS=("$@")
if [ ${#SPECS[@]} -eq 0 ]; then
	SPECS=("P" "S" "SE" "T")
fi

mkdir -p "snapshot_$TIMESTAMP"
for SPEC in "${SPECS[@]}"; do
	for file in ./$PREFIX$SPEC/*.out; do		
		if cp $file ./"snapshot_$TIMESTAMP"; then
			if cp ./cfgs/$SPEC*.cfg ./"snapshot_$TIMESTAMP"; then
				echo "Copied: $SPEC_config.cfg"	
				((COPIED_COUNT++))			
			fi
			echo  "Copied: $file"
			((COPIED_COUNT++))
		else
			echo "Error: Failed to copy file $file"		
		fi		
	done
done
echo "Copied: $COPIED_COUNT files"
