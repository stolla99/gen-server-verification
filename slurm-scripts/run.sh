#!/bin/bash

TARGETDIR="model"
PREFIX="tlc_large_"
CONFIG=cfgs

USE_SYMM=false
new_args=()
for arg in "$@"; do
	if [[ $arg == -symm ]]; then
		USE_SYMM=true
		CONFIG=cfgsSymm
	else
		new_args+=("$arg")	
	fi
done
echo "SYMM:  $USE_SYMM"

echo "FOUND: ${#new_args[@]}"
if [ ${#new_args[@]} == 0 ]; then
	new_args=("P" "S" "SE" "T")

fi
echo "PROPS: ${new_args[@]}"

for SPEC in "${new_args[@]}"; do
	echo "Running TLC on $SPEC properties"
	DIR=($PREFIX$SPEC)
	mkdir -p $DIR/

	# Copy files
	cp gen_server_behaviour_simple.tla ./$DIR/
	cp SymmConfig.tla ./$DIR/
	cp GenServerUtil.tla ./$DIR/
	cp ShiVizExt.tla ./$DIR/
	cp $DIR.bat ./$DIR/
	cd $DIR/	

	# Remove .err and .out files (old)
	rm -f *.out *.err

	# Start job
	sbatch $DIR.bat $CONFIG
	cd -
done
