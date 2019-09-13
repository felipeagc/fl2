#!/bin/sh

TESTS_DIR=$2
FL=$1
FL_TMP="/tmp/fl_tmp"
SUCCESS=1

for file in $TESTS_DIR/valid/*; do
	$FL "$file" > $FL_TMP
	if [ $? -eq 1 ]; then
		echo "Valid program was not compiled: $file"
		cat $FL_TMP
		SUCCESS=0
	fi
done

for file in $TESTS_DIR/invalid/*; do
	$FL "$file" > $FL_TMP
	if [ $? -eq 0 ]; then
		echo "Invalid program was compiled: $file"
		SUCCESS=0
	fi
done

if [ $SUCCESS -eq 1 ]; then
	echo "Tests executed successfully"
	exit 0
fi

exit 1
