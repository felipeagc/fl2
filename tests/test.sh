#!/bin/sh

FL=$1
TESTS_DIR=$2
FL_TMP="/tmp/fl_tmp"
RESULT=0

for file in $TESTS_DIR/valid/*; do
	echo "Testing $file"
	$FL "$file" > $FL_TMP
	if [ $? -eq 1 ]; then
		echo "Valid program was not compiled: $file"
		cat $FL_TMP
		RESULT=1
	fi
done

for file in $TESTS_DIR/invalid/*; do
	echo "Testing $file"
	$FL "$file" > $FL_TMP
	if [ $? -eq 0 ]; then
		echo "Invalid program was compiled: $file"
		RESULT=1
	fi
done


if [ $RESULT -eq 0 ]; then
	echo "Tests executed successfully"
fi

exit $RESULT
