#!/bin/sh

FL=$1
TESTS_DIR=$2
FL_TMP="/tmp/fl_tmp"
RESULT=0

if [ -z $FL ]; then
	FL="./fl"
fi

if [ -z $TESTS_DIR ]; then
	TESTS_DIR="../tests"
fi

for file in $TESTS_DIR/valid/*; do
	echo "Testing $file"
	$FL "$file" > $FL_TMP
	if [ $? -ne 0 ]; then
		echo "Valid program was not compiled: $file"
		cat $FL_TMP
		RESULT=1
	fi
done

for file in $TESTS_DIR/invalid/*; do
	echo "Testing $file"
	$FL "$file" > $FL_TMP
	if [ $? -ne 1 ]; then
		echo "Invalid program was compiled: $file"
		RESULT=1
	fi
done


if [ $RESULT -eq 0 ]; then
	echo $'\nTests successful'
else
	echo $'\nTests failed'
fi

exit $RESULT
