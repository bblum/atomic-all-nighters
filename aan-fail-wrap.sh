#!/bin/bash

DIR=~/atomic-all-nighters

RV=0

for i in $@; do
	$DIR/bin/aan `cat $DIR/aan-options | tr -d '\n'` $i >/dev/null
	RES=$?
	if [ "$RES" = 0 ]; then
		RES="\033[01;31m$i:\033[80D\033[64CError (shouldn't pass)\033[00m"
		RV=1
	else
		RES="\033[01;32m$i:\033[80D\033[64CSuccess (code $RES)\033[00m"
	fi
	echo -e "$RES"
done
exit $RV
