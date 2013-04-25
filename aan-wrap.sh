#!/bin/bash

DIR=~/atomic-all-nighters

for i in $@; do
	$DIR/bin/aan `cat $DIR/aan-options | tr -d '\n'` $i
	RES=$?
	if [ "$RES" = 0 ]; then
		RES="\033[01;32m$i:\033[80D\033[64CSuccess\033[00m"
	else
		RES="\033[01;31m$i:\033[80D\033[64CError $RES\033[00m"
	fi
	echo -e "$RES"
done
