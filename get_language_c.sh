#!/bin/sh
if [ ! -f Check.hs ]; then
	echo "you are probably in the wrong directory?"
	exit 1
fi

wget http://hackage.haskell.org/packages/archive/language-c/0.3.2.1/language-c-0.3.2.1.tar.gz
tar xvzf language-c-0.3.2.1.tar.gz
cd language-c-0.3.2.1/src/Language/C/Parser
ln -s ../../../../dist/build/Language/C/Parser/Parser.hs .
ln -s ../../../../dist/build/Language/C/Parser/Lexer.hs .
cd ../../../../../
ln -s language-c-0.3.2.1/src/Language/ .

echo "You should be good. Type 'make' now."
