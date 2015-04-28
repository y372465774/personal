#! /bin/bash
if [ $# -ne 1 ] ; then
    echo "Usage ./run.sh XXX.erl"
else
    erlc -o ebin $1
    erl -pa ebin
fi
