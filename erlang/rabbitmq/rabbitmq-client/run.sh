#! /bin/bash
erlc -o ebin $1
erl -pa ebin
