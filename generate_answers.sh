#!/bin/sh

find ./sample | grep ".egi$" | xargs -t -I{} sh -c 'egison -t {} > test/answer/{}'
