#!/bin/bash
swipl -q run.pl --port=3020 --interactive --conf=conf/server.json --workers=8
