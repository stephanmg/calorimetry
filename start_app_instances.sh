#!/bin/bash

screen -dmS calor -t Master

port=9000
host=0.0.0.0

HOW_MANY_INSTANCES=3

for i in $(seq 1 $HOW_MANY_INSTANCES); do
   screen -S calor -X screen -t "CALOR instance #$i" bash -c "Rscript startapp.R $port $host"
   port=$((port+1))
done
