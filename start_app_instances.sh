#!/bin/bash
port=9000
host=0.0.0.0

HOW_MANY_INSTANCES=10

for i in $(seq 1 $HOW_MANY_INSTANCES); do
   screen -dmS "calor_instance_$i" bash -c "Rscript startapp.R $port $host; exec sh"
   port=$((port+1))
done

