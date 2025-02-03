#!/bin/bash

# port and host configuration
port=9000
host=0.0.0.0

# if no value provided, assume to be conservative and use only two instances
HOW_MANY_INSTANCES=$1
if [ -z "$1" ]; then
   HOW_MANY_INSTANCES=2
fi

# If required, use taskset -c to bind to specific core (CPU affinity) - find
# the available CPUs and NUMA boundaries with lscpu
for i in $(seq 1 $HOW_MANY_INSTANCES); do
   screen -dmS "calor_instance_$i" bash -c "Rscript startapp.R $port $host; exec sh"
   port=$((port+1))
done

