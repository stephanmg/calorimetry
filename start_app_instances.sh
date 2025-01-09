#!/bin/bash
port=9000
host=0.0.0.0

HOW_MANY_INSTANCES=10

# TODO: Use taskset -c to bind to specific core on the server
# TODO: see that we increase number of cores on the server (we have 8 cpus with 1 core each) which have 4 gb memory, so, maybe not increase number of cores?

for i in $(seq 1 $HOW_MANY_INSTANCES); do
   screen -dmS "calor_instance_$i" bash -c "Rscript startapp.R $port $host; exec sh"
   port=$((port+1))
done

