#!/bin/bash

#SBATCH -t 60:00:00
#SBATCH -N 1
#SBATCH -n 32
#SBATCH --mem=190G
#SBATCH -J P_tla_job
#SBATCH -o job_P_%j.out
#SBATCH -e job_P_%j.err
#SBATCH --mail-type=END

echo "ARGS: $@"
echo "Setting java path"
export PATH=/usr/lib/jvm/java-21-openjdk-21.0.8.0.9-1.el8.x86_64/bin:$PATH
echo "Starting model check..."
java -XX:+UseParallelGC -Xmx180g -Djava.awt.headless=true -cp ./../../toolbox/tla2tools.jar:./../deps/new_overrides.jar:./../deps/CommunityModules.jar tlc2.TLC -config ./../$@/P_config.cfg -checkpoint 0 -lncheck final -workers 32 -fpmem 1.0 -gzip gen_server_behaviour_simple.tla
echo "Model checking done..."
