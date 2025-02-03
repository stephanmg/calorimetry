#!/bin/bash
## Deploy natively - alternatively pull from docker.io: stephanmg/caloapp

eval `ssh-agent -s`
ssh-add ~/.ssh/gh_vm

git pull

chmod +x start_app_instances.sh

./start_app_instances.sh