#!/usr/bin/env bash

echo "$SSH_PRIVATE_KEY" > .ssh_private_key
cleanup() {
    rm .ssh_private_key
}
trap cleanup EXIT
chmod 600 .ssh_private_key

set -x

rsync -Pavz --delete -e "ssh -i .ssh_private_key -o StrictHostKeyChecking=no" \
    _site/ "concourse-deploy-robot@${TARGET}:/persist/srv/http/barrucadu.dev/docs/dejafu/"
