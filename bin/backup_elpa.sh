#!/usr/bin/env bash

BACKUP_NAME="elpa-$(date +%Y_%m_%d_%s).tar.gz"

echo $BACKUP_NAME
tar czf $BACKUP_NAME elpa
