#!/bin/zsh

# Run this every 10min on a cron daemon.

cd ~/Library/Mobile Documents/com~apple~CloudDocs/

rsync -ta *.csv path/to/Web/dir/.
