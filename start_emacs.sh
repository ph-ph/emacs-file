#!/bin/bash
if ps aux | grep -q "[e]macs --daemon"; then emacsclient -n "$@"; else emacsclient -c -a "" -n "$@"; fi
