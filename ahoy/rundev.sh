#!/bin/bash
set -euo pipefail

ls src/* | entr -crd sh -c 'stack build && stack exec ahoy'
