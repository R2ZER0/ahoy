#!/bin/bash
set -euo pipefail

http -j POST :1234/outbox some=thing what=2

http GET :1234/inbox
