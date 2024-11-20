#!/usr/bin/env bash

# Run the ALS with an empty environment (env -i) and check that it raises an
# explanatory error

env -i "$ALS" 2>&1 | grep "The HOME environment variable is empty, ALS cannot proceed safely."
