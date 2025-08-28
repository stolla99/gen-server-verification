#!/bin/bash

scancel $(squeue -u $USER -h -o %i)
