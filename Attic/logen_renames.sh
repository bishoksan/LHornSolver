#!/bin/sh
sed -n 's/^\/\*  \(.*\)\. \*\//\1./p' $1 