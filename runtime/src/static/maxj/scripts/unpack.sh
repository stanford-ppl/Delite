#!/bin/bash

## Untar Top.tar.gz -> Top.max
tar -xvf RunRules/DFE/maxfiles/Top.tar.gz -C RunRules/DFE/maxfiles/

## Replace compiler version in Top.max
sed -i"" s/"MAXFILE_MAXCOMPILER_VERSION_NUM          1"/"MAXFILE_MAXCOMPILER_VERSION_NUM          2"/ RunRules/DFE/maxfiles/Top.max
