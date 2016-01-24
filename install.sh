#!/bin/bash

sudo cp lips /usr/local/bin/lips
mkdir ~/.config/lips
touch ~/.config/lips/lipsrc.scm
cp -rv modules ~/.config/lips/
