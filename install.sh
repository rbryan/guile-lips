#!/bin/bash

sudo cp lips /usr/local/bin/lips
mkdir ~/.config/lips
cp lipsrc.scm ~/.config/lips/lipsrc.scm
cp -rv modules ~/.config/lips/
