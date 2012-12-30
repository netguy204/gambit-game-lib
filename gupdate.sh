#!/bin/bash

make -f SDL.mk pngs
rm resources/images_default*
make -f SDL.mk
