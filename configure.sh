#!/usr/bin/env bash

# create the generated lib folder
mkdir -p lib;
mkdir -p bin;

mkdir -p econfig && echo "let supported_systems = []" > econfig/econfig.ml;

#mkdir -p spec && cd spec \
#&& wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/master/xml/vk.xml"
