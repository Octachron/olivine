#!/usr/bin/env bash

# create the generated lib folder
mkdir lib;
mkdir bin;

mkdir econfig && echo "let supported_systems = []" > config/econfig.ml;

mkdir spec && cd spec \
&& wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml";
