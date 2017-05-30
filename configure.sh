#!/usr/bin/env bash

# create the generated lib folder
mkdir lib;

mkdir config && echo "let supported_systems = []" > config/config.ml;

mkdir spec && cd spec \
&& wget "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/1.0/src/spec/vk.xml";
