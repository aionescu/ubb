#!/usr/bin/env sh

# Run Gradle task to generate HTML docs
gradle javadoc

# Copy the docs to `doc` folder
rm -rf doc
cp -r build/docs doc

if [ "$1" = "--show" ]; then
  xdg-open doc/javadoc/index.html
fi
