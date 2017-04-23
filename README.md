# QNject-client

A browser-based client for [qnject](https://github.com/tfoldi/qnject).

## Building

```bash

# Install elm & other pre-requisites
npm install

# Update the elm repo bits
git submodule init
git submodule update

# generate src/Qnject/ApiObjects.elm from the uproto
npm run uproto

# build the client
npm run build

```