#!/bin/bash
RED='\033[0;31m'
NC='\033[0m' # No Color

# initialize and update submodules
git submodule update --init --recursive

# system dependecies for ycmd
echo -e "${RED}Installing:${NC} dependencies for ycmd"
sudo aptitude install python-dev python3-dev build-essential cmake
echo -e "${RED}Installing:${NC} gloabal"
sudo aptitude install global

# dictionaries
echo -e "${RED}Installing:${NC} dictionaries"
sudo aptitude install aspell-es aspell-en
echo -e "${RED}Installing:${NC} Ag silver search"
sudo aptitude install  silversearcher-ag

HOME_USER=/home/$USER

# builld ycmd server
cd vendors/ycmd
./build.py --clang-complete
cd -

# install tern server
npm install tern -g

# install font incosolata
mkdir -p $HOME_USER/.fonts
ln -sf $(readlink -f vendors/inconsolata-font/) $HOME_USER/.fonts
