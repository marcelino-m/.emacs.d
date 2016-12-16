#!/bin/bash
RED='\033[0;31m'
NC='\033[0m' # No Color

# system dependecies for ycmd
echo -e "${RED}Installing:${NC} dependencies for ycmd"
sudo aptitude install python-dev python3-dev build-essential cmake

# dictionaries
echo -e "${RED}Installing:${NC} dictionaries"
sudo aptitude install aspell-es aspell-en

# misc
echo -e "${RED}Installing:${NC} sqlitebrowser"
sudo aptitude install sqlitebrowser

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
