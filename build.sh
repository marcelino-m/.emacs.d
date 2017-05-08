#!/bin/bash
RED='\033[0;31m'
NC='\033[0m' # No Color

# initialize and update submodules
git submodule update --init --recursive

# system dependecies for ycmd
echo -e "${RED}Installing:${NC} dependencies for ycmd"
sudo apt-get install python-dev python3-dev build-essential cmake
echo -e "${RED}Installing:${NC} gloabal"
sudo apt-get install global

# dictionaries
echo -e "${RED}Installing:${NC} dictionaries"
sudo apt-get install aspell-es aspell-en

# misc
echo -e "${RED}Installing:${NC} xclip"
sudo apt-get install xclip
echo -e "${RED}Installing:${NC} source-highlight"
sudo apt-get install source-highlight
echo -e "${RED}Installing:${NC} sqlitebrowser"
sudo apt-get install sqlitebrowser
echo -e "${RED}Installing:${NC} Ag silver search"
sudo apt-get install  silversearcher-ag
echo -e "${RED}Installing:${NC} Maildir utilities and dependencies"
sudo apt-get install  html2text guile-2.0-dev  libgmime-2.6-dev libxapian-dev  libwebkitgtk-dev maildir-utils  offlineimap



HOME_USER=/home/$USER

# builld ycmd server
cd vendors/ycmd
./build.py --clang-complete
cd -

# install tern server
npm install tern -g

# install fonts
mkdir -p $HOME_USER/.fonts
ln -sf $(readlink -f vendors/inconsolata-font/) $HOME_USER/.fonts
ln -sf $(readlink -f vendors/all-the-icons/fonts) $HOME_USER/.fonts/all-the-icons
