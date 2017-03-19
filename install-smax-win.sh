#!/bin/bash

# Setup git.
[[ -z `git config --global user.name` ]] && read -p "Full name: " name && git config --global user.name "$name"
[[ -z `git config --global user.email` ]] && read -p "Email: " email && git config --global user.email $email

# Now clone smax
if [ ! -d "smax" ]; then
    git clone https://github.com/sdll/smax.git
fi

cd smax
git submodule add https://github.com/jkitchin/emacs-win
git submodule add -f https://github.com/jkitchin/scimax-win-elpa elpa
git submodule init
git submodule update

git add emacs-win
git commit emacs-win -m "add windows emacs"
git add .gitmodules
git commit .gitmodules -m "windows setup for submodules"

echo "smax is installed. To use it, run this command in your terminal."
echo "`pwd`/emacs-win/bin/runemacs.exe -q -l `pwd`/init.el"
echo "or"
echo "run the smax.bat script created in this directory or as ./smax.sh in the terminal."

# This converts the posix style path from git bash to a windows path.
# You can use this as the application to open
SMAX_ROOT=$(echo `pwd` | sed -e 's/^\///' -e 's/\//\\/g' -e 's/^./\0:/')
echo "SET LANG=C" > smax.bat
echo "start \"\" \"${SMAX_ROOT}\\emacs-win\\bin\\runemacs.exe\" -l \"${SMAX_ROOT}\\init.el\" %1" >> smax.bat

# Use this in git bash
echo "LANG=C start \"\" \"${SMAX_ROOT}\\emacs-win\\bin\\runemacs.exe\" -l \"${SMAX_ROOT}\\init.el\" \"\$1\"" > smax.sh


echo "Opening smax.  Be patient."
start "" ".\emacs-win\bin\runemacs.exe" -l ".\init.el"
#end
