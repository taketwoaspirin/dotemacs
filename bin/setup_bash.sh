#! /bin/bash

# Bash history
if [ "x$(which hh)" == "x" ] ; then
    sudo add-apt-repository ppa:ultradvorka/ppa
    sudo apt update
    sudo apt install hh
    hh --show-configuration >> ~/.bashrc
fi

# up

curl --create-dirs -o ~/.config/up/up.sh https://raw.githubusercontent.com/shannonmoeller/up/master/up.sh
grep "config/up/up.sh" ~/.bashrc
if [ $? == 1 ] ; then
    echo 'source ~/.config/up/up.sh' >> ~/.bashrc
fi

# fzf

git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

# z

mkdir ~/.bash_completion.d
curl "https://raw.githubusercontent.com/rupa/z/master/{z.sh}" -o ~/.bash_completion.d/"#1"

# fz

curl "https://raw.githubusercontent.com/changyuheng/fz/master/{fz.sh}" -o ~/.bash_completion.d/z"#1"

grep "~/.bash_completion.d" ~/.bashrc
if [ $? == 1 ] ; then
    echo "if [ -d ~/.bash_completion.d ] ; then" >> ~/.bashrc
    echo "  for file in ~/.bash_completion.d/* ; do" >> ~/.bashrc
    echo "    source $file" >> ~/.bashrc
    echo "  done" >> ~/.bashrc
    echo "fi" >> ~/.bashrc
fi
