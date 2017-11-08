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

