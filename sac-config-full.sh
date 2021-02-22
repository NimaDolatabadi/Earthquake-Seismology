#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######

################### Put This Script In your SEISAN Home Directory ########################

###### First do 'sudo su', due to your distribution ############

sudo add-apt-repository ppa:ubuntu-toolchain-r/test && sudo apt-get update
sudo apt-get install gfortran gcc-4.8 libpcre3-dev libgtk2.0-dev subversion ghostscript build-essential cmake
sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 50
pwd=`pwd`
cd bin/
sed -i "18i export SACHOME=$pwd" sacinit.sh
cd ../exec
sudo cp * /usr/local/bin
echo '#SAC' >> ~/.bashrc
echo "source $pwd/bin/sacinit.sh" >> ~/.bashrc
echo "alias sac="/usr/local/bin/sac $pwd/macros/sac.init"" >> ~/.bashrc
source .bashrc
