#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######

################### Put This Script In your SEISAN Home Directory ########################

###### First do 'sudo su', due to your distribution ############

sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt upgrade
sudo apt install gfortran libhdf5-serial-dev libfftw3-dev gfortran gcc-4.8 libpcre3-dev libgtk2.0-dev subversion ghostscript build-essential cmake libnetcdf-dev libgdal1-dev
pwd=`pwd`
git clone --single-branch -b SAC https://github.com/NimaDolatabadi/Earthquake-Seismology.git
mv Earthquake-Seismology-SAC SAC
cd SAC/bin
sed -i "18i export SACHOME=$pwd" sacinit.sh
cd ../exec
sudo cp * /usr/local/bin
echo '#SAC' >> ~/.bashrc
echo "source $pwd/bin/sacinit.sh" >> ~/.bashrc
echo "alias sac="/usr/local/bin/sac $pwd/macros/sac.init"" >> ~/.bashrc
source .bashrc
