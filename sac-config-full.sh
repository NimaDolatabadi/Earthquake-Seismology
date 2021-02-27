#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######

################### Put This Script In your SEISAN Home Directory ########################

###### First do 'sudo su', due to your distribution ############

sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt upgrade
sudo apt install gfortran libhdf5-serial-dev libfftw3-dev gfortran gcc libpcre3-dev libgtk2.0-dev subversion ghostscript build-essential cmake libnetcdf-dev libgdal1-dev
cd ..
mv Earthquake-Seismology SAC
cd SAC
pwd=`pwd`
SACHOME_def=$pwd
sed -i -e "18s|SACHOME|export PATH=${PATH}:${SACHOME}/bin|g" bin/sacinit.sh
echo '#SAC' >> ~/.bashrc
echo "export SACHOME=$SACHOME_def" >> ~/.bashrc
echo "export PATH=${SACHOME}/bin:${PATH}" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${SACHOME}/lib" >> ~/.bashrc
echo "export C_INCLUDE_PATH=$C_INCLUDE_PATH:${SACHOME}/include" >> ~/.bashrc
echo "export SACAUX=$SACHOME/aux" >> ~/.bashrc
echo "alias sac="$pwd/bin/sac $pwd/macros/sac.init"" >> ~/.bashrc
source ~/.bashrc
