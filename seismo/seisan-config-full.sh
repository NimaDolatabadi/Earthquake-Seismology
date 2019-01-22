#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######

################### Put This Script In your SEISAN Home Directory ########################

###### First do 'sudo su', due to your distribution ############

apt-get install gfortran libhdf5-serial-dev libfftw3-dev gfortran gcc-4.8 libpcre3-dev libgtk2.0-dev subversion ghostscript build-essential cmake libnetcdf-dev libgdal1-dev
add-apt-repository ppa:ubuntu-toolchain-r/test && apt-get update
update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 50
pwd=`pwd`
NMXHOME="/home/nima/src/seismo/LIB/nmx/"
cd COM/
sed -i "16s export SEISAN_TOP=$pwd/" SEISAN.bash
source SEISAN.bash
cd ../PRO
sed -i "120s INSTALL_PRO_PATH = $pwd/pro" MakeFile
make all
make install
cd ../LIB/
make all
patchelf --replace-needed $NMXHOME/libnmxbase.so $pwd/LIB/nmx/libnmxbase.so libnmxacq.so
cd ../PRO
patchelf --replace-needed $NMXHOME/libnmxacq.so $pwd/LIB/nmx/libnmxacq.so
patchelf --replace-needed $NMXHOME/libnmxbase.so $pwd/LIB/nmx/libnmxbase.so
echo '#SEISAN' >> ~/.bashrc
echo "export $pwd/COM/SEISAN.bash" >> ~/.bashrc
source ~/.bashrc
