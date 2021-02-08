#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######

################### Put This Script In your SEISAN Home Directory ########################

###### First do 'sudo su', due to your distribution ############
sudo su
apt-get install gfortran libhdf5-serial-dev libfftw3-dev gfortran gcc-4.8 libpcre3-dev libgtk2.0-dev subversion ghostscript build-essential cmake libnetcdf-dev libgdal1-dev
add-apt-repository ppa:ubuntu-toolchain-r/test && apt-get update
update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 50
pwd=`pwd`
NMXHOME="/home/nima/src/seismo/LIB/nmx"
cd COM/
sed -i -e "16s|SEISAN_TOP|SEISAN_TOP=$pwd|g" SEISAN.bash
cd ../PRO
sed -i -e "16s|SEISAN_TOP|INSTALL_PRO_PATH=$pwd/pro|g" Makefile
#make all
#make install
cd ../LIB/
#make all
patchelf --replace-needed $NMXHOME/libnmxbase.so $pwd/LIB/nmx/libnmxbase.so libnmxacq.so
cd ../PRO
patchelf --replace-needed $NMXHOME/libnmxacq.so $pwd/LIB/nmx/libnmxacq.so y5dump
patchelf --replace-needed $NMXHOME/libnmxbase.so $pwd/LIB/nmx/libnmxbase.so y5dump
echo '# SEISAN' >> ~/.bashrc
echo "export SEISANHOME=$pwd" >> ~/.bashrc
echo "export $pwd/COM/SEISAN.bash" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${SEISANHOME}/LIB" >> ~/.bashrc
source ~/.bashrc
