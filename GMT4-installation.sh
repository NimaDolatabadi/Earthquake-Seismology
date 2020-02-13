#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ##########################################

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######################
#### You can have a look at my github too ------> https://github.com/NimaDolatabadi/Earthquake-Seismology ######

###### GMT4 Installation ######

#### Uncomment if you're in need of this version of GMT ########
#### SAC, Works with GMT4 #######
sudo su
sudo add-apt-repository ppa:ubuntugis/ppa && sudo apt-get update
apt-get install cmake ghostscript cdftools libnetcdf-dev libnetcdf11 libnetcdff-dev libnetcdff6 gdal-devel gdal-python make glibc curl
apt-get install subversion ghostscript build-essential cmake libnetcdf-dev libgdal1-dev libfftw3-dev libpcre3-dev libfftw3-dev libpcre3-dev libgdal-dev gdal-bin
wget -c --retry-connrefused --tries=0 --timeout=5 http://gmt.mirror.ac.za/legacy/gmt-4.5.16-src.tar.bz2
mkdir /opt/gmt-4.5.16
tar -xf gmt-4.5.16-src.tar.bz2
cd gmt-4.5.16
pwd=pwd
wget -c --retry-connrefused --tries=0 --timeout=5 http://gmt.mirror.ac.za/legacy/gshhs-2.2.0.tar.bz2
tar -xf gshhs-2.2.0.tar.bz2
./configure --prefix=$pwd/src/GMT-4.5.16 --with-gshhg-dir=$pwd/gshhs-2.2.0
make && make install && make install-gmt && make install-data && make spotless && make install-man && make install-doc && make examples

echo '# GMT-4.5.16' >> ~/.bashrc
echo "export GMT4HOME=/opt/GMT-4.5.16" >> ~/.bashrc
echo "export PATH=${GMT4HOME}/bin:${PATH}" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${GMT4HOME}/lib" >> ~/.bashrc
echo "export C_INCLUDE_PATH=$C_INCLUDE_PATH:${GMT4HOME}/include" >> ~/.bashrc

source ~/.bashrc
