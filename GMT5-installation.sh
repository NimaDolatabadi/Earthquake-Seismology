#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ##########################################

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######################
#### You can have a look at my github too ------> https://github.com/NimaDolatabadi/Earthquake-Seismology ######

###### GMT5 Installation ######
sudo su
apt-get install cmake ghostscript cdftools libnetcdf-dev libnetcdf11 libnetcdff-dev libnetcdff6 gdal-devel gdal-python make glibc curl
mkdir GMT5
cd GMT5
wget -c --retry-connrefused --tries=0 --timeout=5 http://gmt.mirror.ac.za/legacy/gmt-5.4.2-src.tar.xz
tar gmt-5.4.2-src.tar.xz
cd gmt-5.4.2
mkdir build
wget -c --retry-connrefused --tries=0 --timeout=5 http://gmt.mirror.ac.za/legacy/dcw-gmt-1.1.1.tar.gz
wget -c --retry-connrefused --tries=0 --timeout=5 http://gmt.mirror.ac.za/legacy/gshhs-2.2.0.tar.bz2
tar -xf dcw-gmt-1.1.1.tar.gz gshhs-2.2.0.tar.bz2 && mv gshhs-2.2.0 gshhg-gmt-nc4 && mv dcw-gmt-1.1.1 dcw-gmt
cd cmake
echo 'set (CMAKE_INSTALL_PREFIX "/opt/GMT-5.4.2")' > ConfigUser.cmake
echo 'set (GMT_INSTALL_MODULE_LINKS FALSE)' >> ConfigUser.cmake
echo 'set (COPY_GSHHG TRUE)' >> ConfigUser.cmake
echo 'set (COPY_DCW TRUE)' >> ConfigUser.cmake
echo 'set (GMT_USE_THREADS TRUE)' >> ConfigUser.cmake
cd ../build
cmake ..
make
make install && make docs_man && make docs_html &&  make docs_pdf && make install
echo '#GMT-5.4.2' >> ~/.bashrc
echo "export GMT5HOME=/opt/GMT-5.4.2" >> ~/.bashrc
echo "export PATH=${GMT5HOME}/bin:$PATH" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${GMT5HOME}/lib64" >> ~/.bashrc
source .bashrc
