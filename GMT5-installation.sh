#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ##########################################

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######################
#### You can have a look at my github too ------> https://github.com/NimaDolatabadi/Earthquake-Seismology ######

###### GMT5 Installation ######
tar gmt-5.4.2-src.tar.xz
cd gmt-5.4.2
mkdir build
cd cmake
echo 'set (CMAKE_INSTALL_PREFIX "/opt/GMT5")' > ConfigUser.cmake
echo 'set (GMT_INSTALL_MODULE_LINKS FALSE)' >> ConfigUser.cmake
echo 'set (GSHHG_ROOT <PATH-to-your-gshhg>' >> ConfigUser.cmake
echo 'set (DCW_ROOT <PATH-to-your-dcw>' >> ConfigUser.cmake
echo 'set (COPY_GSHHG TRUE)' >> ConfigUser.cmake
echo 'set (COPY_DCW TRUE)' >> ConfigUser.cmake
echo 'set (GMT_USE_THREADS TRUE)' >> ConfigUser.cmake
cd ../build
cmake ..
make
make install
#### Add the Followings to .Bashrc  ####

## GMT-5.4.2'
export GMT5HOME=/opt/GMT-5.4.2
export PATH=${GMT5HOME}/bin:$PATH
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${GMT5HOME}/lib64


### in the end ###
Source .bashrc
