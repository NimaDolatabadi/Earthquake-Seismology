#!/bin/bash
##### This Script Has Been Written By N.Dolatabadi ##########################################

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######################
#### You can have a look at my github too ------> https://github.com/NimaDolatabadi/Earthquake-Seismology ######

###### GMT4 Installation ######

#### Uncomment if you're in need of this version of GMT ########
#### SAC, Works with GMT4 #######

mkdir /opt/GMT4
cd gmt-4.5.16

./configure --prefix=/opt/GMT4
make && make install && make install-gmt && make install-data && make spotless && make install-man && make install-doc && make examples
Add the Following lines to bashrc:

### GMT-4.5.16
export GMT4HOME=/opt/GMT-4.5.16
export PATH=${GMT4HOME}/bin:${PATH}
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${GMT4HOME}/lib
export C_INCLUDE_PATH=$C_INCLUDE_PATH:${GMT4HOME}/include

source ~/.bashrc
