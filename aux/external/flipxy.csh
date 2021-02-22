#!/bin/csh -f 
# script to add an external command to libext.so, 
# my a shared external command library.  
#
#
# 1) setup the SACSOLIST and LD_LIBRARY_PATH environments. 
#
setenv SACSOLIST libext.so   # note can have multiple shared libs if use quotes
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/us/peterg/res1/sac/bin/extern # 
# 
# 2) compile srcs and load into one or more external libs as indicated above.  
cc -o libext.so -G flipxy.c  # note: can have multiple srcs on this line
#
# Now were ready to run sac load the command and use it.  
# May want to use and init script to load automatically. 
alias sac "/usr/local/bin/sac /us/peterg/bin/sacmac/sac.init" 
# where sac.init includes the command load command_name


