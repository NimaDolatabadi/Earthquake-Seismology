# This script can be used to test the effects of different environmental
#  options within SAC. Copy it to the directory from which you want to
#  launch SAC and edit it to set the various options. If the name of your
#  copy is mysacinit, the settings will be set for the current session
#  by entering
#
#  source ./mysacinit.csh
#
#  and then run SAC.  Once one is satisfied, one should put the 
#  settings directly in your .cshrc or by adding the line
#
#   source ${SACHOME}/bin/sacinit.csh
#
# to your .cshrc
#
# Initialize the SAC Environment (only SACHOME may need to be changed)
setenv SACHOME /usr/local/sac
setenv PATH ${PATH}:${SACHOME}/bin
setenv SACAUX ${SACHOME}/aux

# Uncomment the desired options below to change from the default and
# comment out the default option
#
# SAC_DISPLAY_COPYRIGHT 
# Undefined or 1 -- Show Copyright Notice on Initialization ( Default )
#              0 -- Do no show Copyright Notice
# The copyright is only displayed during the initialization of SAC
# It contains the build date, version number and copyright information
# Default: SAC_DISPLAY_COPYRIGHT 1
setenv SAC_DISPLAY_COPYRIGHT 1
# setenv SAC_DISPLAY_COPYRIGHT 0

# SAC_USE_DATABASE
# Undefined or 1 --        Use SeisMgr Database ( Default )
#              0 -- Do Not Use SeisMgr Database
# The SeisMgr database attempts to keep the CSS data fields in line
#  with those in the SAC header.  If you are handling CSS data it 
#  would be wise to keep the database on.  Using the SeisMgr database
#  currently can be very slow due when handling hundreds of files
#  and turning it off should show a dramatic speed increase.
#  HOWEVER, some features of SAC still depend on its existence, which is
#  why the default is to have it on.
# Default: SAC_USE_DATABASE 1
setenv SAC_USE_DATABASE 1
# setenv SAC_USE_DATABASE 0

# SAC_PPK_LARGE_CROSSHAIRS
# Undefined or 0 - Tiny Cross Hairs ( Default )
#              1 - Cross Hairs of the Full Plot Window
# In older version of SAC, the crosshairs displayed in ppk extended
# across the entire window length.  To some, this was desirable.
# To utilize this feature in ppk, set SAC_PPK_LARGE_CROSSHAIRS to 1
# Default: SAC_PPK_LARGE_CROSSHAIRS 0
setenv SAC_PPK_LARGE_CROSSHAIRS 0
# setenv SAC_PPK_LARGE_CROSSHAIRS 1

