#
# SEISAN definitions to be sourced from users .bashrc file 
#

#
# set platform, used for compilation, 
# MUST BE: solaris, linux32, linux64, windows, macosx or macosxppc
#
export SEISARCH="linux64"
#export SEISARCH="linux32"
#export SEISARCH="solaris"

#
#    set SEISAN top directory
#
SEISAN_TOP=Path.To-Seisan.Home
#
# set editor
#
export SEISAN_EDITOR="vi"

#
# set def base 
#
export DEF_BASE="TEST_"

#
# set printer
#
export PRINTER="hpbw2"

#
# set X and Y scaling for Postscript output
#   some examples:  A4     x=0.55,y=1.0
#                   Letter x=0.55,y=0.9
#
export SEISAN_PSSCALE_X="0.55"
export SEISAN_PSSCALE_Y="1.0"

#
# set up search PATH
#
export PATH="$SEISAN_TOP/PRO:$PATH"

#
# aliases for SEISAN
#
alias pr='cd $SEISAN_TOP/PRO'
alias li='cd $SEISAN_TOP/LIB'
alias ic='cd $SEISAN_TOP/INC'
alias re='cd $SEISAN_TOP/REA'
alias da='cd $SEISAN_TOP/DAT'
alias wo='cd $SEISAN_TOP/WOR'
alias wa='cd $SEISAN_TOP/WAV'
alias ca='cd $SEISAN_TOP/CAL'
alias co='cd $SEISAN_TOP/COM'
alias in='cd $SEISAN_TOP/INF'

#
# following lines are for the PITSA analysis package 
#
export PITSAHOME="$SEISAN_TOP/DAT"
export PITSA_CONFIG_PATH_ENV="$PITSAHOME/"
export PITSA_PRINTDEF_PATH_ENV="$PITSAHOME/"
export PITSA_PRINTDEF_NAME_ENV="8X11_landscape.PS"


#
# include seismo/LIB in LD_LIBRARY_PATH, needed by NANSEI 
export LD_LIBRARY_PATH="$SEISAN_TOP/LIB:$LD_LIBRARY_PATH"

#
# set CLASSPATH for java app
#
export CLASSPATH="$SEISAN_TOP/PRO:$SEISAN_TOP/PRO/jseisan.jar:$CLASSPATH"

#
# aliases for Java tools
#
alias jseisan='java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/jseisan.jar SEISAN'
alias seisconf='java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/jseisan.jar SEISCONF'
alias sformat='java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/sformat.jar Sformat'

alias smap='java -jar $SEISAN_TOP/PRO/seis2viewer.jar $1'
alias getpde='java -jar $SEISAN_TOP/PRO/getPDE.jar'

#
#   alias python tools
#
alias  mopad='python $SEISAN_TOP/PRO/mopad.py $1 $2'
alias  spectrogram='python $SEISAN_TOP/PRO/spectrogram.py $1 $2i $3 $4'


#
# set SEISAN extension code, this variable can be used to implement specific 
# code into SEISAN, at the moment used are:
#                BGS
#
export SEISAN_EXTENSION="none"

