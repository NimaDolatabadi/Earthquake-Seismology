#    
# SEISAN definitions to be sourced from users .cshrc file
#

#
# set platform, used for compilation, can be solaris , gfortran or g77
# MUST BE: solaris, linux32, linux64, windows, macosx or macosxppc
#
setenv SEISARCH linux64
#setenv SEISARCH linux32
#setenv SEISARCH solaris

#
#    set SEISAN top directory
#
setenv SEISAN_TOP /home/s2000/seismo

#
# set default data base
#
setenv DEF_BASE TEST_

#
# set editor
#
setenv SEISAN_EDITOR vi

#
# set printer
#
setenv PRINTER sps

#
# set X and Y scaling for Postscript output 
#   some examples:  A4     x=0.55,y=1.0
#                   Letter x=0.55,y=0.9
#
setenv SEISAN_PSSCALE_X 0.55 
setenv SEISAN_PSSCALE_Y 1.0 

#
# aliases for SEISAN
#
alias pr            'cd $SEISAN_TOP/PRO'     
alias li            'cd $SEISAN_TOP/LIB'      
alias ic            'cd $SEISAN_TOP/INC'        
alias re            'cd $SEISAN_TOP/REA'
alias da            'cd $SEISAN_TOP/DAT'
alias wo            'cd $SEISAN_TOP/WOR'
alias wa            'cd $SEISAN_TOP/WAV'
alias ca            'cd $SEISAN_TOP/CAL'
alias co            'cd $SEISAN_TOP/COM'
alias in            'cd $SEISAN_TOP/INF'
alias is            'cd $SEISAN_TOP/ISO'

#
# following lines are for the PITSA analysis package
#
setenv PITSAHOME $SEISAN_TOP/DAT
setenv PITSA_CONFIG_PATH_ENV $PITSAHOME/
setenv PITSA_PRINTDEF_PATH_ENV $PITSAHOME/
setenv PITSA_PRINTDEF_NAME_ENV 8X11_landscape.PS
  
#
# include seismo/LIB in LD_LIBRARY_PATH, needed by NANSEI
#
if ($?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH $SEISAN_TOP/LIB:$LD_LIBRARY_PATH
else
  setenv LD_LIBRARY_PATH $SEISAN_TOP/LIB
endif

#
# set CLASSPATH for java app
#
if ($?CLASSPATH) then
  setenv CLASSPATH $SEISAN_TOP/PRO:$SEISAN_TOP/PRO/jseisan.jar:$CLASSPATH
else
  setenv CLASSPATH $SEISAN_TOP/PRO:$SEISAN_TOP/PRO/jseisan.jar
endif

#
# aliases for Java tools
#
alias jseisan 'java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/jseisan.jar SEISAN'
alias seisconf 'java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/jseisan.jar SEISCONF'
alias sformat 'java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/sformat.jar Sformat' 
alias smap 'java -jar $SEISAN_TOP/PRO/seis2viewer.jar $1'
alias getpde 'java -jar $SEISAN_TOP/PRO/getPDE.jar'
#
#   alias python tools
#
alias mopad 'python $SEISAN_TOP/PRO/mopad.py $1 $2'
alias spectrogram 'python  $SEISAN_TOP/PRO/spectrogram.py $1 $2 $3 $4'

#
# path
#
set pro_path = ($SEISAN_TOP/PRO)
if( `echo $path | grep -c pro_path` == 0 ) then 
  set path=($pro_path $path)
endif

#
# set SEISAN extension code, this variable can be used to implement specific 
# code into SEISAN, at the moment used are:
#                BGS
#
setenv SEISAN_EXTENSION none

