C
C PROGRAM TO CONVERT HYPO71 FORMAT OR HYPOINVERSE
C TO NORDIC STANDARD FORMAT.
C  
C BY M. VILLAGRAN, NOVEMBER-91
C
C
c Updates
c dec 14 91 by jh: change to new nordic format, add more to header line
c mar 93         : fix bugs
c jun 2 93       : --------, more still
c jun 3          L: ---
c feb7 94        : used format f6.2 doe reading s, comment out CERO for weight 9
c dec 94         : ************* version 5.0 ****************  no change
c mar 1 99 by jh :---------------version 7.0 check --------------------
c                 ask for century, no 5 char stations
c nov 24 2016 jh :read coda as f4.0 instead of f4.1, put 1 at end of heqader line
c
c
C character variables definition
C 
      implicit none
	  character*80 text
      CHARACTER*80 HEADERINFO
	  CHARACTER*30 INFILE
      CHARACTER*5 CERO,SECP
      character*6 secs 
      CHARACTER*4 ST,CODA 
      CHARACTER*3 AMP1,AMP
   	  CHARACTER*2 PHP,PHS,SP
      CHARACTER*1 TYPE,UDP,WTP,UDS,WTS,point     
      logical new_event,write_header
      character*2 century,xyr
C
C integer variables definition 
C
        INTEGER YR,MTH,DY,JJ,HRMN,error,icoda,new_hour,old_hour
        real xsec,xcoda



c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

C
C Formats
C 100 is to read Hypo71pc phases format
C 200 is to write the headline of identification of the quake in NF
c NF means Nordic Format.
C 300 is to write P or S Phases format in NF
C
 200    FORMAT(1X,a2,a2,1X,2I2,1X,I4,1X,f4.1,1x,A1,57X,a1)
	            
C
        error=0
        HEADERINFO(1:40)=' STAT SP IPHASW D HRMN SECON CODA AMPLIT'
c        headerinfo(41:80)=' PERI AZIMU VELO SNR AR TRES W  DIS CAZ7'
        headerinfo(41:80)=' PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'

        WRITE(*,*)'  '
        WRITE(*,*)'  '
C
C open file to convert
C
        WRITE(*,*)' NAME OF HYPO71 FILE ?'
        READ(*,'(A)')INFILE
 666    continue
        write(6,*)' Give century, 19 or 20'
        read(5,'(a2)') century
        if(century.ne.'19'.and.century.ne.'20') goto 666
        OPEN(1,FILE=INFILE,ERR=80,STATUS='OLD')
C
C all quakes will be cataloged like local 
C it has to be updated
C
        TYPE='L'
C
C all phases will be considerated like Z component 
C because H71pc and Hinvpc formats don't have
C notation of that.
C
        SP='SZ'
C
C to be used in P phase if Weigth option in pc format is 9.
C
        CERO='     '
C
C JJ is the quakes counter
C
	JJ=0
C
C OPEN THE NORDIC FORMAT FILE TO CREATE
C WRITE THE HEAD INFORMATION IN OUTPUT FILE
C
        OPEN(2,FILE='hypnor.out',STATUS='unknown')
        open(3,file='hypnor.err',status='unknown')
        WRITE(*,*)'  '
        WRITE(*,*)'  '
C
C START THE PROCESS TO CONVERT FORMAT
C at first the program writes P phase
C that is because the NF uses one line for each phase
C after the program change the variable values
C to write the S phase
C
C
c
c   enter here for new event
c
 20	   continue 
            write_header=.true.
            new_event=.false.
C
C earthquakes counter
C
	    JJ=JJ+1
c
c   here for next line of one event
c
 21     continue
        read(1,'(a)',end=90) text
c
c  check if line is a new event, might by junk with blanks in front
c
        if(text(1:4).eq.'   '.and.(.not.new_event)) goto 21
c
c   set new event flag
c
c
c check that no separator missing, if so make a new event
c
        read(text(16:17),'(i2)')new_hour
        if((new_hour-old_hour).ge.1.and.(.not.write_header)) then
           backspace 1
           old_hour=new_hour
           write_header=.true.
           new_event=.true.
           write(2,*)'   '
           write(3,'(a,a)')' separator: ',text(1:40)
           error=error+1
           goto 21
        endif
        old_hour=new_hour
c
c   check if end of event, if so write blank separation line
c
    	IF(text(1:4).eq.'    ')THEN
           write(2,*)'      '
	   GOTO 20
     	ENDIF
        new_event=.true.
        READ(text,100,ERR=60)ST,PHP,UDP,WTP,YR,MTH,DY,HRMN,
     *  SECP,SECS,
     +  phs,UDS,WTS,AMP,POINT,CODA
 100    FORMAT(A4,A2,2A1,1X,3I2,I4,A5,6X,A6,A2,2A1,3X,A3,A1, 
     +  23X,A4)  

        write(xyr,'(i2)') yr
        if(xyr(1:1).eq.' ') xyr(1:1)='0'     
        if(xyr(2:2).eq.' ') xyr(2:2)='0'   

        goto 65
 60     continue
           write(3,'(a)') text
           error=error+1
           goto 21
 65     continue
c
c   check if writing a header
c
	    IF(write_header)then
           read(secp,'(f5.1)') xsec
           WRITE(2,200)century,xYR,MTH,DY,HRMN,xsec,TYPE,'1'
           WRITE(6,200)century,xYR,MTH,DY,HRMN,xsec,TYPE
           WRITE(2,'(a80)')HEADERINFO 
	      write_header=.false. 
        endif
C
C also check the 9 weigth option of the hp71 and hinvs programs
C
 27     continue
c       IF(WTP.EQ.'9') HRMN=CERO
C
C if the programs H71pc and Hinvpc reads coda values of "0"
C accept that like not a coda value, for that case
C in NF coda has to be blank
C
		IF(CODA.EQ.'   0') CODA='    '
C
C For an H71pc format the program will change the way of notacion
C of the arrival, up will be C and Down D
C
        IF(UDP.EQ.' ')  GOTO 30
        IF(UDP.EQ.'U')THEN
	 	  UDP='C' 
		ELSE
	      UDP='D'
		ENDIF
30      IF(UDS.EQ.' ')THEN 
	  	   GOTO 40
		ENDIF
        IF(UDS.EQ.'U')THEN
          UDS='C'
        ELSE
          UDS='D'
        ENDIF
40      AMP1='   '
C
C    Writes P in the NF file and make variable values to
C    can write S phase
C
c
c   convert coda to integer and write back if not zero
c
        read(coda,'(f4.0)') xcoda  ! was f4.1 before nov 2016
        icoda=xcoda
        if(icoda.ne.0) write(coda,'(i4)') icoda
c
c   check if P-phase has been supplied 
c
        if(secp.ne.'     ') then
           if(php.eq.'  ') php='EP'
           WRITE(2,300)ST,SP,PHP,WTP,UDP,HRMN,SECP,CODA,AMP1
 300       FORMAT(1X,A4,1X,A2,1X,A2,3X,A1,1X,A1,1x,i4,1X,
     *     A5,1X,a4,1X,A3)
        endif
c
c   check if S-phase is there
c
        if(secs.ne.'      ') then
c
c   if no S phase is written, substitute
c
          if(phs.eq.'  ') phs='ES'
c
c   get s time, write in f5.1 to assure that values larger than
c   will be readable in nordic format
c
          read(secs,'(f6.2)') xsec
          write(secs,'(f6.1)') xsec
          WRITE(2,301)ST,SP,PHS,WTS,UDS,HRMN,SECS(2:6)
 301      FORMAT(1X,A4,1X,A2,1X,A2,3X,A1,1X,A1,1x,I4,1X,A5)
        endif
		goto 21
C
C ERRORS DISPLAY
C
80      WRITE(*,*)'  '
        WRITE(*,*)'  '
        WRITE(*,*)'  '
        WRITE(*,*)'             THAT INPUTFILE DO NOT EXISTS !'
        WRITE(*,*)'  '
        WRITE(*,*)'  '
        WRITE(*,*)'  '
        GOTO 99
C
C FINISH
C
90      IF(error.ne.0) THEN
           write(6,*) error, 'errors were found, see hypnor.err'
        endif
		WRITE(*,'(10X,I5,1X,A22)')JJ,'QUAKES WERE CHANGED !!'
        write(6,*)' Output file name is hypnor.out'
98      CLOSE(1)
99      STOP
        END
