c###############################################################
c## Convert an output file (.CAT) of seisan built with collect
c## into a file that matches the input format of hypo71 program
c## F. Courboulex -CNRS- UMR Geosciences Azur FRANCE - March 98
c## e-mail : courboul@faille.unice.fr
c##
c## remark: If 2 picked times exist for one phase (P or S) the
c##         earlier time is kept
c###############################################################
c
c  changes
c
c  mar 1 99 by jh : --------------   version 7.0 check --------------
c                   5 char stations will be shortened to 4 chars
c                   output file to norhyp.out
c  jun 99   jh    : smal bug on pc with variable declaration
c  jan 2012 ulissis at dgf: fix for gfortran 
c
       IMPLICIT NONE

       TYPE pick
             character*4 sta
             character*2 phase
             character*1 w
             character*1 pol
             integer  hh
             integer mn
             real   sec
             character*4 coda
       END TYPE 

       integer NbMAX
       parameter(NbMAX=100)

       TYPE (pick) tab(NbMAX)
       TYPE (pick) tabtemp(NbMAX)
       character*4 sta,not1,sta_encours
       character separator*7
       character*80 input,output
       integer cptemp,y1,iy,id,ih,im,mn,cp
       integer indiceP,indiceS,l,i,j,m
       real sp,ss
       logical bool_pick


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

C
c
       write(*,*) 'INPUT FILE? '
       read(*,'(a)') input
       output='norhyp.out'
       open(unit=1,file=input,status='old')
       open(unit=2,file=output,status='unknown')

c      Loop on each event
       do while (.TRUE.)
         bool_pick=.FALSE.
         read(1,100,end=900) y1,iy,im,id,ih,mn
 100     format(1x,a2,i2,1x,i2,i2,1x,i2,i2)

   3      read(1,300,end=900) not1
 300      format(1x,a4)
          if(not1.ne.'STAT') goto 3

        sta='xxxx'
        cp=0
c       Treat the information of one event
        do while (sta.ne.'    ')
           cp=cp+1
           read(1,400,end=900) tab(cp)%sta,tab(cp)%phase,tab(cp)%w,
     &     tab(cp)%pol,tab(cp)%hh,tab(cp)%mn,tab(cp)%sec,tab(cp)%coda
           sta=tab(cp)%sta
 400        format(1x,a4,4x,a2,3x,a1,x,a1,x,i2,i2,x,f5.2,x,a4)
        enddo
        do m=1,NbMAX
           tabtemp(m)%sta='   '
           tabtemp(m)%phase='  '
           tabtemp(m)%w=' '
           tabtemp(m)%pol=' '
           tabtemp(m)%coda='    '
          enddo

       cp=cp-1
c       Treat the information of one event and one station
        do i=1,cp

          cptemp=0
         if (tab(i)%sta.ne.'xxxx') then
             cptemp=cptemp+1
             sta_encours=tab(i)%sta
             tabtemp(cptemp)=tab(i)
             tab(i)%sta='xxxx'

           do j=i+1,cp
                 if (tab(j)%sta.eq.sta_encours) then
                 cptemp=cptemp+1
                 tabtemp(cptemp)=tab(j)
                 tab(j)%sta='xxxx'
                 endif
             enddo

              indiceP=0
              indiceS=0

           do l=1,cptemp
c          If 2 picked times exist for one phase (P or S) the
c          earlier time is kept
           if( ((tabtemp(l)%phase.eq.'IP').OR.
     &      (tabtemp(l)%phase.eq.'EP').OR.
     &      (tabtemp(l)%phase.eq.' P')).AND.
     &      (indiceP.eq.0) ) then
              indiceP=l
           endif
           if(((tabtemp(l)%phase.eq.'IP').OR.
     &      (tabtemp(l)%phase.eq.'EP').OR.
     &      (tabtemp(l)%phase.eq.' P'))
     &       .AND.(indiceP.ne.0)) then
             if(tabtemp(l)%sec.lt.tabtemp(indiceP)%sec) then
                  indiceP=l
             endif
            endif

          if(((tabtemp(l)%phase.eq.'IS').OR.
     &      (tabtemp(l)%phase.eq.'ES').OR.(tabtemp(l)%phase.eq.' S'))
     &      .AND.(indiceS.eq.0)) then
            indiceS=l
          endif
          if(((tabtemp(l)%phase.eq.'IS').OR.
     &     (tabtemp(l)%phase.eq.'ES').OR.(tabtemp(l)%phase.eq.' S'))
     &     .AND.(indiceS.ne.0)) then
             if(tabtemp(l)%sec.lt.tabtemp(indiceS)%sec) then
                    indiceS=l
               endif
           endif

              sp=tabtemp(indiceP)%sec
              ss=tabtemp(indiceS)%sec
              mn=tabtemp(indiceP)%mn
             if(mn.ne.tabtemp(indiceS)%mn) then
                ss =  tabtemp(indiceS)%sec + 60.0
            endif
           enddo
              if(indiceP.eq.0)  sp=-9
              if(indiceS.eq.0) ss=-9
              if (sp.eq.-9) goto 550

             bool_pick=.TRUE.
             if (ss.ne.-9) then
               write(2,500) tabtemp(indiceP)%sta,
     &              tabtemp(indiceP)%phase,
     &              tabtemp(indiceP)%pol,
     &              tabtemp(indiceP)%w,
     &              iy,im,id,ih,mn,
     &              sp,ss,
     &              tabtemp(indiceS)%phase,
     &              tabtemp(indiceS)%w,
     &              tabtemp(indiceP)%coda
              else
               write(2,700) tabtemp(indiceP)%sta,
     &              tabtemp(indiceP)%phase,
     &              tabtemp(indiceP)%pol,
     &              tabtemp(indiceP)%w,
     &              iy,im,id,ih,mn,
     &              sp,
     &              tabtemp(indiceP)%coda
              endif

           endif
 550    enddo

 500   format(a4,a2,a1,a1,x,i2,i2,i2,i2,i2,f5.2,7x,f5.2,a2,x,a1,31x,a4)
 700   format(a4,a2,a1,a1,x,i2,i2,i2,i2,i2,f5.2,7x,40x,a4)

       separator='10  5.0'
       if(bool_pick) write(2,600) separator
 600   format(17x,a7)

       enddo

 900   continue
       close(1)
       close(2)
       write(6,*)' Output file name is norhyp.out'
       stop
       end

c#############################################################################


