c
c   program implemeted in seisan by j havskov, april 2010
c
c   changes
c
c  oct 30 2010 jh: increase dimension for number of pol from 1000 to 5000
c 2014-04-29 pv: removed PAUSE statement
c 2016-12-02 jh: prepare for better composite solution
c
      program pinv
c
c The program invert P and pP-wave polarity data for moment tensor.
c impulsive up -> +1; impulsive down -> -1
c emergent up  -> +0.5; emergent down -> -0.5
c unclear -> 0.0
c
c   in seisan, only +-1 is used
c
c Reference: Kanamori & Given (1983)
c Coded by D. Suetsugu, Dec 1990
c             Modified  Nov 1992
c             Modified  May 1993



      include 'seidim.inc'                 ! array dimentions

      character*80 data(max_data)          ! one event
      integer nhead,nrecord,nphas,id,nstat ! for reading
      character*1 exp,type                 ! -----------
      character sta(5000)*4,infile*20,textn*40
      dimension f(5000),g(5000,6)
      character*80 text
      dimension cd(5000),azm(5000),tak(5000),a(3,3)
      dimension dip(2),strk(2),slip(2)
c
      pi=acos(-1.e0)
      cdr=pi/180.
      rho=1.0e-3
      mm=5
      r0=4.0
c
c   output file
c
      open(4,file='pinv.out',status='unknown')
c
c Input file name
c      print *,'Data file name?'
c      read(5,'(a20)') infile
       infile='hyp.out'
c
c Input polarity datafile
c
c   now hardwired to hyp.out
c
      open(3,file=infile)
      idat=1
c    7 read(3,101,end=999) sta(idat),cd(idat),tak(idat),azm(idat)
 7    continue
      read(3,'(a)',end=999) text
      if((text(17:17).eq.'D'.or.text(17:17).eq.'C').
     *and.text(11:11).eq.'P'.and.text(80:80).eq.' ') then
c
c   polarity found
c
         if(text(17:17).eq.'C') then
            cd(idat)=1.0
         else
            cd(idat)=-1.0
         endif
c         write(6,*) text
         read(text(77:79),'(f3.0)') azm(idat)
c         write(6,*) azm(idat)
         read(text(58:60),'(f3.0)') tak(idat)
c         write(6,*) tak(idat)
c
c   pinv must ain less than 90
c

         if(tak(idat).gt.90.0) then
            tak(idat)=180.0-tak(idat)
            azm(idat)=azm(idat)+180.0
            if(azm(idat).gt.360.0) azm(idat)=azm(idat)-360.0
         endif
c         write(6,*) text(2:6),cd(idat),'t a',tak(idat),azm(idat)


c  101 format(1x,a4,3f10.2)
      tak(idat)=tak(idat)*cdr
      azm(idat)=azm(idat)*cdr
      acd=abs(cd(idat))
      f(idat)=acd
      if(acd.ge.9.9) then
          f(idat)=acd/10.0
      endif
      idat=idat+1

      endif
      goto 7
  999 close(3)
      ndat=idat-1
      print '(a,i3)',' Number of data used for inversion=',ndat
      write(4,'(a35,i5)') 'Number of data used for inversion=',ndat
c  compute radiation coefficients
      call radpt(azm,tak,cd,g,ndat,mm)
c  solve least squares problem
      call least(g,ndat,mm,f,a,rho)
c  convert from moment tensors to the best double couple
      call mechm(a,strk,dip,slip)

      write
     *(6,'(a18,5x,3f10.1)')' Strike, dip, rake',strk(1),dip(1),slip(1)
      write(4,'(a15,5x,3f10.1)')'strike,dip,rake',strk(1),dip(1),slip(1)
      
c
c   add solution to fps.out
c
      call add_fps(strk(1)-90.0,dip(1),slip(1),'PINV   ','P')


c
c  score --counting consistent data
c
   77 call score(strk(1)*cdr,dip(1)*cdr,slip(1)*cdr,cd,tak,
     *azm,ndat,iyes,ino)  ! jh: in org version, no corr. with cdr


      write(6,'(a,i5)') ' Consistent data:   ',iyes
      write(4,'(a,i5)') ' Consistent data:   ',iyes


      write(6,'(a,i5)') ' Inconsistent data: ',ino
      write(4,'(a,i5)') ' Inconsistent data: ',ino

      close(3)
c
c   if composite solution, write solutions to hyp.out
c
c
c   counts if more than one event
c
       text='hyp.out'
       call number_of_events(100,text,nevent)
       write(6,*)' Number of events',nevent
c
            if(nevent.gt.1) then

                write(6,*)
     *          'This is a composite solution, number of events is',
     '          nevent
                write(6,*)'Solution written to hyp.out'
                text=' '
                text(71:80)='PINV     F'
                write(text(1:30),'(3f10.1)')strk(1)-90.0,dip(1),slip(1)
                write(text(60:62),'(i3)') ino
                open(111,file='hyp.out',status='old')
                call indata(111,nstat,nphas,nhead,
     &          nrecord,type,exp,data,id)
                ifoc=0
                do i=2,nhead
c                  write(6,'(a)') data(i)
                   if(data(i)(71:80).eq.'PINV    F') then
                     ifoc=1
                     data(i)=text
                     goto 1212        ! only first
                   endif
                enddo
c               write(6,*) ifoc
                if(ifoc.eq.0) then
                   do i=nrecord,nhead,-1
                      data(i+1)=data(i)
                   enddo
                   data(nhead)=text
                   nrecord=nrecord+1
                endif
 1212           continue
                open(112,file='hyp.temp',status='unknown')
                write(112,'(a80)')(data(i),i=1,nrecord)
c
c  read-write rest of hyp.out
c
                do k=2,nevent
                   call indata(111,nstat,nphas,nhead,
     &             nrecord,type,exp,data,id)
                   write(112,'(a80)')(data(i),i=1,nrecord)
                enddo
c
c   back to hyp.out
c
                rewind(112)
                rewind(111)

                do k=1,nevent
                   call indata(112,nstat,nphas,nhead,
     &             nrecord,type,exp,data,id)
                   write(111,'(a80)')(data(i),i=1,nrecord)
                enddo
                close(111)
                close(112,status='delete')
           endif   


      stop
      end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine least(g,ndat,mm,f,a,rho)
      dimension f(5000),g(5000,6),y(6),w(6),u(5000,6),v(6,6),work(6)
      dimension s(6),x(6),a(3,3)
      call svdd(g,5000,ndat,mm,3,s,u,5000,v,6,work,ill)
      if(ill.ne.0) then
      write(6,'('' ILL='',i5)') ill
      stop
      endif
c
      do 10 i=1,mm
      sm=0.0
      do 11 j=1,ndat
      sm=sm+u(j,i)*f(j)
   11 continue
      w(i)=sm
   10 continue
      krank=mm
      tau=rho*s(1)
      do 12 i=1,mm
      if(s(i).le.tau) goto 13
   12 continue
      goto 14
   13 krank=i-1
   14 continue
      write(6,100) tau,krank
  100 format(1x,'Absolute pseudorank tolerance ',F12.6,
     1  10X,'Pseudorank ',I3)
      do 15 i=1,krank
      f(i)=w(i)/s(i)
   15 continue
      do 16 i=1,mm
      sm=0.0
      do 17 j=1,krank
      sm=sm+v(i,j)*f(j)
   17 continue
      x(i)=sm
   16 continue
      do 18 i=1,mm
      y(i+6-mm)=x(i)
   18 continue
      if(mm.eq.5) y(1)=-y(2)-y(3)
      a(1,1)=y(1)
      a(2,2)=y(2)
      a(3,3)=y(3)
      a(1,2)=y(4)
      a(1,3)=y(5)
      a(2,3)=y(6)
      a(3,2)=a(2,3)
      a(3,1)=a(1,3)
      a(2,1)=a(1,2)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine radpt(azes,takof,pol,g,ndat,mm)
      dimension g(5000,6),azes(5000),takof(5000),pol(5000)
      do 1 i=1,ndat
      sni=sin(takof(i))
      csi=cos(takof(i))
      snf=sin(azes(i))
      csf=cos(azes(i))
      sni2=sni*sni
      csi2=csi*csi
      snf2=snf*snf
      csf2=csf*csf
      if(nint(pol(i)).gt.0) then
      p=1.0
      else
      p=-1.0
      endif
      if(mm.eq.6) then
      g(i,1)=csi2*p
      g(i,2)=sni2*csf2*p
      g(i,3)=sni2*snf2*p
      g(i,4)=2.0*sni*csi*csf*p
      g(i,5)=-2.0*sni*csi*snf*p
      g(i,6)=-2.0*snf*csf*sni2*p
      else
      g(i,1)=(sni2*csf2-csi2)*p
      g(i,2)=(sni2*snf2-csi2)*p
      g(i,3)=2.0*sni*csi*csf*p
      g(i,4)=-2.0*sni*csi*snf*p
      g(i,5)=-2.0*snf*csf*sni2*p
      endif
    1 continue
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 
**********************************************
      subroutine score(strk1,dip1,slp1,cd,tak,azm,idat,iyes,ino)
      dimension cd(5000),tak(5000),azm(5000)
      iyes=0
      ino=0
      do 1 i=1,idat
          if(nint(cd(i)*10).eq.0) goto 1
          call pamp(strk1,dip1,slp1,azm(i),tak(i),amp)
          a=amp*cd(i)
          if(a.gt.0.0) then
              iyes=iyes+1
          else if(a.lt.0.0) then
              ino=ino+1
          endif
 1    continue
      return
      end
**********************************************
      SUBROUTINE PAMP(STR,DIP,SLP,AZIMUTH,TAKEOFF,AMP)
c Reference: Aki & Richards, 1980, pp115
      PI=ACOS(-1.D0)
      CDR=PI/180.
c
      SNSLP=SIN(SLP)
      CSSLP=COS(SLP)
      SNDIP=SIN(DIP)
      CSDIP=COS(DIP)
      SN2DP=2.*SNDIP*CSDIP
      CS2DP=CSDIP*CSDIP-SNDIP*SNDIP
c
      SNEM=SIN(takeoff)
      CSEM=COS(takeoff)
      SNEM2=SNEM*SNEM
      CSEM2=CSEM*CSEM
      SN2EM=2.*SNEM*CSEM
c
      AZM=AZIMUTH-STR
      SNAZ=SIN(AZM)
      CSAZ=COS(AZM)
      SN2AZ=2.*SNAZ*CSAZ
      SNAZ2=SNAZ*SNAZ
c
      AMP=CSSLP*SNDIP*SNEM2*SN2AZ
      AMP=AMP-CSSLP*CSDIP*SN2EM*CSAZ
      TEMP=CSEM2-SNEM2*SNAZ2
      AMP=AMP+SNSLP*SN2DP*TEMP
      AMP=AMP+SNSLP*CS2DP*SN2EM*SNAZ
      RETURN
      END
********************************************************
      subroutine rake(strk1,dip1,strk2,dip2,rake1,rake2)
      cosd1=cos(dip1)
      cosd2=cos(dip2)
      sind1=sin(dip1)
      sind2=sin(dip2)
      cosf1=cos(strk1)
      cosf2=cos(strk2)
      sinf1=sin(strk1)
      sinf2=sin(strk2)
c
      sinr1=cosd2/sind1
      temp=sind2*cosf2+cosd2/sind1*cosd1*cosf1
      cosr1=temp/sinf1
      rake1=atan2(sinr1,cosr1)
c
      sinr2=cosd1/sind2
      temp=sind1*cosf1+cosd1/sind2*cosd2*cosf2
      cosr2=temp/sinf2
      rake2=atan2(sinr2,cosr2)
      return
      end
**********************************************
********************************************
      subroutine strkdip(di,azm,strk,dip)
      dimension di(2),azm(2)
      pi=acos(-1.e0)
      cdr=pi/180.
      bid1=pi-di(1)
      bid2=pi-di(2)
      fai=azm(2)-azm(1)
      cosi1=cos(bid1)
      cosi2=cos(bid2)
      sini1=sin(bid1)
      sini2=sin(bid2)
      cosfi=cos(fai)
      sinfi=sin(fai)
c
      cosd=cosi1*cosi2+sini1*sini2*cosfi
      d=acos(cosd)
      sinb=sinfi*sini1/sin(d)
      temp=cosi1-cosi2*cosd
      cosb=temp/sini2/sin(d)
      b=atan2(sinb,cosb)
c     print *,'beta ',b/cdr,'   bid2 ',bid2/cdr
c
      cosdip=sini2*sinb
      dip=acos(cosdip)
      sinstr=cosb/sin(dip)
      cosstr=-cosi2/sini2/tan(dip)
      str=atan2(sinstr,cosstr)
      strk=azm(2)+str-pi/2.
      if(dip.gt.pi/2.0) then
         dip=pi-dip
         strk=strk+pi
      endif
      if(strk.le.0.0) strk=strk+2.0*pi
      if(strk.ge.2.0*pi) strk=strk-2.0*pi
c
      return
      end
****************************************

*************************************************************



      SUBROUTINE DELAZ4 (THEI, ALEI, THSI, ALSI, DELT, DELTDG,
     2DELTKM, AZES, AZESDG, AZSE, AZSEDG, I  )
      IF (I)  50, 50, 51
C   IF COORDINATES ARE GEOGRAPH DEG     I = 0
C   IF COORDINATES ARE GEOCENT RADIAN   I = 1
C*****
   50 THE = 1.745329252E-2  *  THEI
      ALE = 1.745329252E-2  *  ALEI
      THS = 1.745329252E-2  *  THSI
      ALS = 1.745329252E-2  *  ALSI
      THE1 = 0.9931177 * TAN(THE)
      THE = ATAN(THE1)
      THS1 = 0.9931177 * TAN(THS)
      THS = ATAN(THS1)
      GO TO 32
   51 THE = THEI
      ALE = ALEI
      THS = THSI
      ALS = ALSI
C*****
   32 C = SIN ( THE )
      AK = - COS ( THE )
      D = SIN (ALE )
      E =  -COS (ALE )
      A = AK * E
      B = -AK * D
      G = -C * E
      H = C* D
      CP = SIN (THS )
      AKP = -COS (THS )
      DP = SIN (ALS)
      EP = -COS (ALS)
      AP = AKP * EP
      BP = -AKP * DP
      GP = -CP * EP
      HP = CP * DP
      C1 = A * AP   + B * BP + C * CP
      IF ( C1 - 0.94 )  30, 31, 31
   30 IF ( C1 + 0.94)  28, 28, 29
   29 DELT = ACOS  (C1)
   33 DELTKM = 6371.0 * DELT
      C3 = (AP-D)**2 + (BP-E)**2 + CP**2 - 2.
      C4 = (AP-G) **2 + (BP-H)**2 + (CP-AK)**2 - 2.
      C5 = (A-DP)**2 + (B-EP)**2 + C**2 -2.
      C6 = (A-GP)**2 + (B- HP)**2 + (C-AKP ) ** 2 -2.
      DELTDG = 57.29577951 * DELT
      AZES = ATAN2 ( C3, C4 )
      IF ( AZES )  80, 81, 81
   80 AZES = 6.283185308  + AZES
   81 AZSE = ATAN2 ( C5, C6 )
      IF (AZSE )   70, 71, 71
   70 AZSE = 6.283185308  + AZSE
   71 AZESDG = 57.29577951 * AZES
      AZSEDG = 57.29577951 * AZSE
      goto 34
   31 C1 = (A - AP)**2 + (B-BP)**2 + (C-CP)**2
      C1 = SQRT (C1)
      C1 = C1 / 2.0
      DELT = ASIN (C1)
      DELT = 2.0*DELT
      GO TO 33
   28 C1 = (A + AP)**2 + (B + BP)**2 + (C + CP)**2
      C1 = SQRT (C1)
      C1 = C1 / 2.0
      DELT = ACOS (C1)
      DELT =  2.0 * DELT
      GO TO 33
   34 return
      END

      SUBROUTINE SVDD(A,KA,M,N,ISW,Q,U,KU,V,KV,W,IND)
      DIMENSION A(KA,N),U(KU,N),V(KV,*),Q(N),W(N)
      IND=30000
      MN=MIN0(M,N)
      IF(MN.LT.1.OR.M.GT.KA.OR.M.GT.KU) GO TO 490
      MU=ISW/2
      MV=MOD(ISW,2)
      IF(MU.LT.0.OR.MU.GT.1.OR.MV.LT.0.OR.MV.GT.1) GO TO 490
      IF(MV.EQ.1.AND.N.GT.KV) GO TO 490
      M1N=MIN0(M+1,N)
      DO 10 J=1,N
      DO 10 I=1,M
   10 U(I,J)=A(I,J)
      ANORM=0.
      G=0.
      DO 100 I=1,M1N
      Q(I)=0.
      W(I)=G
      IF(I.GT.M) GO TO 100
      IP1=I+1
      G=U(I,I)
      IF(I.EQ.M) GO TO 30
      SUM=0.
      DO 20 K=I,M
   20 SUM=U(K,I)*U(K,I)+SUM
      S=SUM
      G=-SIGN(SQRT(S),G)
      H=U(I,I)*G-S
      U(I,I)=U(I,I)-G
   30 Q(I)=G
      IF(I.EQ.N) GO TO 100
      IF(S.EQ.0..OR.I.EQ.M) GO TO 60
      DO 50 J=IP1,N
      SUM=0.
      DO 40 K=I,M
   40 SUM=U(K,I)*U(K,J)+SUM
      F=SUM/H
      DO 50 K=I,M
   50 U(K,J)=U(K,I)*F+U(K,J)
   60 G=U(I,IP1)
      IF(IP1.EQ.N) GO TO 100
      SUM=0.
      DO 70 K=IP1,N
   70 SUM=U(I,K)*U(I,K)+SUM
      S=SUM
      G=-SIGN(SQRT(S),G)
      H=U(I,IP1)*G-S
      U(I,IP1)=U(I,IP1)-G
      IF(S.EQ.0..OR.I.EQ.M) GO TO 100
      DO 90 J=IP1,M
      SUM=0.
      DO 80 K=IP1,N
   80 SUM=U(I,K)*U(J,K)+SUM
      F=SUM/H
      DO 90 K=IP1,N
   90 U(J,K)=U(I,K)*F+U(J,K)
  100 ANORM=MAX1(ABS(Q(I))+ABS(W(I)),ANORM)
c modified by D. Suetsugu 92 Nov 24
      tol=0.0
      TOL=DMACH(TOL)*ANORM
      IF(MV.EQ.0) GO TO 180
      DO 170 II=1,M1N
      I=M1N+1-II
      IF(I.EQ.N) GO TO 170
      IP1=I+1
      IF(I.EQ.M1N) GO TO 150
      IF(IP1.EQ.N.OR.W(IP1).EQ.0.) GO TO 130
      H=U(I,IP1)*W(IP1)
      DO 120 J=IP1,M1N
      SUM=0.
      DO 110 K=IP1,N
  110 SUM=U(I,K)*V(K,J)+SUM
      F=SUM/H
      DO 120 K=IP1,N
  120 V(K,J)=U(I,K)*F+V(K,J)
  130 DO 140 J=IP1,M1N
  140 V(I,J)=0.
  150 DO 160 J=IP1,N
  160 V(J,I)=0.
  170 V(I,I)=1.
  180 IF(MU.EQ.0) GO TO 260
      DO 250 II=1,MN
      I=MN+1-II
      IF(I.EQ.MN) GO TO 200
      IP1=I+1
      DO 190 J=IP1,MN
  190 U(I,J)=0.
  200 IF(Q(I).EQ.0.) GO TO 250
      IF(I.EQ.MN) GO TO 230
      H=U(I,I)*Q(I)
      DO 220 J=IP1,MN
      SUM=0.
      DO 210 K=IP1,M
  210 SUM=U(K,I)*U(K,J)+SUM
      F=SUM/H
      DO 220 K=I,M
  220 U(K,J)=U(K,I)*F+U(K,J)
  230 DO 240 K=I,M
  240 U(K,I)=U(K,I)/Q(I)
  250 IF(I.LT.M.OR.Q(I).EQ.0.) U(I,I)=U(I,I)+1.
  260 IF(ANORM.EQ.0.) GO TO 470
      DO 390 KK=1,M1N
      K=M1N+1-KK
      DO 360 IT=1,30
      DO 270 LL=1,K
      L=K+1-LL
      IF(ABS(W(L)).LT.TOL) GO TO 310
      IF(ABS(Q(L)).LT.TOL) GO TO 280
  270 CONTINUE
  280 C=0.
      S=-1.
      DO 300 II=2,L
      I=L+1-II
      F=-W(I+1)*S
      W(I+1)=W(I+1)*C
      IF(ABS(F).LT.TOL) GO TO 310
      G=Q(I)
      Q(I)=SQRT(G*G+F*F)
      C=G/Q(I)
      S=F/Q(I)
      IF(MV.EQ.0) GO TO 300
      DO 290 J=1,N
      X=V(J,I)
      V(J,I)=V(J,L)*S+X*C
  290 V(J,L)=V(J,L)*C-X*S
  300 CONTINUE
  310 IF(L.EQ.K) GO TO 370
      G=W(K-1)
      H=W(K)
      X=Q(L)
      Y=Q(K-1)
      Z=Q(K)
      F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(H*Y*2.)
      F=((X-Z)*(X+Z)+H*(Y/(SIGN(SQRT(F*F+1.),F)+F)-H))/X
      C=1.
      S=1.
      LP1=L+1
      DO 350 I=LP1,K
      H=W(I)*S
      G=W(I)*C
      W(I-1)=SQRT(F*F+H*H)
      C=F/W(I-1)
      S=H/W(I-1)
      F=X*C+G*S
      G=G*C-X*S
      H=Q(I)*S
      Y=Q(I)*C
      IF(MV.EQ.0) GO TO 330
      DO 320 J=1,N
      X=V(J,I-1)
      V(J,I-1)=V(J,I)*S+X*C
  320 V(J,I)=V(J,I)*C-X*S
  330 Q(I-1)=SQRT(F*F+H*H)
      C=F/Q(I-1)
      S=H/Q(I-1)
      F=G*C+Y*S
      X=Y*C-G*S
      IF(MU.EQ.0) GO TO 350
      DO 340 J=1,M
      Y=U(J,I-1)
      U(J,I-1)=U(J,I)*S+Y*C
  340 U(J,I)=U(J,I)*C-Y*S
  350 CONTINUE
      W(L)=0.
      W(K)=F
  360 Q(K)=X
      GO TO 480
  370 IF(Q(K).GE.0.) GO TO 390
      Q(K)=-Q(K)
      IF(MV.EQ.0) GO TO 390
      DO 380 J=1,N
  380 V(J,K)=-V(J,K)
  390 CONTINUE
      IF(N.EQ.1) GO TO 470
      K=MN
  400 L=1
      II=1
      LL=1
      DO 420 I=2,K
      IF(Q(I).GT.Q(L)) GO TO 410
      L=I
      GO TO 420
  410 II=I
  420 CONTINUE
      IF(II.EQ.LL) GO TO 460
      S=Q(II)
      Q(II)=Q(LL)
      Q(LL)=S
      IF(MV.EQ.0) GO TO 440
      DO 430 J=1,N
      S=V(J,II)
      V(J,II)=V(J,LL)
  430 V(J,LL)=S
  440 IF(MU.EQ.0) GO TO 460
      DO 450 J=1,M
      S=U(J,II)
      U(J,II)=U(J,LL)
  450 U(J,LL)=S
  460 K=II-1
      IF(K.GE.2) GO TO 400
  470 IND=0
      RETURN
  480 IND=20000
  490 RETURN
      END
C#NUMPAC#DMACH               REVISED ON 1984-11-30
      FUNCTION DMACH(x)
      DATA ONE,IFIRST/1.0e0,1/
c added by D. Suetsugu
      x=x
c
      IF(IFIRST.EQ.0) GO TO 20
      IFIRST=0
      EPS=ONE
   10 EPS=EPS*0.5e0
      IF(EPS+ONE.NE.ONE) GO TO 10
      EPS=EPS+EPS
   20 DMACH=EPS
      RETURN
      END
      SUBROUTINE MECHM(a,strk,dip,slip)
C***** INPUT : MOMENT TENSOR ELEMENT (SEE USAGE OF EIGRS)
C***** coordinate system is (r, thita, phai)
C***** OUTPUT: EIGENVALUES, EIGEN VECTORS, AND FAULT PLANE SOLUTIONS
C***** CODED FEB. 21, 1983 ,REVISED MAR. 16
      real*8 v(3,3),d(3),b(3,3)
      dimension a(3,3),dip(2),strk(2),slip(2)
      PI=ACOS(-1.e0)
      RAD= PI / 180.0
      SQ2H=SQRT(2.0)/2.0
      do 1 i=1,3
          do 1 j=1,3
    1         b(i,j)=dble(a(i,j))
      call jacobi(b,3,3,d,v,nrot)
      call eigsrt(d,v,3,3)
      ANT1=SQ2H*sngl(v(2,1)+v(2,3))
      ANT2=SQ2H*sngl(v(2,1)-v(2,3))
      AET1=ANT2
      AET2=ANT1
      ANF1=SQ2H*sngl(v(3,1)+v(3,3))
      ANF2=SQ2H*sngl(v(3,1)-v(3,3))
      ANR1=SQ2H*sngl(v(1,1)+v(1,3))
      ANR2=SQ2H*sngl(v(1,1)-v(1,3))
      AER1=ANR2
      AER2=ANR1
      dip(1)=ACOS(ANR1)/RAD
      dip(2)=ACOS(ANR2)/RAD
      SINDA1=SIN(dip(1)*RAD)
      SINDA2=SIN(dip(2)*RAD)
      SINST1=ANT1/SINDA1
      COSST1=ANF1/SINDA1
      SINST2=ANT2/SINDA2
      COSST2=ANF2/SINDA2
      strk(1)=ATAN2(SINST1,COSST1)/RAD
      strk(2)=ATAN2(SINST2,COSST2)/RAD
      RDA1=dip(1)*RAD
      RDA2=dip(2)*RAD
      RSTRK1=STRK(1)*RAD
      RSTRK2=STRK(2)*RAD
      SINSL1=AER1/SINDA1
      SINSL2=AER2/SINDA2
      TEMP1=-AET1-SINSL1*COS(RDA1)*SIN(RSTRK1)
      TEMP2=-AET2-SINSL2*COS(RDA2)*SIN(RSTRK2)
      COSSL1=TEMP1/COS(RSTRK1)
      COSSL2=TEMP2/COS(RSTRK2)
      SLIP(1)=ATAN2(SINSL1,COSSL1)/RAD
      SLIP(2)=ATAN2(SINSL2,COSSL2)/RAD
      IF(dip(1).GT.90.) THEN
      dip(1)=180.-dip(1)
      STRK(1)=STRK(1)+180.
      SLIP(1)=360.-SLIP(1)
      ENDIF
      IF(dip(2).GT.90.) THEN
      dip(2)=180.-dip(2)
      STRK(2)=STRK(2)+180.
      SLIP(2)=360.-SLIP(2)
      ENDIF
      IF(SLIP(1).GT.180.0) SLIP(1)=SLIP(1)-360.
      IF(SLIP(2).GT.180.0) SLIP(2)=SLIP(2)-360.
      IF(STRK(1).LT.0.0) STRK(1)=STRK(1)+360.
      IF(STRK(2).LT.0.0) STRK(2)=STRK(2)+360.0
      IF(SLIP(1).LT.-180.0) SLIP(1)=SLIP(1)+360.
      IF(SLIP(2).LT.-180.0) SLIP(2)=SLIP(2)+360.
      IF(STRK(1).GE.360.0) STRK(1)=STRK(1)-360.
      IF(STRK(2).GE.360.0) STRK(2)=STRK(2)-360.
      END
c
c
c
      SUBROUTINE JACOBI(A,N,NP,D,V,NROT)
      implicit real*8 (a-h,o-z)
      PARAMETER (NMAX=100)
      DIMENSION A(NP,NP),D(NP),V(NP,NP),B(NMAX),Z(NMAX)
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          TRESH=0.2*SM/N**2
        ELSE
          TRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=100.*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+G.EQ.ABS(D(IP)))
     *         .AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=0.5*H/A(IP,IQ)
                T=1./(ABS(THETA)+SQRT(1.+THETA**2))
                IF(THETA.LT.0.)T=-T
              ENDIF
              C=1./SQRT(1+T**2)
              S=T*C
              TAU=S/(1.+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=0.
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE
c     PAUSE '50 iterations should never happen'
        write(*,*) '50 iterations should never happen'
        read(*,'()')
      RETURN
      END
      SUBROUTINE EIGSRT(D,V,N,NP)
      implicit real*8 (a-h,o-z)
      DIMENSION D(NP),V(NP,NP)
      DO 13 I=1,N-1
        K=I
        P=D(I)
        DO 11 J=I+1,N
          IF(D(J).GE.P)THEN
            K=J
            P=D(J)
          ENDIF
11      CONTINUE
        IF(K.NE.I)THEN
          D(K)=D(I)
          D(I)=P
          DO 12 J=1,N
            P=V(J,I)
            V(J,I)=V(J,K)
            V(J,K)=P
12        CONTINUE
        ENDIF
13    CONTINUE
      RETURN
      END


