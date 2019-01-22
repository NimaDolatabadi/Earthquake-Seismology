C
C   PROGRAM TO DELTE FILES USING LIST OF FILE CREATED WITH DIRF
C
C   JENS HAVSKOV     LAST UPDATE AUGUST 14, 1987
c   
c   sep 11 92 by j.h. : unix version
c   jan 8, 2003 jh : also delete all files in filenr.lis or index file
C
C

      IMPLICIT NONE		
      CHARACTER*1	ANSWER	
      CHARACTER*80      FILE,file1
      CHARACTER*80	QUESTION
      logical 		del_all


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      del_all=.false.
      write(6,*) ' Give name of index file to delete all, else return'
      read(5,'(a)') file
      if(file.ne.' ') del_all=.true.


C
C   LOOP TO DELETE FILES
C

C
C     INPUT FILENAME TO DELETE if not all deleted
C
      if(.not.del_all) then
 1       continue
         QUESTION='  FILE NUMBER TO DELETE, ? TO GET LIST'
         CALL FILENAME(QUESTION,FILE)	
         IF(FILE.EQ.'EOF') GO TO 5		
         WRITE(6,*)
         WRITE(6,205)FILE
 205     FORMAT('  SURE YOU WANT TO DELETE FILE (Y/N): ',A60)
         READ(5,'(A1)') ANSWER
         IF(ANSWER.EQ.'Y'.OR.ANSWER.EQ.'y') THEN
            OPEN(1,FILE=FILE,STATUS='OLD')
            CLOSE(1,STATUS='DELETE')
            WRITE(6,200) FILE
 200        FORMAT('  FILE DELETED: ',A60)
         ENDIF
         GO TO 1
      else                                  ! all deleted
         open(1,file=file,status='old')
 2       continue
         read(1,'(7x,a)',end=5) file1
         if(file1.eq.' ') goto 5
         open(2,file=file1,status='old',err=3)
         close(2,status='delete')
         write(6,'(1x,a,a)')'Deleted: ',file1(1:60)
         goto 4
 3       continue 		
         write(6,'(1x,a,a)')'Not present: ',file1
         stop
 4       continue
         goto 2
      endif
C
 5    CONTINUE
      ENd
