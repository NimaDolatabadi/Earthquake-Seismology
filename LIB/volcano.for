
      subroutine select_volcano_subclass(sub_class)
c
c
c   changes
c
c   oct 18 2002  jh  : small change in write statement to compile on pc
c
      implicit none
 
      include 'seisan.inc'
      include 'libsei.inc'                 ! Library definitions & data defns.

c-- User entered sub-class flag
      CHARACTER*(*)     sub_class
c-- sub class code
      CHARACTER*6       s_class(60)
c-- sub class number
      INTEGER           n_class(60)
c-- user entered number
      INTEGER           n_select
c-- sub-class description array
      CHARACTER*40      description
c-- true if sub_class is in s_class()
      LOGICAL           valid_class
c-- full pathname of VOLCANO.DEF
      CHARACTER*80      vol_file
c-- comment text
      character*80 comment
c-- flag
      logical exist
c-- counter
      integer i,code,read2
c-- path to seismo
      character*60 top_directory
c-- directory separator
      character*1 dchar
c-- computer type
      logical pc,sun,linux
c-- function to get length of character string
      integer seiclen
c-- flag for special events
      logical star_flag

      call computer_type(sun,pc,linux)
      call dir_char(dchar)   ! directory separation character
      call topdir(top_directory)


C bjb 18/3/97
C If a local volcanic event then we want register a sub class defined in
C the file VOLCANO.DEF located in the DAT directory.
C The format of this file will be one line of text (80A) followed by
C i2,1x,6A,1X,40A for number, code, space, description
c
c next lines moved down to be run only if 'V' LO, 04.97
c
      valid_class=.FALSE.
      sub_class='  '
C Create path name for VOLCANO.DEF on sun or PC
      vol_file=top_directory(1:60)
      vol_file(seiclen(top_directory)+1:
     *        seiclen(top_directory)+1)=dchar
      vol_file(seiclen(vol_file)+1:seiclen(vol_file)+3)='DAT'
      vol_file(seiclen(vol_file)+1:seiclen(vol_file)+1)=dchar
      vol_file(seiclen(vol_file)+1:)='VOLCANO.DEF'

      CALL sei open(check$,
     &                  ' ',
     &                  vol_file,
     &                  0,
     &                  exist,
     &                  code)
      IF(.not.exist) THEN
         WRITE(6,*)
     *   'Warning: Cannot find VOLCANO.DEF file in DAT dir'
         sub_class='NONE'
         return    
      ENDIF
c
C If we are here then we want to open up VOLCANO.DEF file and print to
C the screen and let the user select which code he wishes to enter
C in line 1 col 56
c
      CALL sei open(old$,
     &                ' ',
     &                 vol_file,
     &                read2,
     &                exist,
     &                code)
 10   READ(read2,'(a80)') comment
      WRITE(6,*)' '
      WRITE(6,*) comment

      WRITE(6,*)'---------------------------------------'
      Write(6,*)'Number Sub-Class Description'
      WRITE(6,*)'---------------------------------------'
      DO i=1,60
         READ(read2,'(i2,1x,a6,1x,a40)') n_class(i),s_class(i),
     &                                    description
         WRITE(6,'(4x,i2,1x,a6,4x,40a)') n_class(i),s_class(i),
     &                                    description

         IF(description.eq.'QUIT') GOTO 20
      ENDDO
 20   CONTINUE
      WRITE(6,*)'---------------------------------------'
      WRITE(6,*) 'Please enter number for sub-class : '
cjh      WRITE(6,*) '(Negative number to add \'*\' to sub-class)'      
      WRITE(6,*) '(Negative number to add to sub-class)'
      call flush (6)
      star_flag=.false. 
      READ(5,'(i2)') n_select
      if (n_select.lt.0) then
        n_select=abs(n_select)
        star_flag=.true. 
      endif
C Test for valid sub_class code number
      DO i=1,60
         IF(n_select.eq.n_class(i)) then
                    valid_class=.TRUE.
                    sub_class=s_class(i)
c insert star if special event
                    if (star_flag) 
     &          sub_class=sub_class(1:seiclen(sub_class))//'*'
                    WRITE(6,*)'Sub-class ', sub_class, ' selected.'
         endif
      ENDDO
      IF(.not.valid_class) THEN
         WRITE(6,*)'Invalid Sub-class code. Try again'
         REWIND(read2)
         GOTO 10
      ENDIF
      CALL sei close(close$,read2,code)

      return
      end

