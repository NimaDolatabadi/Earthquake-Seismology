      program sample_udp_message

c  written by oyvind natvik
c  adopted as a seisan sample program by jh
c
c  the program is started in two instances in two different
c  terminal windows. at the start, each program gives its port number
c  and  which is then entered in the other program. the user
c  must also choose which program sends and which listen. messages
c  can then be sent from one program to the other. a quit terminates
c  both programs. a flip reverses which program send and which listen.


      implicit none
      character*128 message
      character*1 mode              ! listen or send
      integer myport, port          ! port number of this program and other program
      integer msglen
c
c     Initialize the network and print the port number we listen to.
c
      print '(a)', "Initializing network..."
      call init_network(myport)
      write(*,*) 'This program listens on port:',myport    
c
c     Send UDP message to another program running on localhost or listen
c     to another program
c
      print '(a)', "Enter the other program's port number:"
      read (*,*) port
      write(6,*)'This program should listen(l) or send (s) only'
      read(5,'(a)') mode

 5    continue

c
c  Enter loop to only receive messages
c
      if(mode.eq.'l') then
         print '(a)', ""
         print '(a)', "Program has entered listen mode."
         print '(a,i6)', "Now listening on port number ", myport
         print '(a)', "Program will exit if 'quit' is received."

         do while (message .ne. 'quit')
            call get_udp_msg(message, msglen)
            if (msglen .gt. 0) then
               print '(2a)', "Received message: ", message
               print '(a,i2)', "Message length: ", msglen
c
c   check if change mode
c
               if(message(1:4).eq.'flip') then
                  mode='s'
                  goto 5
               endif

            end if
            call ms_sleep(100)
         end do 
      endif
c
c   enter loop to only write messages
c
      if(mode.eq.'s') then
         print '(a)', "Program has entered sending mode."
         print '(a,i6)', "Now sending to port number ", port
         print '(a)', "Program will exit if 'quit' is entered"
         print '(a)', "Programs will flip modes if 'flip' is entered"

         do while (message .ne. 'quit')
            print '(a)', "Enter the message you want to send:"
            read(*,*) message      
            call send_udp_msg(port, message)
            print '(a)', "Message has been sent"
            if(message(1:4).eq.'quit') goto 10
c
c   check if flip mode
c
            if(message(1:4).eq.'flip') then
               mode='l'
               goto 5   ! start again
            endif
            print '(a)', ""
         enddo
      endif
c
c     Close the network
c
 10   continue

      call close_network()
      end
