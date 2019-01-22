//
//   Network support library for SEISAN programs.
//   Supports two-way communications using the UDP protocol.
//   Author: Oyvind Natvik
//
//   Version: 1.2
//   Last update: 12.01.2017, Ø.N.
//
// 2017-12-21 pv: added changes for compilation on sun
// 2018-05-11 on: added changes for compilation on macosx
//

#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
SOCKET Socket;
#endif

#ifdef __linux__
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>
int Socket;
#endif

#ifdef __APPLE__
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
int Socket;
#endif

#ifdef __sun__
#include <sys/socket.h>
int Socket;
#endif

#include <stdio.h>
#include <stdbool.h>

void UnPadString(char*, int);



// **********************************************************************
// Creates a network socket for incoming connections. Call once at start
// of program. Socket will bind to localhost interface only. Function
// returns a random port number that program will listen to for incoming
// UDP connections. Function can be called from C and fortran programs:
// Fortran: call init_network(my_port), C: init_network_(&my_port)
// **********************************************************************
void init_network_(int *server_port)
{
   struct sockaddr_in sain;

#ifdef __WIN32__
   WSADATA wsaData;
   u_long iMode = 1;
   WSAStartup(0x202, &wsaData);
   Socket = socket(AF_INET, SOCK_DGRAM, 0);
   ioctlsocket(Socket, FIONBIO, &iMode);
#endif
#if __linux__ || __APPLE__ || __sun__
   Socket = socket(AF_INET, SOCK_DGRAM, 0);
#endif
   memset((char*) &sain, 0, sizeof(sain));
   sain.sin_family = AF_INET;
   sain.sin_port = htons(0);
   sain.sin_addr.s_addr = inet_addr("127.0.0.1");
#ifdef __WIN32__
   int sain_size = sizeof(sain);
   bind(Socket, (SOCKADDR*)(&sain), sain_size);
   getsockname(Socket, (SOCKADDR*) &sain, &sain_size);
#endif
#if __linux__ || __APPLE__ || __sun__
   socklen_t sain_size = sizeof(sain);
   bind(Socket, (struct sockaddr*) &sain, sizeof(sain));
   getsockname(Socket, (struct sockaddr*) &sain, &sain_size);
#endif
   *server_port = ntohs(sain.sin_port);
}



// ***************************************************************
// Send an UDP message to a program running on localhost. Correct
// port number for the listener must be given. This function can
// only be called from Fortran: call send_udp_msg(port, msg)
// ***************************************************************
void send_udp_msg_(int *port, char *msg, int msgsize)
{
#ifdef __WIN32__
   SOCKET Sock;
#endif
#if __linux__ || __APPLE__ || __sun__
   int Sock;
#endif
   UnPadString(msg, msgsize);
   struct sockaddr_in sain;
   Sock = socket(AF_INET, SOCK_DGRAM, 0);
   memset((char*) &sain, 0, sizeof(sain));
   sain.sin_family = AF_INET;
   sain.sin_port = htons(*port);
   sain.sin_addr.s_addr = inet_addr("127.0.0.1");
#ifdef __WIN32__
   connect(Sock, (SOCKADDR*) &sain, sizeof(sain));
   send(Sock, msg, strlen(msg), 0);
   closesocket(Sock);
#endif
#if __linux__ || __APPLE__ || __sun__
   connect(Sock, (struct sockaddr*) &sain, sizeof(sain));
   send(Sock, msg, strlen(msg), MSG_DONTWAIT);
   close(Sock);
#endif
}



// *********************************************************************
// Receive an incoming UDP message (max 512 bytes long). Returns length
// of message in variable 'msglen'. Returns zero if no message has been
// received. Function is nonblocking. This function can only be called
// from Fortran: call get_udp_msg(msg, msglen)
// *********************************************************************
void get_udp_msg_(char *msg, int *msglen, int msgsize)
{
   char udp_data[512];

#ifdef __WIN32__
   *msglen = recv(Socket, udp_data, sizeof(udp_data), 0);
   if (*msglen == SOCKET_ERROR) { *msglen = 0; return; }
   memset(msg, 32, msgsize); memcpy(msg, udp_data, *msglen);
#endif
#if __linux__ || __APPLE__ || __sun__
   ssize_t datasize = recv(Socket, udp_data, sizeof(udp_data), MSG_DONTWAIT);
   if (datasize == -1) { *msglen = 0; return; }
   memset(msg, 32, msgsize); memcpy(msg, udp_data, datasize);
   *msglen = datasize;
#endif
}



// *********************************************************************
// Receive an incoming UDP message (max 512 bytes long). Returns length
// of message. Returns zero if no message has been received. Function is
// nonblocking. This function can only be called from C:
// msglen = get_udp_msg(msg)
// *********************************************************************
int get_udp_msg(char *msg)
{
   char udp_data[512];
#ifdef __WIN32__
   int msglen = recv(Socket, udp_data, sizeof(udp_data), 0);
   if (msglen == SOCKET_ERROR) return 0;
   udp_data[msglen] = 0; strcpy(msg, udp_data);
#endif
#if __linux__ || __APPLE__ || __sun__
   ssize_t msglen = recv(Socket, udp_data, sizeof(udp_data), MSG_DONTWAIT);
   if (msglen == -1) return 0;
   udp_data[msglen] = 0; strcpy(msg, udp_data);
#endif
   return msglen;
}



// *********************************************************
// Close the network. Must be called when the program exits.
// This function can be called from C and fortran:
// Fortran: call close_network(), C: close_network_()
// *********************************************************
void close_network_()
{
#ifdef __WIN32__
   closesocket(Socket);
   WSACleanup();
#endif
#if __linux__ || __APPLE__ || __sun__
   close(Socket);
#endif
}



// *********************************************************
// Sleep function for use from fortran.
// Can be called from fortran: call ms_sleep(milliseconds)
// *********************************************************
void ms_sleep_(int *millisec)
{
#ifdef __WIN32__
   Sleep(*millisec);
#endif
#if __linux__ || __APPLE__ || __sun__
   usleep((useconds_t)(*millisec*1000));
#endif
}



// *********************************************************
// Function converts a fortran string to a character string.
// *********************************************************
void UnPadString(char *string, int strsize)
{
   bool done = false;
   int index = strsize;

   // Start at end of string. Find first character that is not
   // a space. Replace the last space with a NULL terminator.
   while (!done) {
      index--;
      if (string[index] != 32 && string[index] != 0) {
         if (index == strsize - 1) {
            string[index] = 0;
         } else {
            string[index + 1] = 0;
         }
         done = true;
      }
      // Check if we have processed the whole string.
      if (!index && !done) { string[0] = 0; return; }
   }
}
