%SAC Command Reference Manual                                   MAT
%
%SUMMARY:
%Copy SAC workspace into Matlab and either execute a user-specified 
%m-file or else get a Matlab prompt for interactive manipulation.
%The SAC workspace is updated with changes made to the data after
%the return from Matlab. 
%
%SYNTAX:
%MAT [mfile]
%
%
%DESCRIPTION:
%The mat command allows processing of SAC data from within SAC 
%using the Matlab (Version 5) engine and any user-written m-files. 
%When this command is executed, the SAC workspace is copied into 
%the following Matlab variables:
%   SeisData      --- an M-points by N-traces array of waveforms.
%   SACdata   	 --- an M-element structure array containing the 
%                     header information from the SAC workspace.
%   BlackBoard    --- a structure array containing any blackboard
%   		     variables.
%
%SEISDATA:                    
%If the SAC data are time-domain, the SeisData array is real. Other
%wise it is complex. However, be aware that the default behavior of
%SAC's fft command is to produce transformed data in amplitude-phase
%format while in Matlab, the data will be treated as real-imaginary.
%The easiest way around that is to use the rlim option with SAC's
%fft. 
%
%You must return trace data from Matlab to SAC in the same domain
%as it was in before the mat command was executed. Otherwise,
%changes to the trace data made in Matlab will not be preserved.
%Also, you must not change the length of the traces in Matlab.
%
%SACDATA:
%The SACdata structure array contains the following elements:
%    times
%    station
%    event
%    user
%    descrip
%    evsta
%    llnl
%    response
%    trcLen
%
%Most of these elements are themselves structures and their members
%are as follows:
%times	station	event	user	 descrip  evsta	llnl		
%-----	-------	-----	------   -------  -----	----
%delta	stla	evla	data(10) iftype   dist	xminimum
%b	stlo	evlo	label(3) idep     az	xmaximum
%e	stel	evel		 iztype   baz	yminimum
%o	stdp	evdp		 iinst    gcarc	ymaximum
%a	cmpaz	nzyear		 istreg		norid
%t0	cmpinc	nzjday		 ievreg		nevid
%t1	kstnm	nzhour		 ievtyp		nwfid
%t2	kcmpnm	nzmin		 iqual		nxsize
%t3	knetwk	nzsec		 isynth		nysize
%t4		nzmsec
%t5		kevnm
%t6		mag
%t7		imagtyp
%t8		imagsrc
%t9
%f
%k0
%ka
%kt1
%kt2
%kt3
%kt4
%kt5
%kt6
%kt7
%kt8
%kt9
%kf
%
%response is a 10-element array, and trcLen is a scalar. Thus, to 
%reference the begin time for the 10th trace in memory you would
%write:
%SACdata(10).times.b 
%
%To reference the first 4 characters of the station name for the 
%first trace, you would write:
%SACdata(1).station.kstnm(1:4)
%
%BLACKBOARD
%The BlackBoard variable is also a structure array which will be
%missing if you have no numeric or string black board variables in
%the SAC workspace. Otherwise there is an element for each black
%board variable. Each element is a structure containing a name
%and a value. You cannot create new black board variables in
%Matlab (If you do, the changes will not be saved). However, you
%can modify the ones passed from SAC to Matlab. So, if your Matlab
%script will create a number of output quantities that you want
%to store in SAC's blackboard, create the variables in SAC before
%executing the mat command.
%
%MATLAB PATH:
%By default, the Matlab path available to you from within SAC will
%consist of the current directory, ~/matlab and the $MATLAB
%hierarchy. You can add an additional directory to the Matlab path
%from within SAC using the SETMAT command (SETMAT directoryName).
%Also, from within Matlab, you can modify the search path command
%using the path command. (Type help path for more information).
%
%EXITING THE MATLAB INTERPRETER:
%There are two ways to exit the Matlab interpreter and return to SAC.
%You can type "exitmat" at the  SACMAT>> prompt. This will return
%you to SAC and leave the engine running. This can be useful if you
%intend to move back and forth between the two environments
%frequently, since there is no delay associated with starting the
%Matlab engine. However, a Matlab license will be tied up while you
%are running SAC and this may inconvenience other users who cannot
%start a session. To exit the Matlab interpreter and/or close the 
%engine, type "closemat" at either the SACMAT>> or the SAC> prompt.
%
%HEADER CHANGES:
%Potentially all. User is responsible for consistency of changes.
%
%EXAMPLE:
%Execute an m-file that converts the data to their absolute
%values. Assume the m-file is named absv.m and contains the one line
%SeisData=abs(SeisData);
%       u:  mat absv
%       
%NOTES:
%You may find it easier to develop a complex m-file directly from
%Matlab rather than from within the SAC-Matlab environment. The
%primary reasons are that there is no command line recall at the
%SACMAT>> prompt and because SACMAT does not trap ^C (used to stop
%errant m-files in Matlab). The easiest way to do this is to load
%your data into SAC, start the Matlab engine with mat, and then
%type save. This will save the workspace in a file called matlab.mat. 
%You may then start a normal matlab session, and type load. This will 
%load matlab.mat and you may then develop your application within
%Matlab.
%
%The entire range of plotting commands are available. However, if
%you execute your m-file from SAC (i.e. mat mfilename) Matlab will 
%return to SAC immediately after executing the last command in the 
%m-file. Therefore, if you want to look at your plots created in 
%Matlab either execute the m-file from the Matlab command line, or
%execute a pause in your Matlab script. 
%.
%.
%.
%plot(SeisData)
%pause(10)	 	      
%	
%
%LATEST REVISION:
%Aug  9, 1997 (Version 00.56a)
%
