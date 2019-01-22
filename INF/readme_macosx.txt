
README: TO INSTALL SEISAN AND SEISAN EXPLORER ON MAC
	(plus very short intro to the terminal for anyone who never used it on 
	Mac before)

	by Felix Halpaap (felix.halpaap@uib.no, 5.2.2016)

If you have never or hardly used the terminal (Launchpad -> Terminal) on Mac
before, you will need to install the following tools and packages:

brew package manager: 
	Copy the following line in the Terminal and press Enter:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

gcc compilers: 
	Upon successful installation of brew, execute "brew install gcc" from 
	the terminal.
	
XCODE: Either install it from the Mac store under
	https://itunes.apple.com/us/app/xcode/id4977998x5?ls=1&mt=12#
	or from the terminal when you are automatically asked for it.
	
X11 Quartz: 
	Download the dmg-file, doubleclick & install:
	https://dl.bintray.com/xquartz/legacy-downloads/SL/XQuartz-2.7.8.dmg
	
SEISAN
	1. Download seisan_v10.3_macosx_64.tar.gz from 
	ftp://ftp.geo.uib.no/pub/seismo/SOFTWARE/SEISAN/
	
	2. In the terminal, go to your download folder:
	If you never used the terminal before, there are two commands you need 
	for that:
		a) ls: list files
		b) cd: change directory. 
	So, upon starting the terminal, just enter "ls", and you'll probably see
	folders called "Documents" and "Downloads" (alternatively, their pendants
	in your Mac's respective language (e.g. "Nedlastinger"). Move to the 
	directory where you downloaded Seisan to (probably Downloads) by entering
	"cd Downloads". 
	
	3. Create a new directory called Seisan with "mkdir Seisan" (make direc-
	tory), move the archive in that folder with 
	"mv seisan_v10.x_macosx_64.tar.gz Seisan/" and cd into the folder with
	"cd Seisan". Unpack the Seisan archive with 
	"tar -xzf seisan_v10.x_macosx_64.tar.gz".
	
	4. Move the new seisan_v10.x_macosx_64-directory to a location of your
	preference and cd into it. 
	
	5. I would advice you to start working in the bash-shell on Mac (shells 
	are slightly different languages within the terminal, which mostly dif-
	fer in their syntax). Fix your standard shell to bash with
	"sudo chsh -s /bin/bash username" - enter your admin password to fix the
	change. Restart the terminal afterwards, and check if you did actually
	log in with bash by typing "echo $0". If the answer is bash, you're good.
	
	6. Open your .bashrc-file (this file contains preferred settings for your
	personal bash-login. If it's your first time to the Mac terminal, it 
	probably does not exist, so you need to create it by typing 
	"touch ~/.bashrc". The tilde leads straight to your home directory, where
	.bashrc should be located. The dot in front of its name means it is a 
	hidden file, and you will only see it in the terminal when you enter 
	"ls -a" instead of "ls" in the home directory. Open the file e.g. with
	the text editor nano, by executing "nano ~/.bashrc". Right now, we only 
	need one line in there: 
	source YourSeisanDirectory/COM/SEISAN.bash
	Replace YourSeisanDirectory with the actual path to your Seisan direc-
	tory. Save and exit with ctrl+x, enter, y, enter. You can get the full 
	path by cd'ing into the directory and then entering "pwd" (print working
	directory).
	
	7. You need to add/change two entries in COM/SEISAN.bash:
		ca. line  7: export SEISARCH=macosx
		ca. line 10: export SEISAN_TOP="/Users/yourVeryOwnPath/Seisan"
	Change the 2nd line appropriately for your own Seisan path.
	
	8. Reopen the terminal, or just reload the bash presets by executing "bash"
	in the terminal. If you enter "echo $SEISAN_TOP" now, you should get the
	correct path to Seisan, while "echo $SEISARCH" should give you "macosx".
	
	9. Type "pr", then enter in the terminal - this is now an alias for the 
	Seisan Program and source code directory. You can compile all Seisan 
	programs by entering 
	"make clean"
	"make all"
	from this directory. In version v.10.3 of seisan, there are unfortunately
	two programs which do not compile on Mac, for which reason we need to
	take them out of the compilation so that they don't cause any errors
	(getstressdrop.c:166:3: error: non-void function 'main' should return a 
	value).	For this purpose, open the Makefile in a text editor (e.g. "nano
	 Makefile") and comment out (write a # in the beginning of the line) all
	lines which include references to "getstressdrop" and other functions 
	that might cause errors.
	
	Test if SEISAN works by moving to the Seisan Work directory "wo", and 
	enter: "eev 199606 TEST", which should open up an event from the TEST
	database. If you execute "po" now, you should be able to open up an 
	xwindow with a few seismograms in the Seisan Explorer. 
	
	If this does not work, the error that I saw most often is a problem with
	the correct link to the fortran libraries, something like:
	" dyld: Library not loaded: /usr/local/lib/libgfortran.3.dylib"
	To fix this, search for the actual path of the library with the following
	command: "mdfind -name "libgfortran.3.dylib". Now create a symbolic link
	which connects the location where Seisan seems to expect the gfortran 
	library with its actual location: 
	"ln -s pathToActualLibrary pathToExpectedLibrary", e.g.
	"ln -s /usr/local/Cellar/gcc/4.9.2_1/lib/gcc/4.9/libgfortran.3.dylib 
	/usr/local/lib/libgfortran.3.dylib"
	Try "po" again within eev, and check if the previous error disappeared.
	If another, similar but different error appears, repeat the symbolic 
	linking for that 2nd missing library as well.
	
	I hope that your Seisan and Seisan Explorer are both running now - if 
	you have any further errors, contact your trusted Mac or Linux person!

