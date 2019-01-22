###############################################################################
# This short file is meant to be an example of the use of Perl to read    
# Seisan files.  There already exist a perfectly good Seisan to ASCII con-     
# verter in the Seisan distribution.  This file was written by a non-      
# programmer.  So there may be a better and shorter way to do this in Perl.
# I have avoided any cyrptic Perl structures or short cuts, for clarity.    
#                                                                           
# Much of the code here is to determine if the file was written by a SUN   
# or a PC and if the length of the data is two or four. If you are only    
# going to read one kind of file all this can be simpler.
#
# I have not given much thought to portablity but this code should run on
# and Perl machine with only a little modification mainly anywhere I use
# the "system" command I am calling a DOS function.
#
# Here is a summary of what the progam does:
# I read the file called "filenr.lis created when you do a "dirf" 
# I parse it and put it in an array and print it for you to choose one
# Then determine if the chosen file was written on SUN or PC
# I then read the header which is already in ACSII and write it out to the
# output file called "seiascP.out"
# then read the channel header
# Then read the data, invert it if I have to, convert it to ACSII
# format it so it is 8 samples to a line and append that line to "seiascP.out"
# do this for each channel
#
# To run the program place the files to convertand the seibinasc.exe in the
# same directory.  Do a "dirf" on the files
# invoke seibinasc.exe and do the prompts and look for the output in the
# same directory as "seiascP.out"
#
# If you have any questions feel free to contact me directly:            
#                                                                       
# Angel Rodriguez   angel(at)volcanbaru.com                               
#
#
# You get the free Perl at
#
# http://www.activestate.com/Products/ActivePerl/ 
#                                                                                                                                                
################################################################################

system ('cls');                 # Clear the DOS console
system ('del seiascP.out');     # get rid of old output files

open ("filenr",'filenr.lis') || die "Please do a dirf first";

@filenr = ();                   # intialize array (not really needed)
while ($_ = <filenr>) {         # look at each line of filenr.lis
    push(@filenr,$_);           # build the array
}
chomp @filenr;                  #get rid of the last char (CL/LF) of each element of the array
pop @filenr;                    #get rid of an empty line at the end of filenr.lis

print join ("\n", @filenr);     # print the array
print "\n\n\n";                 #three blank lines for readablity

print "Choose a file to convert    ","\n\n";
$numfile = @filenr;             # $numfile equal the number of elements in the array
$what = <STDIN>;
if ($what < 1 || $what > $numfile) { # check to see if user input is in range
    print "Choice out of range, I quit!";
    exit;
}
chomp $what;                    #get rid of the CR/LF
$what--;                        # decrement $what because array counts from 0

$file_to_convert = substr($filenr[$what],6); # get the name of the choosen file

open ("seifile", $file_to_convert) || die "Strange, I can't find that file! Do another dirf";
binmode "seifile";                  # set to binary mode, you must do this
                                    # seiascP.out is the final results
open (seiasc_out, ">>seiascP.out"); # open seiascP.out for appending.
binmode seiasc_out;                 # once again in binary mode

seek ("seifile", 0, 0);             # position pointer are begining of file
read ("seifile", $endian, 2);       # get the first two characters

if ($endian  =~ /P/) {              # see the seisan file format in seisan manual appendix
	$endian = 80;               # If the P is present the file was written on a PC
}                                  
else {
	$endian = 0;                # 0 if written by SUN
}

$stepper = 0;
seek ("seifile", 0, 0);             # place pointer at begining of file
read ("seifile", $var1, 88);        # read the first 88 characters
$numchannels = substr($var1, 34,3); # fish out the nuber of channels (see seisasn appendix)

# In this follow while loop I write the file header which must be at least 12
# line long.  What I do is to print all the line in the file until I find the
# first channel header which I bracketed by "1040" the length of the header.
 
while (1) { #the only way out of this loop is through the "last" statemnts

    if ($endian == 0){              # this is the SUN byte order
        seek ("seifile", $stepper + 2, 0);
        read ("seifile", $temp, 1);        # read one byte
        seek ("seifile", $stepper + 3, 0);
        read ("seifile", $tempx, 1);       # read one byte
        $temp = $tempx.$temp;              # join the 2 bytes in reverse order

        if (unpack("s",$temp) == 1040) { # "s" = reads the binary as a signed short value
            last 
        }
    }
    if ($endian == 80) {             # this is the PC byte order
        seek ("seifile", $stepper, 0);
        read ("seifile", $temp, 2);

        if (unpack("s",$temp) == 1040) { # "s" = reads the binary as a signed short value
            last
        }
    }
    read ("seifile", $oneline, 88);   
    print seiasc_out substr($oneline,2,80),"\n"; # output the first line to the file    
    $stepper = $stepper + 88;                     # move the pointer to the next line
}

# this next while loop is the heart of the program it finds and prints the channel headers
# and the data so that it looks like the output of the program seisac.exe which
# comes in the seisan distribution.

# here the pointer $stepper is at the begining of the first channel header.

# For the number of channels in the file go thru the following while loop 

while ($numchannels > 0) {

    $data_length = $number_samples = $counter = 0; # clear variables

    seek("seifile",$stepper+4,0); #skip first 4 bytes
    read ("seifile",$var2,80);    # read one line 
    $data_length = substr($var2,76,1); # get the data lenght for this channel
    if ($data_length == " ") {         # a blank means it is a 2 byte data length
        $data_length = 2;              #  nordic ' ' == 2
    }
    
    $number_samples = substr($var2,44,6);

    if ($data_length == 4) {
        $unpack_type = "l";            # "l" is the unpack type for a signed long
    }
    else {
        $unpack_type = "s";            # "s" is the unpack type for a signed short
    }    
                                       # unpack is a Perl function to convert binary data
    $cnt = 1;                               # start at 1 instead of 0
    while ($cnt != 14) {                    # in this while loop I build the channel
        seek("seifile",$stepper + 4,0);     # header which is always 1040 long
        read ("seifile",$var2,80);          # plus 4 bytes at begining and end
        print seiasc_out $var2,"\n";        # write the line to disk
        $stepper = $stepper + 80;
        $cnt++
    }                                       # go get another line
    
# lines of data in the ASCII file are in this format, seven per line  
#        439        491        451        465        473        458        460
# so below I build one line from each seven samples 

    $stepper = $stepper + 12;               # add 12 which is 4 byte at beginning
                                            # 4 bytes at the end and 4 bytes of new header
    while ($counter < $number_samples) {    # while number of samples in channel        
        $datacounter = 1;                   # initialize this variable
        while ($datacounter < 8) {          # the ASCII format has 7 smaples per line
            if ($counter == $number_samples) { # check to see if done with one line
                $stepper = $stepper + ($data_length * 2); # move stepper to next line
                $counter++;
                $datacounter++;
                last                        # leave this loop and write line to disk
            }
                     
            seek ("seifile",$stepper,0);    # position pointer
            read ("seifile",$var2,$data_length); # read one sample
            
            # If the file was written by a big-endian computer the btye order needs to be flipped    
            if ($endian == 0) {    
                if ($data_length == 4) {
                    $temp1 = substr($var2,3,1).substr($var2,2,1).substr($var2,1,1).substr($var2,0,1);
                }
                else {
                    $temp1 = substr($var2,1,1).substr($var2,0,1);
                }
            }
            else {                          # if written on PC come here
                $temp1 = $var2;
            }
            
            # the result of "unpack' is a list not a scalar. So I show it as an array element $unp[]
            $unp[0] = unpack("$unpack_type",$temp1);      # convert the sample to ASCII   
            $tl = length $unp[0];                         # get the lenght of the sample 
            # the line below pads the sample with spaces to make it 11 chars long
            $ds = " " x (11 - $tl).$unp[0];               # pad to 11 characters so that 
                                                          # ASCII format works
            print seiasc_out $ds;                         # write to disk

            $stepper = $stepper + $data_length;
            $counter++;
            $datacounter++;
        }
        print seiasc_out "\n";                            # add a CR/LF
        if ($datacounter == 8 && $counter == $number_samples) { # are we at the end?
            $stepper = $stepper + ($data_length * 2); # if not move stepper forward
        }

    }       
    $stepper = $stepper - $data_length; # mmove stepper forward
    $numchannels--; # decerment the number of channels
}

print "\n","Look for the converted file in seiascP.out","\n"; # done!!


