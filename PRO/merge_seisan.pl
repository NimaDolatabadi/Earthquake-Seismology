#!/usr/bin/perl -s
#
# merge_seisan.pl   
#
# operates on SEISAN S-files, CAT Files and the database
#
# Author: Frederik Tilmann, 2002
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $LastChangedBy: tilmann $
# $LastChangedDate: 2009-10-26 11:36:57 +0000 (Mon, 26 Oct 2009) $
# $Rev: 50 $
#
# 28/ 9/2008: Option -d=gmtd for output of both hypocentres in lon, lat, depth format (FJT)
#  1/11/2008: Option -d=bias for calculation of bias vector between boths sets
#  4/11/2008: Create output suitable for psvelo
#  8/2/2011:  Select by distance
# 20/5/2011:  Allow m0 (magnitude_obs) as (inofficial) magnitude type
# 10/11/2013: Extend distance option to select by hypocentral distance

BEGIN {
package seisutil;
require Exporter;

use strict 'vars';
use File::Temp qw(tmpnam);

our @ISA = qw(Exporter);
our @EXPORT = qw(jul2epoch epoch2jul julday calday deg2km deg2rad distaz slurp event2epoch);
our $VERSION = 0.1 ;

# jul2epoch(yr,jday,hour,min,ss)
#   returns number of seconds after 1st January 1970
# correct between 1/1/1970 and 31/12/2099
# does not allow for leap seconds
sub jul2epoch {
  my ($yr,$jday,$hour,$min,$ss)=@_ ;
  my ($epoch);

# guess correct century
  if ($yr>=70 && $yr<100)
    { $yr += 1900;}
  elsif ($yr>=0 && $yr<70)
    { $yr += 2000 ; }

# epoch number of days due to 4 year blocks after 1900
  $yr=$yr-1900;
  $epoch=int($yr/4)*(4*365+1);
# add number of days due to remaining years (2nd term correcting for 366 day year)
  $epoch+=($yr % 4)*365 + ($yr%4 >=1 ? 1 : 0);
# substract number of days from 1/1/1900 to 1/1/1970
#         1900-1967    1968 1969
  $epoch-=17*(4*365+1)+366+ 365 ;
# add jdays
  $epoch+=($jday-1) ;

###  print STDERR "LIB DEBUG: ((($epoch*24+$hour)*60+$min)*60)+$ss\n";
  $epoch=((($epoch*24+$hour)*60+$min)*60)+$ss ;
  return($epoch) ;
}

# ($yr,$jday,$hour,$min,$ss)=epoch2jul($epoch)
# inverse of jul2epoch, except it always returns year as 4-digit number
sub epoch2jul {
  my ($epoch) = @_ ;
  my ($yr,$jday,$hour,$min,$ss,$days,$four_year,$year);
  $days=int $epoch / 86400 ;
  $epoch -= $days*86400 ;
  $hour=int $epoch / 3600 ;
  $epoch -= $hour*3600 ;
  $min=int $epoch / 60 ;
  $epoch -= $min*60 ;
  $ss=$epoch;

# add days from 1900 to 1970
  $days += 17*(4*365+1)+366+ 365 ;
  $four_year=int $days / (4*365+1);
  $days -= $four_year * (4*365+1);

  if ($days>365)
    { $days -= 366;
      $year = 1+int $days / 365 ;
      $days -= ($year-1)*365; }
  else
    { $year = 0; }

  $yr=1900+4*$four_year+$year ;
  $jday=$days+1 ;
  return ($yr,$jday,$hour,$min,$ss);
}

#julday(year,month,day) returns julian day corresponding to date
# (ignores 100 and 400 year rule)
sub julday {
  my($yr,$mo,$day)=@_ ;
  my($jdy) ;
  if ($mo==1) {
    $jdy=$day ;
  } elsif ($mo==2) {
    $jdy=$day + 31 ;
  } elsif ($mo==3) {
    $jdy=$day + 59 ;
  } elsif ($mo==4) {
    $jdy=$day + 90 ;
  } elsif ($mo==5) {
    $jdy=$day + 120 ;
  } elsif ($mo==6) {
    $jdy=$day + 151 ;
  } elsif ($mo==7) {
    $jdy=$day + 181 ;
  } elsif ($mo==8) {
    $jdy=$day + 212 ;
  } elsif ($mo==9) {
    $jdy=$day + 243 ;
  } elsif ($mo==10) {
    $jdy=$day + 273 ;
  } elsif ($mo==11) {
    $jdy=$day + 304 ;
  } elsif ($mo==12) {
    $jdy=$day + 334 ;
  }
  $jdy=$jdy+1 if ($yr%4 == 0 && $mo > 2 );
  return $jdy ;
}

#(mo,dy)=calday(year,jdy)
# (ignores 100 and 400 year rule)
sub calday {
  my ($yr,$jdy)=@_;
  my ($mo,$dy);
  if ($yr%4 == 0) {
    # leap year
    if ($jdy==60) {
      return (2,29);
    } elsif ($jdy > 59 ) {
      $jdy=$jdy-1;
    }
  }
  if ($jdy<=31) {
    $mo=1; $dy=$jdy;
  } elsif ($jdy<=59) {
    $mo=2; $dy=$jdy-31;
  } elsif ($jdy<=90) {
    $mo=3; $dy=$jdy-59;
  } elsif ($jdy<=120) {
    $mo=4; $dy=$jdy-90;
  } elsif ($jdy<=151) {
    $mo=5; $dy=$jdy-120;
  } elsif ($jdy<=181) {
    $mo=6; $dy=$jdy-151;
  } elsif ($jdy<=212) {
    $mo=7; $dy=$jdy-181;
  } elsif ($jdy<=243) {
    $mo=8; $dy=$jdy-212;
  } elsif ($jdy<=273) {
    $mo=9; $dy=$jdy-243;
  } elsif ($jdy<=304) {
    $mo=10; $dy=$jdy-273;
  } elsif ($jdy<=334) {
    $mo=11; $dy=$jdy-304;
  } elsif ($jdy<=365) {
    $mo=12; $dy=$jdy-334;
  } else {
    die "Impossible Julian day $jdy.";
  }
  return( $mo,$dy );
}

# deg2km(distance)
# converts great circle distance in degree into km 
# (ignoring ellipticity)
sub deg2km {
  my($pi,$radius)=(3.14159265358979,6371) ;
  return ( $_[0] * $pi * $radius / 180.);
}

# deg2km(distance)
# converts degree to radians
sub deg2rad {
  my ($pi)=3.14159265358979;
  return( $_[0]*$pi/180.);	
}


# distaz(epilat,epilon,stlat,stlon)
# returns list (azimuth,backazimuth,gcarc,distkm)
# translated straight from FORTRAN routine distazfr.f in fortran/CMT_sorter
# takes into account ellipticity of the Earth
# all angles given in degree
sub distaz {
  use POSIX qw(tan atan acos);

  my($epilat,$epilon,$stlat,$stlon)=@_ ;
  my($az,$baz,$dist,$distkm) ;
  my($pi,$radius)=(3.141459,6371) ;
  my($elat,$elon,$slat,$slon,$arg,$a,$b,$c,$aa,$bb,$cc) ;

  if (abs($epilat) < 180) {
    $arg=0.993277*tan($epilat*$pi/180.) ;
    $elat=atan($arg);
  } else {
    $elat=$epilat*$pi/180. ;
  }
  $elon=$epilon*$pi/180. ;
  if (abs($stlat) < 180) {
    $arg=0.993277*tan($stlat*$pi/180);
    $slat=atan($arg);
  } else {
    $slat=$stlat*$pi/180.;
  }
  $slon=$stlon*$pi/180 ;
  # special cases
  if ( $slon == $elon ) {
    return (0,0,0,0) if $slat==$elat;
    $a=abs($slat-$elat);
    if ($slat>$elat) {
      $az=0; $baz=180;
    } else {
      $az=180; $baz=0;
    }
  } else {
    $b=$pi/2.-$elat ;
    $c=$pi/2.-$slat ;
    $aa=$slon-$elon ;
    $a=acos(cos($b)*cos($c)+sin($b)*sin($c)*cos($aa)) ;
    $cc=acos((cos($c)-cos($a)*cos($b))/(sin($a)*sin($b))) ;
    $bb=acos((cos($b)-cos($a)*cos($c))/(sin($a)*sin($c))) ;
    if (mod($slon-$elon+4*$pi,2*$pi) < $pi) {
      $az=$cc*180./$pi ;
      $baz=360.-$bb*180./$pi ;
    } else {
      $az=360.-$cc*180./$pi ;
      $baz=$bb*180./$pi ;
    }
  }
  $dist=$a*180./$pi ;
  $distkm=$a*$radius ;
  return($az,$baz,$dist,$distkm) ;
}


# SEISAN format routines

# slurp(fn) creates array with each element corresponding to one event
# from nordic input.
#
# fn can be
# -           input is STDIN 
# a filename  nordic file
# CCCCC       a database title (3-5 characters)
# DBASE       use default database
sub slurp {
  my ($fn)=@_ ;
  my ($fh,$n,$new_event);
  my (@file);
  my ($tempfile);


  if ( $fn eq "-" ) {
#    print STDERR "DEBUG: $fn is stdin .\n";
    $fh=STDIN ;
  } elsif ( -e $fn ) {
#    print STDERR "DEBUG: $fn is a file.\n";
    open $fh,"<$fn" or die "Could not open $fn. System error: $!.";
  } elsif ( length($fn) >= 3 && length ($fn) <=5 ) {
#    print STDERR "DEBUG: $fn is a database";
    if ( $fn =~ m/^D(BASE)?$/ ) {
      $fn=$ENV{DEF_BASE};
    }
    $fn = $fn ."____" ; $fn=substr($fn,0,5);
    if ( -d "$ENV{SEISAN_TOP}/REA/$fn" ) {
      #
      #"/tmp/merge_seisan.$$";
      $tempfile=tmpnam();
      system("cd $ENV{SEISAN_TOP}/REA/$fn ; cat ????/??/??-????-???.S?????? > $tempfile" );
      open $fh,"<$tempfile"  or die "Could not open $tempfile. System error: $!.";
    } else {
      die "$ENV{SEISAN_TOP}/REA/$fn does not exist. (Did you remember to set SEISAN_TOP and/or DEF_BASE)?";
    }
  } else {
    die "Cannot find $fn.";
  }
  $n=-1;
  $new_event=1;
  while(<$fh>)  {
#    print STDERR "Line: $_";
    if ( $_ =~ /^\s*$/ ) {
      # empty line, end of record
      #      $file[$n] = $file[$n] . $_;
      $file[$n] = $file[$n] . "\n" ;
      $new_event = 1;
    } elsif ( $new_event ) {
      $file[++$n]=$_;
      $new_event=0;
    } else {
      $file[$n] = $file[$n] . $_;
    }
  }
  return(@file);
  close($fh);
  unlink($tempfile) if (-e $tempfile);
}



# event2epoch(event) 
# where event is a string containing one event in nordic format 
# returns the origin time (or reference time where not located) as epoch time
sub event2epoch {
  my ($event)=@_;
  my ($date);
  substr($event,79,1)=~/[1]/ or die "event2jul: event does not begin with a type 1 line";
  $date=substr($event,1,19);
  my ($yr,$mo,$dy,$hr,$mi,$se)= ($date =~ /^([\d ]{4}) ([\d ]{2})([\d ]{2})[ F]([\d ]{2})([\d ]{2}) ([\d\. ]{4})/ ) or die "event2jul: Format error in date |$date|" ;
  my $jdy=julday($yr,$mo,$dy);
  # print STDERR "DEBUG: $yr,$jdy,$hr,$mi,$se\n";
  return(jul2epoch($yr,$jdy,$hr,$mi,$se));
}




# Auxiliary routines, not for export

## mod(a,b) for real positive a,b
sub mod {
  my($div)=$_[0]/$_[1] ;
  return ($_[0]-int($div)*$_[1]) ;
}
}


use English;
import seisutil qw(jul2epoch epoch2jul julday deg2km distaz);
use Math::Trig;


#use strict; 
use constant {
# Association method (can be combined, hence give powers of 2)
  ID_MATCH => 1,
  TIME_MATCH => 2,
  DIST_MATCH => 4,
  HYPO_MATCH => 8,
# Output method, set     AB
  INTERSECTION => 1,   # 00->0,01->0,10->0,11->1
  UNION => 2,          # 00->0,01->1,10->1,11->1
  SET_A => 3,          # 00->0,01->0,10->1,11->1
  SET_B => 4,          # 00->0,01->1,10->0,11->1
  NOT_A => 5,          # 00->1,01->1,10->0,11->0
  NOT_B => 6,          # 00->1,01->0,10->1,11->0
# output method, METHOD
  SPLIT => 1,
  PRIOR_A => 2,
  PRIOR_B => 3, 
  DIFF => 4,
# output method, DIFF_FORMAT
  XYZ =>1,
  GMT =>2,
  GMTD => 3,
  BIAS => 4,
# mag_slot_find
  EQUIV_SLOT =>1,
  MATCH_TYPE_AGENCY => 2
};
## unsafe tempfile: Note that name must be an absolut filename (starting with /)
my $tempfile="/tmp/merge_seisan.$$";

my %match_method=( TYPE=>ID_MATCH ); 
my %output_method=( SET => INTERSECTION, METHOD => SPLIT ) ;
my $mag_slot_find= EQUIV_SLOT;
my ($fh,$n,$dum,$dum1,$dum2,$dum3,$dum4,$dum5);
my (@filea,@fileb);
# for bias calculation
my ($biasx,$biasy,$biasz,$latave,$lonave,$wsum);
my ($cxxs,$cxys,$cxzs,$cyys,$cyzs,$czzs);



if (defined($h) || defined($help) || defined($H)|| scalar(@ARGV)!=2  ) { usage(); }

if (defined($I)) { $match_method{TYPE}=ID_MATCH ; }
if (defined($T)) { $match_method{TYPE}=TIME_MATCH ; 
		   $match_method{TIME_TOL}=$T; }
if (defined($D)) { if ($D=~s/h$// ) {
                     $match_method{TYPE}|=HYPO_MATCH ; 
		   } else {
                     $match_method{TYPE}|=DIST_MATCH ; 
		   }
		   $match_method{DIST_TOL}=$D; }
if (defined($u)) { $output_method{SET}=UNION ; }
if (defined($i)) { $output_method{SET}=INTERSECTION ; }
if (defined($S)) { if    ( $S eq "U" ) { $output_method{SET}=UNION ; }
		   elsif ( $S eq "I" ) { $output_method{SET}=INTERSECTION ; }
		   elsif ( $S eq "A" ) { $output_method{SET}=SET_A ; }
		   elsif ( $S eq "B" ) { $output_method{SET}=SET_B ; } 
		   elsif ( $S eq "~A" ) { $output_method{SET}=NOT_A ; $output_method{METHOD}=PRIOR_B }
		   elsif ( $S eq "~B" ) { $output_method{SET}=NOT_B ; $output_method{METHOD}=PRIOR_A } }
 if (defined($s)) { $output_method{METHOD}=SPLIT ; }
if (defined($A)) { $output_method{METHOD}=PRIOR_A ; }
if (defined($B)) { $output_method{METHOD}=PRIOR_B ; }
if (defined($b)) { $output_method{B_LIST}=[ split(/,/,$b) ]; $output_method{METHOD}=PRIOR_A ; }
if (defined($m)) { $output_method{M_LIST}=[ split(/,/,$m) ]; $output_method{METHOD}=PRIOR_A ; }
if (defined($a)) { die "Option -a not implemented yet.  Flip order of sets and use option -b instead" ; }
if (defined($d)) { $output_method{METHOD}=DIFF ; 
		   if ($d eq "gmt") {$output_method{DIFF_FORMAT}=GMT;}
		   elsif ($d eq "gmtd") {$output_method{DIFF_FORMAT}=GMTD;}
		   elsif (substr($d,0,4) eq "bias") {
		     $output_method{DIFF_FORMAT}=BIAS; 
		     ($dum1,$dum2,$dum3)=split(':',$d); 
		     if (defined($dum2) ) {
		       $output_method{MIN_VAR}=$dum2**2;
		     } else { 
		       $output_method{MIN_VAR}=1; 
		     }
		     if (defined($dum3) ) {
		       $output_method{COV_SCALE}=$dum3;
		     } else { 
		       $output_method{COV_SCALE}=2.71; 
		     }
		     # print stderr "DEBUG: MinVar: $output_method{MIN_VAR}\n";
		     $latave=0.0;
		     $lonave=0.0;
		     $biasx=$biasy=$biasz=0.0;
		     $wsum=0.0;
		     $cxxs=$cxys=$cxzs=$cyys=$cyzs=$czzs=0.0 ;
		   }
		   else { $output_method{DIFF_FORMAT}=XYZ;} }
if (defined($M)) { $mag_slot_find=MATCH_TYPE_AGENCY ; }

# Default match method: ID_MATCH
$match_method{TYPE}!=0 or $match_method{TYPE}=ID_MATCH;
  
if ( $output_method{METHOD}== DIFF && $output_method{SET} != INTERSECTION) {
  die "ERROR Difference output requires -i option (intersection";
}


# How to access array elements of B_LIST
#print STDERR "MLIST ",@{$output_method{M_LIST}},".",split(/,/,$b),".",$output_method{M_LIST}->[0],"\n";
#exit;


# slurp first file
@filea=slurp($ARGV[0]);
@fileb=slurp($ARGV[1]);

#print STDERR "DEBUG: ", $output_method{SET},"\n";
print STDERR "Number of events in File A: ", scalar(@filea),"\n";
print STDERR "Number of events in File B: ", scalar(@fileb),"\n";

$cnta=0;
$cntb=0;
$cntmatch=0;
$cntout=0;

while ( $cnta <= $#filea && $cntb<=$#fileb ) {
  $dum=compare($filea[$cnta],$fileb[$cntb],\%match_method);
  if ( $dum == 0 ) {
    # two identical files
    output($filea[$cnta],$fileb[$cntb],\%output_method);
    $cnta++;
    $cntb++;
    $cntmatch++;
  } elsif ( $dum < 0 ) {
    # A<B
    output($filea[$cnta],"",\%output_method);
    $cnta++;
  } else {
    # A>B
    output("",$fileb[$cntb],\%output_method);
    $cntb++;
  }
}

# output remainder of unfinished sequences
while ( $cnta <= $#filea ) {
  output($filea[$cnta],"",\%output_method);
  $cnta++;
}
while ( $cntb <= $#fileb ) {
  output("",$fileb[$cntb],\%output_method);
  $cntb++;
}

print STDERR "Number of events in File A and B: ", $cntmatch,"\n";
print STDERR "Number of events printed: ", $cntout,"\n";

if ( $output_method{METHOD}==DIFF &&  $output_method{DIFF_FORMAT}==BIAS && $cntmatch>=1 ) {
  # calculate final errors and correction vector;
  $latave /= $wsum ;
  $lonave /= $wsum ;
    print stderr "DEBUG latave $latave lonave $lonave wsum $wsum " ,1/$wsum,"\n";
  $biasx /= $wsum;
  $biasy /= $wsum;
  $biasz /= $wsum;
  $cxxs  /= $wsum**2;
  $cyys  /= $wsum**2;
  $czzs  /= $wsum**2;
  $cxys  /= $wsum**2;
  $cxzs  /= $wsum**2;
  $cyzs  /= $wsum**2;

  print "Bias (set B - set A) #eq: $cntmatch \n";
  printf "(Lon,Lat,Dep) = ( %5.2f, %5.2f, %5.2f )\n",$biasx,$biasy,$biasz ;
  $dum1=deg2km($biasx*cos(deg2rad($latave)));  # dum1 : x bias in km
  $dum2=deg2km($biasy);                        # dum2 : y bias in km
  printf "(R,THETA,Z)   = ( %5f, %5.1f, %5f )\n",sqrt($dum1**2+$dum2**2),rad2deg(atan2($dum1,$dum2)),$biasz;
  printf "(X,Y,Z) (km)  = ( %5f, %5f, %5f )\n",$dum1,$dum2,$biasz ;
  printf "(EX,EY,EZ)    = ( %5f, %5f, %5f )\n",sqrt($cxxs),sqrt($cyys),sqrt($czzs);
  print "CXX,CXY,CXZ,CYY,CYZ,CZZ = $cxxs, $cxys, $cxzs, $cyys, $cyzs, $czzs\n";
  # Input line for psvelo (bias in km)
  # lon, lat, ve, vn, sigma_e, sigma_n, correlation(=cxy/(sqrt(cxx)*sqrt(cxy)))
  print "$lonave $latave $dum1 $dum2 ",sqrt($cxxs/$output_method{COV_SCALE})," ", sqrt($cyys/$output_method{COV_SCALE})," ",$cxys/(sqrt($cxxs)*sqrt($cyys)),"\n";
#, ,
}

sub compare {
  my ($eventa,$eventb,$match_method)=@_ ;
  my ($ida,$idb,$timea,$timeb,$date,$az,$baz,$dist,$distkm,$lata,$lona,$latb,$lonb,$dist_check);


  if ( $match_method->{TYPE} & (DIST_MATCH|HYPO_MATCH) ) {
    substr($eventa,79,1)=~/[1]/ or die "merge_seisan.pl compare: event in set A does not begin with a type 1 line. Offending line:\n:$eventa";
    substr($eventb,79,1)=~/[1]/ or die "merge_seisan.pl compare: event in set B does not begin with a type 1 line. Offending line:\n:$eventa";
    $lata=substr($eventa,23,7); $lona=substr($eventa,30,8) ; $depa=substr($eventa,38,5);
    $latb=substr($eventb,23,7); $lonb=substr($eventb,30,8) ; $depb=substr($eventb,38,5);
    ($az,$baz,$dist,$distkm)=distaz($lata,$lona,$latb,$lonb);
    if ( $match_method->{TYPE} & HYPO_MATCH ) {
      $disthypkm=sqrt($distkm**2+($depb-$depa)**2);
      $dist_check= ($disthypkm < $match_method->{DIST_TOL});
    } else {
      $dist_check= ($distkm < $match_method->{DIST_TOL});
    }
    # print STDERR "DEBUG $lata,$lona,$depa $latb,$lonb,$depb D_epi $distkm D_hypo $disthypkm  | $_method->{DIST_TOL} distcheck $dist_check\n";
  } else {
    $dist_check=1;
  }

  if ($match_method->{TYPE} & ID_MATCH ) {
    # ACTION:SPL 05-04-27 23:05 OP:fjt  STATUS:               ID:20041215024414     I
    ($ida) = ($eventa=~ m/ID:(\d{14}).....I$/m);
    ($idb) = ($eventb=~ m/ID:(\d{14}).....I$/m);
    return ( $ida cmp $idb );
  } elsif ( $match_method->{TYPE} & TIME_MATCH ) {
    $timea= event2epoch($eventa);
    $timeb= event2epoch($eventb);
    if ( abs($timea-$timeb) <= $match_method->{TIME_TOL} && $dist_check ) {
      #  print STDERR "DEBUG Sucess: $timea, $timeb, ", abs($timea-$timeb), ",", $match_method->{TIME_TOL}," ... \n";
      return 0 ; 
    } else {
      #  print STDERR "DEBUG Fail: $timea, $timeb, ", abs($timea-$timeb), ",", $match_method->{TIME_TOL}," ... \n";
      return( $timea <=> $timeb );
    }
  }
  else {
    die "Unknown method $typ .";
  }
}

sub event2epoch {
  my ($event)=@_;
  substr($event,79,1)=~/[1]/ or die "event2epoch: event does not begin with a type 1 line. Offending line:\n $event";
  $date=substr($event,1,19);
  my ($yr,$mo,$dy,$hr,$mi,$se)= ($date =~ /^([\d ]{4}) ([\d ]{2})([\d ]{2})[ F]([\d ]{2})([\d ]{2}) ([\d\. ]{4})/ ) or die "event2jul: Format error in date |$date|" ;
  my $jdy=julday($yr,$mo,$dy);
  # print STDERR "DEBUG: $yr,$jdy,$hr,$mi,$se\n";
  return(jul2epoch($yr,$jdy,$hr,$mi,$se));
}

sub output {
  my ($eventa,$eventb,$output_method )=@_ ;
  my ($spaces)=" " x 82 ;
  my @linetypes=("1","E","H","I","F","2","3","5","6","7","[4 ]");
  my (@linesa)=split(/\n/,$eventa);
  my (@linesb)=split(/\n/,$eventb);
  my (@matcha,@matchb,@dum,$linetype,$first,$offset,$offset2,$i,$j);
  my (@errlina,@errlinb,$cxx,$cxy,$cxz,$cyy,$cyz,$czz,$weight,$w2);
  my ($lata,$latb,$lona,$lonb,$depa,$depb);

  if ( $eventb eq "" ) {
     # only a event
#    print STDERR "DEBUG : No event in B $output_method->{SET}\n";
#    print STDERR "DEBUG Cond", ($output_method->{SET}== INTERSECTION), " , ",($output_method->{SET} eq SET_B),"\n";
    return if ( $output_method->{SET} eq INTERSECTION || $output_method->{SET} eq SET_B || $output_method->{SET} eq NOT_A );
    print $eventa;
    $cntout++; 
  } elsif ( $eventa eq "" ) {
    # only b event present
    return if ( $output_method->{SET} eq INTERSECTION || $output_method->{SET} eq SET_A || $output_method->{SET} eq NOT_B );
    $eventb=~ s/^(.*?)$/$spaces$1/mg if ( $output_method->{METHOD} eq SPLIT ); 
    print $eventb; 
    $cntout++;
  } else {
    # both events present
    return if ( $output_method->{SET} eq NOT_A || $output_method->{SET} eq NOT_B );
    $cntout++;
    if ( $output_method->{METHOD} eq SPLIT ) {
      foreach $linetype ( @linetypes ) { 
	@matcha=grep /${linetype}$/,@linesa;
	@matchb=grep /${linetype}$/,@linesb;
	###print STDERR "DEBUG: Linetyp $linetype matcha ",scalar @matcha," matchb ",scalar @matchb,"\n";

	chomp @matcha; chomp @matchb;
	if ( " " =~ m/$linetype/ ) {
	  # print picks, line up picks according to stationname
	  # additional sorting by station name of travel time picks (associates correctly unless there are different picks
	  @matcha=sort @matcha;
	  @matchb=sort @matchb;
	  my %picksa=();
	  my %picksb=();
	  foreach $pickline ( @matcha ) {
	    $key = make_hash_from_4_line($pickline);
	    if (!defined($picksa{$key})) {
	      $picksa{$key}=$pickline ;
	    } else {
	      $picksa{$key}.= "|" . $pickline;
	    }
	  }
	  foreach $pickline ( @matchb ) {
	    $key = make_hash_from_4_line($pickline);
	    if (!defined($picksb{$key})) {
	      $picksb{$key}=$pickline ;
	    } else {
	      $picksb{$key}.= "|" . $pickline;
	    }
	  }
	  my (@outa,@duma,@dumb);
	  # plot shared picks first, then only those in set A,
	  # then those only in set B

	  # plot joint picks, and save picks just in A
	  foreach $key ( sort keys %picksa ) {
	    if (defined($picksb{$key})) {
	      @duma=split(/\|/,$picksa{$key});
	      @dumb=split(/\|/,$picksb{$key});

	      while ( @duma || @dumb) {
		if ( @duma && @dumb) {
		  print shift @duma, "  ", shift @dumb,"\n";
		} elsif (@duma)  {
 		  print shift @duma,$spaces,"\n";
		} else {
		  print $spaces, shift @dumb,"\n";
		}
		###print STDERR "DEBUG: keyab $key ",scalar @duma, scalar @dumb,"\n";
	      }
	    } else {
	      push(@outa, split(/\|/,$picksa{$key}));
	    }
	  }
	  print join("\n",@outa),"\n" if @outa;
	  foreach $key ( sort keys %picksb ) {
	    ###print STDERR "DEBUG: keyb $key\n";
	    # skip picks already output above
	    next if defined($picksa{$key});
	    print $spaces . join("\n" . $spaces,$picksb{$key}),"\n";
	  }
	} else {
	  # print all other plot lines
	  while ( @matcha ) {
	    print shift @matcha, "  ", shift @matchb,"\n";
	  }
	  while ( @matchb ) {
	    print $spaces, shift @matchb,"\n";
	  }
	}
      }
      print "\n" ;
    }
    elsif ( $output_method->{METHOD} eq PRIOR_A ) {
      if ( defined($output_method{B_LIST}) || defined($output_method{M_LIST}) ) {
#	print STDERR "DEBUG: B-List ",@{$output_method{B_LIST}},"\n";
	my ( @mblist ) = () ;
	if ( defined($output_method{B_LIST}) ) { push @mblist, @{$output_method{B_LIST}}; }
	if ( defined($output_method{M_LIST}) ) { push @mblist, @{$output_method{M_LIST}}; }
#	foreach $type ( @{$output_method{B_LIST}} ) {
#	print STDERR "DEBUG: mblist:", @mblist," linetypes:",join(":",@linetypes),"\n" ; 
	foreach $type ( @mblist ) {
	  if ( $type eq "4" or grep /^${type}$/,@linetypes ) {
	    $linetype = ( $type eq "4" ? qr/[ 4]/ : $type );
#	    print STDERR "DEBUG: TYPE $type $linetype \n";
            # type is a type of line
	    @matchb=grep /${linetype}$/,@linesb;
#	    print STDERR "DEBUG: matchb ",scalar @matchb,"\n";
	    $first=-1;
            for ($i=0;$i<@linesa;$i++) {
#	      print STDERR "DEBUG: linea $i $linesa[$i]\n";
	      if ( $linesa[$i] =~ m/${linetype}$/ ) {
		$first=$i if ($first<0);
		# remove all lines of appropriate type from linesa if in b_list
		if ( grep /$linetype/, @{$output_method{B_LIST}} ) {
		  splice(@linesa,$i,1);
		  $i=$i-1; # we need to look at the same index again as a new element will be in the same position now
		}
	      }
	    }
            if ($first<0) {
	      # always insert lines before type 7 line if it exists, otherwise right at the end
	      for($first=0;$first<@linesa; $first++) {
		if ( $linesa[$first] =~ m/7$/ ) {
		  last ;
		}
	      }
	    }
#            print STDERR "DEBUG: first $first \n";
	    splice(@linesa,$first,0,@matchb);
	  } 
          elsif ( $type =~ m/^m[WLSBCO]?$/ ) {
	    # copy magnitude information from type 1 line
	    @matchb=grep /1$/,@linesb;
	    $linesa[0] =~ m/1$/ or die "First line of event in first set is not type 1 line";
	    for ( $i=0; $i<3; $i++ ) {
	      # loop over three magnitude slots
	      $offset=55+$i*8;
	      if ( $type eq "m" or substr($type,1,1) eq substr($matchb[0],$offset+4,1) ) {
		# find equivalent or free slot in other subset
		if ( $mag_slot_find == EQUIV_SLOT || $type eq "m") {
#		  print STDERR "EQUIV_SLOT\n";
		  substr($linesa[0],$offset,8)=substr($matchb[0],$offset,8);
		}
		elsif ( $mag_slot_find == MATCH_TYPE_AGENCY ) {
		  # print STDERR "MAG_SLOT_FIND\n";
		  for ( $j=0; $j<3; $j++ ) {
		    # loop over three magnitude slots
		    $offset2=55+$j*8;
		    if ($j==2                                        # last slot
			|| substr($linesa[0],$offset2+4,4) eq "    "    # empty slot
			|| substr($matchb[0],$offset+4,4) eq substr($linesa[0],$offset2+4,4) # agency and type match
		       ) { 
		      substr($linesa[0],$offset2,8)=substr($matchb[0],$offset,8);
		      last ;
		    }
		  }
		}
		else {
		  die "INTERNAL: Unknown mag_slot_find_method $mag-slot_find."
		}
	      }
	    }
	  } elsif ( $type =~ m/^[oed]$/ ) {
	    @matchb=grep /1$/,@linesb;
	    $linesa[0] =~ m/1$/ or die "First line of event in first set is not type 1 line";
	    if ( $type eq 'o' ) {
	      substr($linesa[0],1,19)=substr($matchb[0],1,19);
	    } elsif ( $type eq 'e' ) {
	      substr($linesa[0],23,15)=substr($matchb[0],23,15);
	    } elsif ( $type eq 'd' ) {
	      substr($linesa[0],38,5)=substr($matchb[0],38,5);
	    }
          } else {
	    die "Unknown b-list type $type."
	  }
        } # foreach $type ( B_LIST )
        print join ("\n",@linesa),"\n";
        print substr($space,0,80), "\n" unless $linesa[$#linesa]=~m/^\s*\n/;
      } else {
         print $eventa;
      }
    }
    elsif ( $output_method->{METHOD} eq PRIOR_B ) {
      print $eventb;
    }
    elsif ( $output_method->{METHOD} eq DIFF ) {
      @matcha=grep /1$/,@linesa;
      @matchb=grep /1$/,@linesb;
      $lata=substr($matcha[0],23,7);
      $lona=substr($matcha[0],30,8);
      $depa=substr($matcha[0],38,5);
      $latb=substr($matchb[0],23,7);
      $lonb=substr($matchb[0],30,8);
      $depb=substr($matchb[0],38,5);
      if ($output_method{DIFF_FORMAT} eq GMT ) {
	print "> \n";
	print "$lona $lata\n"; 
	print "$lonb $latb\n"; 
      }
      elsif ($output_method{DIFF_FORMAT} eq GMTD ) {
	print "> \n";
	print "$lona $lata $depa\n"; 
	print "$lonb $latb $depb\n"; 
      }
      elsif ($output_method{DIFF_FORMAT} eq XYZ ) {
	printf "%.2f %.2f %.2f\n", cos(0.5*($lata+$latb)*3.14159/180)*($lonb-$lona)*111.25,($latb-$lata)*111.25,$depb-$depa;
      }
      elsif ($output_method{DIFF_FORMAT} eq BIAS ) {
	@errlina=grep /E$/,@linesa;
	@errlinb=grep /E$/,@linesb;

#          1         2         3         4         5         6         7          8
# 123456789012345678901234567890123456789012345678901234567890123456789012345678901234
# GAP=250        0.97      12.7     8.5 26.6  0.1599E+02 -0.3146E+02 -0.2907E+03E
# GAP=                      4.5     4.7 21.0  6.0502e+00  5.5199e+01  3.3099e+01E
	# covariance of bias vector for one event is cov_A+cov_B
 	$cxx=(1.0*substr($errlina[0],26,4))**2;
	$cyy=(1.0*substr($errlina[0],34,4))**2;
	$czz=(1.0*substr($errlina[0],39,4))**2;
	$cxy=(1.0*substr($errlina[0],44,11));
	$cxz=(1.0*substr($errlina[0],56,11));
	$cyz=(1.0*substr($errlina[0],68,11));
	#print stderr "DEBUG: cxy |",substr($errlina[0],44,11),"|,",$cxy," $cxy\n";

 	$cxx+=(1.0*substr($errlinb[0],26,4))**2;
	$cyy+=(1.0*substr($errlinb[0],34,4))**2;
	$czz+=(1.0*substr($errlinb[0],39,4))**2;
	$cxy+=(1.0*substr($errlinb[0],44,11));
	$cxz+=(1.0*substr($errlinb[0],56,11));
	$cyz+=(1.0*substr($errlinb[0],68,11));
	# weight by horizontal error
	$weight=1/($cxx+$cyy);

	if ($weight > 1/$output_method{MIN_VAR}) {
	  $weight = 1/$output_method{MIN_VAR} ;
	}
	# keep bias vector calculation it in degrees
	# (calculation will fail near the poles)
	$latave+=$lata*$weight;
	if ( $wsum>0 && $lona - $lonave/$wsum  > 180 ) {
	  $lona -= 360;
	} elsif ( $wsum>0 && $lona - $lonave/$wsum  < -180 ) {
	  $lona += 360;
	} 
	$lonave+=$lona*$weight;


	$dum=($lonb-$lona);
	$dum+=360 if ($dum<-180);
	$dum-=360 if ($dum>180);
	$biasx+=$weight*$dum;
	$biasy+=$weight*($latb-$lata);
	$biasz+=$weight*($depb-$depa);

	$wsum+=$weight;
	$w2=$weight**2;
 	$cxxs+=$w2*$cxx;
	$cyys+=$w2*$cyy;
	$czzs+=$w2*$czz;
	$cxys+=$w2*$cxy;
	$cxzs+=$w2*$cxz;
	$cyzs+=$w2*$cyz;
      }
    }
  }
}

# create array with each element corresponding to one event
sub slurp {
  my ($fn)=@_ ;
  my @file;

  if ( $fn eq "-" ) {
    print STDERR "$fn is stdin .\n";
    $fh=STDIN ;
  } elsif ( -e $fn ) {
    print STDERR "$fn is a file.\n";
    open $fh,"<$fn" or die "Could not open $fn. System error: $!.";
  } elsif ( length($fn) >= 3 && length ($fn) <=5 ) {
    print STDERR "$fn is a database\n";
    if ( $fn =~ m/^D(BASE)?$/ ) {
      $fn=$ENV{DEF_BASE};
    }
    $fn = $fn ."____" ; $fn=substr($fn,0,5);
    if ( -d "$ENV{SEISAN_TOP}/REA/$fn" ) {
      #
      system("cd $ENV{SEISAN_TOP}/REA/$fn ; cat ????/??/??-????-???.S?????? > $tempfile" );
      open $fh,"<$tempfile"  or die "Could not open $tempfile. System error: $!.";
    } else {
      die "$ENV{SEISAN_TOP}/REA/$fn does not exist. (Did you remember to set SEISAN_TOP and/or DEF_BASE)?";
    }
  } else {
    die "Cannot find $fn.";
  }
  $n=-1;
  $new_event=1;
  while(<$fh>)  {
#    print STDERR "Line: $_";
    if ( $_ =~ /^\s*$/ ) {
      # empty line, end of record
      #      $file[$n] = $file[$n] . $_;
      $file[$n] = $file[$n] . "\n" ;
      $new_event = 1;
    } elsif ( $new_event ) {
      $file[++$n]=$_;
      $new_event=0;
    } else {
      $file[$n] = $file[$n] . $_;
    }
  }
  return(@file);
  close($fh);
  unlink($tempfile) if (-e $tempfile);
}

# make_hash_from_4_line( line );
# given a type 4 pick line from a SEISAN file returns a hash used for 
# grouping station.
# Hash = (a5,a4) - station name, phase
sub make_hash_from_4_line {
  ###print STDERR "DEBUG Hash:", substr($_[0],1,5) . substr($_[0],10,4),":\n";
  return( substr($_[0],0,5) . substr($_[0],10,4) );
}

sub usage { 
die <<EOF;
Usage: merge_seisan.pl [options] cat1 cat2 > out.nor

Reads two CAT Files or Sfiles in nordic format and associates events,
merging the information and respective files.  Merged events have information
from both files interspersed. Both files must be sorted.  Also each event
of the first set can only be associated with at most one event of the second set,
and vice versa.

cat1 and cat2 can be one of the following:

<file.nor>    The filename of a file in Nordic format

DBASE         If cat1 is three to five letters long and a file of the same 
              name does not exist the corresponding catalogues is created on 
              the fly from the S-files in the respectiv database. If the code
	      DBASE or D is used, use the current database

-             read from standard in (only one - is allowed, of course)

Options:

Method of association:

-I            associate events by ID (default)
-T=<tol>      associate events within (tol) s of each other

-D=<dist>[h]  Maximum epicentral distance in km. If h is appended then the hypocentral
              distance is used instead.

-D can be combined with -T but cannot be used on its own (both conditions need to be fulfilled).
 It also cannot be used with -I option


Output control

-S=I,-i            Only output associated events (Intersection)

-S=U,-u            Output all events (Union)

-S=A          Output only events in A (only makes sense when combined with -b or -B option)
-S=B          Ouptut only events in B

-S=~A         Output only events not in A (i.e. only in B, only makes sense with -B option)
-S=~B         Output only events not in B (i.e. only in A, only makes sense with -A option)


-s            Show different sets side-by-side (split screen)

-A            For associated events, only show event from first set
-B            For associated events, only show event from second set

-b=1,2,m      For associated events, take all header lines of the type listed from second 
              set, all other headerlines from first set (implies -A).  If the second set 
              does not have the required headerline, they will not be included in the output
              (i.e., specified header lines will never be taken from first set if there is 
              an event association) 

              1...9,F,E,I,H :  the corresponding header lines (Note that line type 4 refers
                               to phase pick lines, even if they have a blank in column 80
              o:            :  Origin time (from type 1 line only)
              e:            :  Epicentral coordinates (from type 1 only)
              d:            :  Depth (from type 1 line only)
              m             :  copy all magnitude information into header line
              mW,mL,mb,mS   :  only copy designated magnitudes (note that magnitudes are copied
                               into the same slot unless option -M is set

-a=1,2,..     Like -b, but copy information from first set to 2nd set, implies -B (not 
              implemented yet)

-m=1,2,..     Like -b, but add header lines to existing header lines, effectively merging
              the information in both files. Other header lines are taken from A (implies -A).

-M            Find named slot for copied magnitudes (only relevant if -b={mW,ML,mb,mS} is set
              Both type and agency need to match; otherwise the first empty slot is occupied, or
              the last slot is overwritten)

-d=xyz        Plot differences in hypocentre (in km) (loc2-loc1)
-d=gmt        Plot differences in epicentre as gmt-style multi-segment file (for input into psxy)
-d=gmtd       Plot differences in hypocentre as gmt-style multi-segment file.  This is similar
              to the output of -d=gmt, but insted of just latitude and longitude include latitude,
              longitude and depth (for pre-processing with awk before passing to psxy)
-d=bias[:<minerr>][:covscale]
              Calculate bias vector between the two sets (i.e. the vector that needs to be
              added to loc1 locations to make them coincide with loc2 locations on average). 
              The calculation weighs pairings according to their horizontal errors and an error
              estimate is provided for the resulting bias vector.  For combined error estimates 
              less than 1 km (or <minerr>) if set, the weighing is applied assuming the error 
              is 1 km (minerr).   The calculation assumes the area of interest is small such 
              that all calculations ignore spherical geometry.  The average latitude of the 
              events is used to determine the latitude for the spherical to Cartesian 
              conversion.

              Example Output (actual output without line number)

              1 Bias (set B - set A) #eq: 58 
              2 (Lon,Lat,Dep) = (  0.01,  0.04, -8.21 )
              3 (R,THETA,Z)   = ( 4.427799,  19.4, -8.209310 )
              4 (X,Y,Z) (km)  = ( 1.470831, 4.176369, -8.209310 )
              5 (EX,EY,EZ)    = ( 0.147675, 0.159429, 0.339260 )
              6 CXX,CXY,CXZ,CYY,CYZ,CZZ = 0.0218079551088708, -0.000638279192795213, -0.000749725773138676, 0.0254174912655165, 0.00139172909726588, 0.115097515525919
              7 96.1097639008737 2.39858930837142 1.47083143290329 4.17636917834104 0.0897062757410673 0.0968460018196738 -0.0271104591760068

              2 Average shift of events in B with associated events in A in deg latitude, deg 
                longitude and depth (km)
              3 As line 2, but horizontal shift is expressed in terms of a distance R (km)and 
                a direction theta (deg)
              4 As line 2, but horizontal distances expressed in km rather than degree
              5 Formal errors of the mean shift in km. Note that this is the error of the mean
                and not the standard deviation.
                The formal location errors and covariance matrices of the catalogue events are 
                taken into account in this calculation 
              6 Input line for gmt commandpsvelo.  psvelo expects errors to be specified for 
                68% confidence intervals.
                If covariances are given for a different confidence interval, then they are 
                divided by covscale prior to working out the quantities for psvelo (the error 
                and covariance output lines are always left unchanged).  You can use the 
                following table:

              Input file 
              Confidence    covscale
              68%           1.0
              90%           2.71    [ Value for NEIC and JHD Oxford ]
              95.4%         4.0
              99%           6.63
              99.73%        9.00

              [Defaults: minerr=1.0 ; covscale=2.71 (Native Confidence Interval 90%)]


 Note: all -d options require -i option

Author: F. Tilmann (tilmann at gfz-potsdam.de)  (c) 2002-2011

EOF
}
