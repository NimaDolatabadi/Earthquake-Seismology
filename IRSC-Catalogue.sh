#!/bin/bash

##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######
#========================================================================#
# fist install Dependencies as shown below
sudo apt-get install curl lynx

# Let's Define some Variables in which you can fill it
# to your needs.
sy="2013"
sm="01"
sd="01"
ey="2020"
em="03"
ed="19"
minlat="29"
maxlat="31"
minlon="55.5"
maxlon="60"
minmag="2.5"
maxmag="10"
# Now it's time to get the catalogue from IRSC.UT.AC.IR
curl --cookie cjar --cookie-jar cjar \
	-d "start_Y=$sy" \
	-d "start_M=$sm" \
	-d "start_d=$sd" \
	-d "end_Y=$ey" \
	-d "end_M=$em" \
	-d "end_D=$ed" \
	-d "min_lat=$minlat" \
	-d "max_lat=$maxlat" \
	-d "min_lon=$minlon" \
	-d "max_lon=$maxlon" \
	-d "min_mag=$minmag" \
	-d "max_mag=$maxmag" \
	-d "action=Search" \
		--location \
		--output Result.html \
			http://irsc.ut.ac.ir/bulletin.php
# Just adding some modificatoin so thats it would be nice in view and usage
lynx --dump Result.html > Result.txt
cat Result.txt | sed -e '1,46d' | sed '/Gap/d;/file/d;/Version/d;/ID/d;/^$/d' | awk -F " " '{print $3,$4,$5,$6,$7,$8}' | awk NF > Result-Regular.txt
cat Result-Regular.txt | awk -F "]" '{print $2}' | awk -F "[/:]" '{print $1,$2,$3,$4,$5}' | sort > Final-IRSC-Catalogue.txt
sed -i '1i Year Mon Day Hour Min Sec Lat Lon Depth MN' Final-IRSC-Catalogue.txt
rm -r Result.html Result.txt Result-Regular.txt
mv Final-IRSC-Catalogue.txt IRSC.$sy$sm$sd.$ey$em$ed.txt

echo 'WELL DONE'
