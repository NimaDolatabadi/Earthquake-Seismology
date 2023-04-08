#!/bin/bash
# A very simple script in order to get earthquake catalogue from Iranian Seismological Center (IRSC).
# If you're using other distributions then change line NO 10. to your needs.

##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######
#========================================================================#
# fist install Dependencies as shown below
sudo apt-get install curl lynx

# Let's Define some Variables in which you can fill it
# to your needs.
out="CATCSV"
sy="2006"	# start year
sm="01"	 	# start month
sd="01"		# start day
ey="2020"	# end year
em="01"		# end month
ed="01"		# end day
sesh="RECT"	# search shape
minlat="33"	# minimum latitude
maxlat="36"	# maximum latitude
minlon="45"	# minimum longitude
maxlon="48"	# maximum longitude
minmag="2.5"	# minimum magnitude
maxmag="10"	# maximum magnitude
magtype="any"	# magnitude type

# Now it's time to get the catalogue from WWW.ISC.AC.UK

curl -G --cookie cjar --cookie-jar cjar \
	-d "out_format=$out" \
	-d "searchshape=$sesh" \
	-d "bot_lat=$minlat" \
	-d "top_lat=$maxlat" \
	-d "left_lon=$minlon" \
	-d "right_lon=$maxlon" \
	-d "start_year=$sy" \
	-d "start_month=$sm" \
	-d "start_day=$sd" \
	-d "end_year=$ey" \
	-d "end_month=$em" \
	-d "end_day=$ed" \
	-d "min_mag=$minmag" \
	-d "max_mag=$maxmag" \
	-d "req_mag_type=$magtype" \
	-d "ctr_lat=" \
	-d "ctr_lon=" \
	-d "radius=" \
	-d "max_dist_units=deg" \
	-d "srn=" \
	-d "grn=" \
	-d "start_time=00%3A00%3A00" \
	-d "end_time=00%3A00%3A00" \
	-d "req_mag_agcy=" \
	-d "include_links=on" \
		--location \
		--output Result.html \
			http://www.isc.ac.uk/cgi-bin/web-db-run
# Just adding some modificatoin so thats it would be nice in view and usage
#lynx --dump Result.html > Result.txt
#cat Result.txt | sed -e '1,46d' | sed '/Gap/d;/file/d;/Version/d;/ID/d;/^$/d' | awk -F " " '{print $3,$4,$5,$6,$7,$8}' | awk NF > Result-Regular.txt
#cat Result-Regular.txt | awk -F "]" '{print $2}' | awk -F "[/:]" '{print $1,$2,$3,$4,$5}' | sort > Final-IRSC-Catalogue.txt
#sed -i '1i Year Mon Day Hour Min Sec Lat Lon Depth MN' Final-IRSC-Catalogue.txt
#sed -i 's/ /,/g' Final-IRSC-Catalogue.txt
#rm -r Result.html Result.txt Result-Regular.txt cjar
#mv Final-IRSC-Catalogue.txt IRSC.$sy$sm$sd.$ey$em$ed.csv

echo 'WELL DONE'

