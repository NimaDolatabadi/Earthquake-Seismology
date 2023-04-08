#!/bin/bash
# A very simple script in order to get earthquake catalogue from Iranian Seismological Center (IRSC).
# If you're using other distributions then change line NO 10. to your needs.

##### This Script Has Been Written By N.Dolatabadi ######

#### If you were in need, contact me ------> syn_org939@rocketmail.com ######
#========================================================================#
# fist install Dependencies as shown below
sudo apt install curl links

# Let's Define some Variables in which you can fill it
# to your needs.
req="COMPREHENSIVE"	# request Type: ISC Bulletin(COMPREHENSIVE) && Reviewed ISC Bulletin(REVIEWED)
out="CATCSV"		# output format: CSV(CATCSV) && QuakeML(CATQuakeML)
sy="2000"		# start year
sm="01"	 		# start month
sd="01"			# start day
st="00%3A00%3A00"	# start time(HH%3AMM%3ASS)			
ey="2020"		# end year
em="01"			# end month
ed="01"			# end day
et="00%3A00%3A00"	# end time(HH%3AMM%3ASS)
sesh="RECT"		# search shape: Rectangle(RECT) & Circle(CIRC) & Global(GLOBAL) & Flinn-Engdahl(FE)
clat="" 		# central latitude
clon=""			# central longitude
rad=""			# search radius
mdu=""			# search radius Unit (deg / km)
srn=""			# seismic region number (1 to 50)
grn=""			# geographic region number (1 to 757)
minlat="33"		# minimum latitude
maxlat="36"		# maximum latitude
minlon="45"		# minimum longitude
maxlon="48"		# maximum longitude
minmag="2.5"		# minimum magnitude
maxmag="10"		# maximum magnitude
magtype="Any"		# magnitude type(MB/MS/MW/MD/ML)
rma="Any"		# magnitude author (check https://github.com/NimaDolatabadi/Earthquake-Seismology/blob/EE/ISC-Agencies.csv)

# Now it's time to get the catalogue from WWW.ISC.AC.UK

curl -G --cookie cjar --cookie-jar cjar \
	-d "out_format=$out" \
	-d "searchshape=$sesh" \
	-d "bot_lat=$minlat" \
	-d "top_lat=$maxlat" \
	-d "left_lon=$minlon" \
	-d "right_lon=$maxlon" \
	-d "ctr_lat=$clat" \
	-d "ctr_lon=$clon" \
	-d "radius=$rad" \
	-d "max_dist_units=$mdu" \
	-d "srn=$srn" \
	-d "grn=$grn" \
	-d "start_year=$sy" \
	-d "start_month=$sm" \
	-d "start_day=$sd" \
	-d "start_time=$st" \
	-d "end_year=$ey" \
	-d "end_month=$em" \
	-d "end_day=$ed" \
	-d "end_time=$et" \
	-d "min_mag=$minmag" \
	-d "max_mag=$maxmag" \
	-d "req_mag_type=$magtype" \
	-d "req_mag_agcy=$rma" \
	-d "include_links=on" \
		--location \
		--output Result.html \
			http://www.isc.ac.uk/cgi-bin/web-db-run
# Just adding some modificatoin so thats it would be nice in view and usage
links -dump Result.html > Result.txt
end_no=`cat Result.txt | grep -n STOP | awk -F ":" '{print $1-1}'`
cat Result.txt | sed -e "$end_no,\$d" | sed -e '1,28d' | awk -F " " '{for(i=1;i<=NF;i++) printf "%s ",$i; print ""}' > Result-Regular.txt
#cat Result-Regular.txt | awk -F "]" '{print $2}' | awk -F "[/:]" '{print $1,$2,$3,$4,$5}' | sort > Final-IRSC-Catalogue.txt
#sed -i '1i Year Mon Day Hour Min Sec Lat Lon Depth MN' Final-IRSC-Catalogue.txt
#sed -i 's/ /,/g' Final-IRSC-Catalogue.txt
#rm -r Result.html Result.txt Result-Regular.txt cjar
#mv Final-IRSC-Catalogue.txt IRSC.$sy$sm$sd.$ey$em$ed.csv

echo 'WELL DONE'
