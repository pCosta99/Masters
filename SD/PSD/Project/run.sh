#!/bin/sh

# Running API
cd API
apijar='ls target | grep StayawayCovid-1.jar'
if [ $apijar ] 
	then
		mvn clean install
	fi
java -jar target/StayawayCovid-1.jar server StayawayCovid.yml &

#Run District Server for each yml
cd ../DistrictServer
districtjar='ls target | grep DistrictServer-1.0-SNAPSHOT-jar-with-dependencies.jar'
if [ $districtjar ] 
	then
		mvn clean compile assembly:single
	fi
for yaml in configs/*
do
	java -jar target/DistrictServer-1.0-SNAPSHOT-jar-with-dependencies.jar $yaml &
done

#Run Broker
cd ../Broker
brokerjar='ls target | grep Broker-1.0-SNAPSHOT-jar-with-dependencies.jar'
if [ $brokerjar ] 
	then
		mvn clean compile assembly:single
	fi
java -jar target/Broker-1.0-SNAPSHOT-jar-with-dependencies.jar 22221 22222 &
