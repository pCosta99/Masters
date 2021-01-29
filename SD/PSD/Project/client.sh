#!/bin/sh

# Running Client
cd Client
clientjar='ls target | grep Client-1.0-jar-with-dependencies.jar'
if [ $clientjar ] 
	then
		mvn clean compile assembly:single
	fi
pwd
pwd="`pwd`"
java -jar target/Client-1.0-jar-with-dependencies.jar "$pwd/src/main/java/config.json" &
