# StayawayCovid API

How to start the StayawayCovid application
---

1. Run `mvn clean install` to build your application
1. Start application with `java -jar target/StayawayCovid-1.jar server StayawayCovid.yml`
1. To check that your application is running enter url `http://localhost:8080`

Health Check
---

To see your applications health enter url `http://localhost:8081/healthcheck`

### Requests
```
Type - Path
GET
	/{district}/Users -> Get users count from a given district
	
GET
	/{district}/Infecteds -> Get infected users count from a given district
	
GET
	/TopRatio -> Get top 5 districts with higher ratio infected/users
GET
	/Location/MostUsers -> Get top 5 location with the max simultaneous user register
GET
	/AvgInfectedContacts -> Get number of average contacts each infected pacient had

POST
	/District -> Add one district
	body:
		{
			district: String,
			size: Integer
		}
	Example: http://localhost:8080/District?district=Braga&size=10

POST
	/User -> Adds a new user to a district and location
	body:
		{
			district: String,
			latitude: Integer,
			longitude: Integer
		}
POST
	/Infected -> Add a infected user to a district and location	
	body:
		{
			district: String,
			contacts: Integer
		}
PUT
	/Move -> Moves a user inside a district between to location
	body:
		{
			district: String,
			initialLatitude: Integer,
			initialLongitude: Integer,
			finalLatitude: Integer,
			finalLongitude: Integer		
		}
```
