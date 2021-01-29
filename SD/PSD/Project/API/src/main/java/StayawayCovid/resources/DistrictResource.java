package StayawayCovid.resources;

import StayawayCovid.api.*;
import StayawayCovid.models.*;
import com.codahale.metrics.annotation.Timed;

import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.NotFoundException;

import java.util.concurrent.ConcurrentHashMap;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.Comparator;
import java.util.Collections;

@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DistrictResource {

    private final Map<String, District> districts;

    public DistrictResource() {
        districts = new ConcurrentHashMap<>();
    }

    @GET
    @Timed
    @Path("/{district}/Users")
    public Response getDistrictUsers(
        @NotNull @PathParam("district") String district) {
        District dist = districts.get(district);
        if(dist == null) 
            throw new WebApplicationException(Response.Status.NOT_FOUND);
        return Response.ok(dist.getUsersCount()).build();
    }

    @GET
    @Timed
    @Path("/{district}/Infecteds")
    public Response getDistrictInfecteds(
        @NotNull @PathParam("district") String district) {
        District dist = districts.get(district);
        if(dist == null) 
            throw new WebApplicationException(Response.Status.NOT_FOUND);
        return Response.ok(dist.getInfectedCount()).build();
    }

    @GET
    @Timed
    @Path("/TopRatio")
    public Response getTopRatio() {
        Map<String,Float> districtsRatio = new HashMap<>();
        districts.forEach((k,v) -> districtsRatio.put(k,v.getInfectedContactsRatio()));

        List<String> topN = districtsRatio.entrySet().stream()
            .sorted(Map.Entry.comparingByValue())
            .map(Map.Entry::getKey)
            .limit(5)
            .collect(Collectors.toList());

        return Response.ok(topN).build();
    }

    @GET
    @Timed
    @Path("/Location/MostUsers")
    public Response getLocationsMostUsers() {
        List<Location> locations = new ArrayList<>();
        districts.values().forEach( district -> {
            district.getLocations().values().forEach(local -> locations.add(local));
        });

        List<Location> mostUsersLocal = new ArrayList<>(locations);

        Collections.sort(mostUsersLocal, new Comparator<Location>() {
            @Override
            public int compare(Location l1, Location l2) {
                return -l1.getMaxUsers().compareTo(l2.getMaxUsers());
            }
        });

        mostUsersLocal.sort(Comparator.comparingInt(l -> l.getUsers()));
        Collections.reverse(mostUsersLocal);
        List<Location> ret = mostUsersLocal.stream().limit(5).collect(Collectors.toList());

        return Response.ok(ret).build();
    }

    @GET
    @Timed
    @Path("/AvgInfectedContacts")
    public Response getAvgInfectedContacts() {
        int contacts = districts.values().stream().mapToInt(v -> v.getInfectedContacts()).sum();
        int infected = districts.values().stream().mapToInt(v -> v.getInfectedCount()).sum();

        return Response.ok(((float)contacts)/(float)infected).build();
    }


    @POST   
    @Timed
    @Path("/District")
    public Response addDistrict(@NotNull DistrictModel district) {
        if(districts.containsKey(district.getDistrict()))
            throw new WebApplicationException(Response.Status.FORBIDDEN);
        districts.put(district.getDistrict(), new District(district.getDistrict(), district.getSize()));
        return Response.ok().build();
    }

    @POST   
    @Timed
    @Path("/User")
    public Response addUser(@NotNull UserModel user) {
        District dist = districts.get(user.getDistrictName());
        if(dist == null) 
            throw new WebApplicationException(Response.Status.NOT_FOUND);
        dist.addUser(user.getLatitude(), user.getLongitude());
        districts.put(dist.getName(), dist);
        return Response.ok().build();
    }

    @POST
    @Timed 
    @Path("/Infected")
    public Response addInfected(@NotNull InfectedModel infected) {
        District dist = districts.get(infected.getDistrict());
        if(dist == null) 
            throw new WebApplicationException(Response.Status.NOT_FOUND);
        dist.addInfected(infected.getContacts());
        districts.put(dist.getName(),dist);
        return Response.ok().build();
    }

    @PUT
    @Timed 
    @Path("/MoveUser")
    public Response moveUser(@NotNull LocationUpdateModel locationUpdateModel) {
        District dist = districts.get(locationUpdateModel.getDistrict());
        if(dist == null) throw new WebApplicationException(Response.Status.NOT_FOUND);
        
        try{
            dist.move(locationUpdateModel.getInitialLatitude(), locationUpdateModel.getInitialLongitude(),
                    locationUpdateModel.getFinalLatitude(), locationUpdateModel.getFinalLongitude());
        } catch(NotFoundException e) {
            throw new WebApplicationException(Response.Status.NOT_FOUND);
        }

        districts.put(dist.getName(),dist);
        return Response.ok().build();
    }
}
