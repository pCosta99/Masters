package StayawayCovid;

import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

import StayawayCovid.resources.DistrictResource;

public class StayawayCovidApplication extends Application<StayawayCovidConfiguration> {

    public static void main(final String[] args) throws Exception {
        new StayawayCovidApplication().run(args);
    }

    @Override
    public String getName() {
        return "StayawayCovid";
    }

    @Override
    public void initialize(final Bootstrap<StayawayCovidConfiguration> bootstrap) {
        // TODO: application initialization
    }

    @Override
    public void run(final StayawayCovidConfiguration configuration,
                    final Environment environment) {
        final DistrictResource districtResource = new DistrictResource();

        environment.jersey().register(districtResource);
        // TODO: implement application
    }

}
