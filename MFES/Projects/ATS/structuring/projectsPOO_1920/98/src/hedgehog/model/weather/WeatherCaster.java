package hedgehog.model.weather;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Random;

public final class WeatherCaster {
    private static final Random rng;
    private static LocalDate last_asked;
    private static Weather last_weather;

    static {
        rng = new Random();
        last_asked = LocalDate.now();
        last_weather = generate_random_weather();
    }

    private WeatherCaster() {}

    public enum Weather {
        SUNNY(68),
        RAINY(30),
        SNOWY(2);

        private final int probabilty;
        Weather(final int probabilty) {
            this.probabilty = probabilty;
        }
    }

    private static Weather generate_random_weather() {
        final var rand = rng.nextInt(100);  // [0, 100)

        if (rand < Weather.SNOWY.probabilty) {
            return Weather.SNOWY;
        }
        if (rand < Weather.RAINY.probabilty + Weather.SNOWY.probabilty) {
            return Weather.RAINY;
        }

        return Weather.SUNNY;
    }

    public static Weather ask_weather() {
        final var current_date = LocalDate.now();

        if (ChronoUnit.DAYS.between(last_asked, current_date) > 0) {
            last_asked = current_date;
            last_weather = generate_random_weather();
        }

        return last_weather;
    }
}
