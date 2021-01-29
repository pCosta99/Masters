package model;

import java.io.Serializable;
import java.util.Random;

public class RandomGenerator implements Serializable {

    public static String generateEncomenda() {
        StringBuilder sb = new StringBuilder(4);

        sb.append("e");

        for (int i = 0; i < 4; i++) {

            String AlphaNumericString = "0123456789";

            int index = (int)(AlphaNumericString.length() * Math.random());

            sb.append(AlphaNumericString.charAt(index));
        }
        return sb.toString();
    }

    public static int generateInt(int start, int end) {
        return start + (int) Math.round(Math.random() * (end - start));
    }
/*
    public static LocalDateTime generateDate() {
        int month = generateInt(1, 12);
        int day;

        if(month == 2) day = generateInt(1, 28);
        else day = generateInt(1, 30);

        return LocalDateTime.of(2020,month,day,0,0);
    }*/

    public static boolean generateBoolean() {
        Random rand = new Random();
        return rand.nextBoolean();
    }

}
