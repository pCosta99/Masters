package View;



import Model.SystemUser;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

public class InputHelper {
    public static Predicate<Object> greaterThanPredicate(int num) {
        return (p-> ((Integer) p) > num);
    }

    public static BetterPredicate greaterThan(int num) {
        return new BetterPredicate(greaterThanPredicate(num),("um inteiro maior que " + num));
    }

    public static Predicate<Object> betweenEqPredicate(int num1, int num2) {
        return (p-> ((Integer) p) >= num1 && ((Integer) p) <= num2);
    }

    public static BetterPredicate betweenEq(int num1, int num2) {
        return new BetterPredicate(betweenEqPredicate(num1,num2),("um inteiro entre " + num1 + " e " + num2));
    }


    public static Predicate<Object> isMonthPredicate() {
        return (p-> ((Integer) p) >= 1 && ((Integer) p) <= 12);
    }

    public static BetterPredicate isMonth() {
        return new BetterPredicate(isMonthPredicate(),("um mês (1 a 12)"));
    }

    public static Predicate<Object> fileNamePredicate() {
        return (p-> true);
    }

    public static BetterPredicate fileName() {
        return new BetterPredicate(fileNamePredicate(),("o nome do ficheiro (inclua '.dat')"));
    }

    public static BetterPredicate isAny() {
        return new BetterPredicate(p->true,(""));
    }

    public static BetterPredicate isUser(Map<String, SystemUser> users) {
        return new BetterPredicate(p->users.containsKey((String)p),(""));
    }

    public static List<String> monthList() {
        return Arrays.asList("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez");
    }

    public static Map<Character,Integer> navDefaultCommands() {
        return new HashMap<>() {{
            put('X',-1);
            put('B',0);
            put('N',1);
            put('S',2);
        }};
    }

    public static Map<Character,Integer> tableDefaultCommands() {
        return new HashMap<>() {{
            put('X',-1);
        }};
    }

    public static Map<Character,Integer> encDefaultCommands() {
        return new HashMap<>() {{
            put('X',-1);
            put('B',0);
            put('A',1);
            put('C',2);
        }};
    }


    public enum TYPE {
        INTEGER,CHAR,STRING
    }

}
