package View;

import Controller.Controller;
import Helpers.ITriplet;
import Helpers.Triplet;
import Model.SystemUser;
import Model.Utilizador;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static View.InputHelper.TYPE.STRING;
import static View.InputHelper.*;

public class ViewHelper {

    // ---------------------------------------------------------------
    //Faz mais sentido estarem numa classe auxiliar

    public static final int MARGIN_TOP = 0;
    public static final int MARGIN_RIGHT = 1;
    public static final int MARGIN_BOTTOM = 2;
    public static final int MARGIN_LEFT = 3;

    public static final String SEP_LINE = "│";
    public static final String SEP_LINEBOLD = "┃";
    public static final String SEP_LINEDASHED = "┆";
    public static final String SEP_TWOLINES = "║";

    public static final String HSEP_LINE = "─";
    public static final String HSEP_TWOLINES = "═";

    // Reset
    public static final String RESET = "\033[0m";  // Text Reset

    // Regular Colors
    public static final String BLACK = "\033[0;30m";   // BLACK
    public static final String RED = "\033[0;31m";     // RED
    public static final String GREEN = "\033[0;32m";   // GREEN
    public static final String YELLOW = "\033[0;33m";  // YELLOW
    public static final String BLUE = "\033[0;34m";    // BLUE
    public static final String PURPLE = "\033[0;35m";  // PURPLE
    public static final String CYAN = "\033[0;36m";    // CYAN
    public static final String WHITE = "\033[0;37m";   // WHITE

    // Bold
    public static final String BLACK_BOLD = "\033[1;30m";  // BLACK
    public static final String RED_BOLD = "\033[1;31m";    // RED
    public static final String GREEN_BOLD = "\033[1;32m";  // GREEN
    public static final String YELLOW_BOLD = "\033[1;33m"; // YELLOW
    public static final String BLUE_BOLD = "\033[1;34m";   // BLUE
    public static final String PURPLE_BOLD = "\033[1;35m"; // PURPLE
    public static final String CYAN_BOLD = "\033[1;36m";   // CYAN
    public static final String WHITE_BOLD = "\033[1;37m";  // WHITE

    // Background
    public static final String BLACK_BACKGROUND = "\033[40m";  // BLACK
    public static final String RED_BACKGROUND = "\033[41m";    // RED
    public static final String GREEN_BACKGROUND = "\033[42m";  // GREEN
    public static final String YELLOW_BACKGROUND = "\033[43m"; // YELLOW
    public static final String BLUE_BACKGROUND = "\033[44m";   // BLUE
    public static final String PURPLE_BACKGROUND = "\033[45m"; // PURPLE
    public static final String CYAN_BACKGROUND = "\033[46m";   // CYAN
    public static final String WHITE_BACKGROUND = "\033[47m";  // WHITE

    public static final String BRIGHTBLACK_BACKGROUND = "\033[100m";  // BRIGHTBLACK_BACKGROUND

    // Bold High Intensity
    public static final String BLACK_BOLD_BRIGHT = "\033[1;90m"; // BLACK
    public static final String RED_BOLD_BRIGHT = "\033[1;91m";   // RED
    public static final String GREEN_BOLD_BRIGHT = "\033[1;92m"; // GREEN
    public static final String YELLOW_BOLD_BRIGHT = "\033[1;93m";// YELLOW
    public static final String BLUE_BOLD_BRIGHT = "\033[1;94m";  // BLUE
    public static final String PURPLE_BOLD_BRIGHT = "\033[1;95m";// PURPLE
    public static final String CYAN_BOLD_BRIGHT = "\033[1;96m";  // CYAN
    public static final String WHITE_BOLD_BRIGHT = "\033[1;97m"; // WHITE

    public static final String BLINK = "\033[0;5m";   // BLINK


    public static final Integer[] defaultMargins = {0,1,0,1};

    public static void printSpace(int n) {
        //todo - refactor
        for(int i = 0; i<n; i++) {
            System.out.print(" ");
        }
    }

    public static void clearScreen() {
        System.out.print("\033[H\033[J");
    }

    public static Line newLine(String string) {
        return new Line(string);
    }


    private Block gatherInputCommands() {
        Block b = new Block();
        b.addLine(new Line(" COMANDOS: X - Cancelar Querie | B - Voltar atrás ").color(0,BLUE_BOLD));
        return b;
    }




    public static Map<Integer,String> optionsToInfo(Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> options) {
        //return options.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, en->en.getValue().getFirst()));
        Map<Integer,String> map = new LinkedHashMap<>();
        options.forEach((key, value) -> map.put(key, value.getFirst()));
        return map;
    }




    public static InputSequence loginInputSeq(String typeOfUser, Controller c) {
        Map<String, SystemUser> users = specifiedUsers(typeOfUser,c);
        return  new InputSequence(
                Arrays.asList(
                        new AbstractMap.SimpleEntry<>(STRING, isUser(users)),
                        new AbstractMap.SimpleEntry<>(STRING, isAny())
                )
        );
    }

    public static Map<String,SystemUser> specifiedUsers(String type, Controller c) {
        switch (type) {
            case "Cliente":
                return c.getUtilizadores();
            case "Loja":
                return c.getLojas();
            case "Transportadora":
                return c.getTransportadoras();
            case "Voluntário":
                return c.getVoluntarios();
            default:
                return null;
        }
    }

    public static Block inputBlock(InputSequence seq, int width, String title, List<String> labels) {
        Block input = new BoxBlock(width," "+title);
        List<Object> args = seq.getArgs();
        int i;
        for(i = 0; i<args.size() && args.get(i) != null; i++) {
            input.addLine(newLine(labels.get(i)).colorBG(BLACK+WHITE_BACKGROUND));
            input.addLine(newLine(args.get(i).toString()));
        }
        if(i<=args.size()) {
            input.addLine(newLine(labels.get(i)).colorBG(BLACK+WHITE_BACKGROUND));
            input.addLine(newLine(" >").colorBG(BLINK+BLUE_BACKGROUND));
            i++;
        }
        for(; i<args.size();i++) {
            input.addLine(newLine(labels.get(i)).colorBG(BLACK+WHITE_BACKGROUND));
            input.addLine(newLine(""));
        }
        return input;
    }


    public static Block inputBlockEnc(InputSequenceEnc seq, int width, String title, List<String> labels) {
        Block input = new BoxBlock(width," "+title);
        List<Object> args = seq.getArgs();
        int i;
        for(i = 0; i<args.size() && args.get(i) != null; i++) {
            input.addLine(newLine(labels.get(i)).colorBG(BLACK+WHITE_BACKGROUND));
            input.addLine(newLine(args.get(i).toString()));
        }
        if(i<=args.size()) {
            input.addLine(newLine(labels.get(i)).colorBG(BLACK+WHITE_BACKGROUND));
            input.addLine(newLine(" >").colorBG(BLINK+BLUE_BACKGROUND));
            i++;
        }
        for(; i<args.size();i++) {
            input.addLine(newLine(labels.get(i)).colorBG(BLACK+WHITE_BACKGROUND));
            input.addLine(newLine(""));
        }
        return input;
    }



}
