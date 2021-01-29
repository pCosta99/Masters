package View;

import Helpers.IPair;
import Helpers.Pair;

import java.util.InputMismatchException;
import java.util.Map;
import java.util.Scanner;

import static java.lang.System.in;
import static java.lang.System.out;

public class Input {

    public static void readAny() {
        Scanner input = new Scanner(in);
        input.nextLine();
    }

    public static IPair<Integer,Integer> readInt(BetterPredicate p, IView v, Window w, boolean isInSequece) {
        IPair<Integer,Integer> ret = new Pair<>(1,0);
        Scanner input = new Scanner(in);
        boolean ok = false;
        String txt = "";
        v.showWindow(w);
        v.printStringBright("Introduza " + p.getInstruction());
        while(!ok) {
            v.printInputLine();
            try {
                txt = input.nextLine();
                if(txt.length() == 1) {
                    if(isInSequece && Character.toUpperCase(txt.charAt(0)) == 'X')  {
                        ret.setFirst(-1);
                        return ret;
                    }
                    else if(isInSequece && Character.toUpperCase(txt.charAt(0)) == 'B')  {
                        ret.setFirst(0);
                        return ret;
                    }
                }
                if(p.test(Integer.parseInt(txt))) {
                    ok = true;
                } else {
                    v.showWindow(w);
                    v.printStringError("Valor Inválido");
                    v.printStringBright("Introduza " + p.getInstruction());
                }

            }
            catch(Exception e) {
                v.showWindow(w);
                v.printStringError("Valor Inválido");
                v.printStringBright("Introduza " + p.getInstruction());
            }
        }
        //input.close();
        ret.setSecond(Integer.parseInt(txt));
        return ret;
    }

    public static IPair<Integer,String> readString(BetterPredicate p, IView v, Window w) {
        IPair<Integer,String> ret = new Pair<>(1,"");
        Scanner input = new Scanner(in);
        boolean ok = false;
        String txt = "";
        v.showWindow(w);
        v.printStringBright("Introduza " + p.getInstruction());
        while(!ok) {
            v.printInputLine();
            try {
                txt = input.nextLine();
                if(txt.length() == 1) {
                    if(Character.toUpperCase(txt.charAt(0)) == 'X')  {
                        ret.setFirst(-1);
                        return ret;
                    }
                    else if(Character.toUpperCase(txt.charAt(0)) == 'B')  {
                        ret.setFirst(0);
                        return ret;
                    }
                }
                if(p.test(txt)) {
                    ok = true;
                } else {
                    v.showWindow(w);
                    v.printStringError("Valor Inválido");
                    v.printStringBright("Introduza " + p.getInstruction());
                }

            }
            catch(Exception e) {
                v.showWindow(w);
                v.printStringError("Valor Inválido");
                v.printStringBright("Introduza " + p.getInstruction());
            }
        }
        //input.close();
        ret.setSecond(txt);
        return ret;
    }


    public static int readOption(Map<Character,Integer> commands, IView v, Window w) {
        Scanner input = new Scanner(in);
        int ret = -10;
        boolean ok = false;
        String txt = "";
        v.showWindow(w);
        v.printStringBright("Introduza uma opção válida");
        while(!ok) {
            v.printInputLine();
            try {
                txt = input.nextLine();

                if(txt.length() == 1) {
                    if( commands.containsKey(Character.toUpperCase(txt.charAt(0))))  {
                        ret = commands.get(Character.toUpperCase(txt.charAt(0)));
                        return ret;
                    }
                }
                v.showWindow(w);
                v.printStringError("Opção Inválida");
                v.printStringBright("Introduza uma opção válida");

            }
            catch(Exception e) {
                v.showWindow(w);
                v.printStringError("Opção Inválida");
                v.printStringBright("Introduza uma opção válida");
            }
        }
        return ret;
    }

    public static String lerString() {
        Scanner input = new Scanner(in);
        boolean ok = false;
        String txt = "";
        while(!ok) {
            try {
                txt = input.nextLine();
                ok = true;
            }
            catch(InputMismatchException e)
            { out.println("Texto Invalido");
                out.print("Novo valor: ");
                input.nextLine();
            }
        }
        //input.close();
        return txt;
    }


}
