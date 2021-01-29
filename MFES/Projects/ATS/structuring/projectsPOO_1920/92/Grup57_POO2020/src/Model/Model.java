package Model;

import View.View;
import Model.TrazAqui;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Scanner;
import java.util.Queue;
import java.lang.Character;
import java.util.List;
import java.util.AbstractMap.SimpleEntry;
import java.util.Collections;

public class Model {

	public void signIn(ITrazAqui trazAqui) {
        int aceitaFila = 1000;
	    String userName = new String();
        String pw = new String();
        String nome = new String();
        String email = new String();
        String nif = new String();
        double x = 0.0;
        double y = 0.0;
        double raio = 0.0;
        double precoKM = 0.0;
        boolean aceita_Fila = false;
        Queue<Encomenda> q = new LinkedList<>();
        int quemFala = 1000;
        Scanner sc = new Scanner(System.in);
        View.quemFala();
        quemFala = Integer.parseInt(sc.next());
        View.printSpace();
        while (quemFala != 1 && quemFala != 2 && quemFala != 3 && quemFala != 4) {
            View.deveSerDe1a4();
            quemFala = Integer.parseInt(sc.next());
            View.printSpace();
        }
        View.insertUserName();
        userName = sc.next();
        View.printSpace();
        while ((!Auxiliar.validUserName(userName)) || (trazAqui.checkUserName(userName))) {
            if (!Auxiliar.validUserName(userName)) {
                View.invalidUserName();
                userName = sc.next();
                View.printSpace();
            }
            else {
                View.userNameAlreadyExists();
                userName = sc.next();
                View.printSpace();
            }
        }
        View.insertPassWord();
        pw = sc.next();
        View.printSpace();
        View.insertName();
        nome = sc.next();
        View.printSpace();
        View.insertGPSX();
        x = Double.parseDouble(sc.next());
        View.printSpace();
        View.insertGPSY();
        y = Double.parseDouble(sc.next());
        View.printSpace();
        View.insertEmail();
        email = sc.next();
        View.printSpace();
        switch (quemFala) {
            case 1 -> {
                trazAqui.putUtilizador(userName, new Utilizador(userName,nome,new GPS(x, y),email));
                trazAqui.putLogin(userName,pw);
                View.registoComSucesso();
            }
            case 2 -> {
                View.insertRaio();
                raio = Double.parseDouble(sc.next());
                View.printSpace();
                trazAqui.putVoluntario(userName,new Voluntario(userName,nome,new GPS(x,y),email,raio,new ArrayList<>(),true));
                trazAqui.putLogin(userName,pw);
                View.registoComSucesso();
            }
            case 3 -> {
                View.insertNIF();
                nif = sc.next();
                View.printSpace();
                View.insertRaio();
                raio = Double.parseDouble(sc.next());
                View.printSpace();
                View.insertPrecoKM();
                precoKM = Double.parseDouble(sc.next());
                View.printSpace();
                trazAqui.putTransportadora(userName,new Transportadora(userName,nome,new GPS(x,y),email,nif,raio,precoKM,new ArrayList<>(),true));
                trazAqui.putLogin(userName,pw);
                View.registoComSucesso();
            }
            case 4 -> {
                View.insertAceitaFila();
                aceitaFila = Integer.parseInt(sc.next());
                View.printSpace();
                while (aceitaFila != 1 && aceitaFila != 2) {
                    View.deveSerDe1a2();
                    aceitaFila = Integer.parseInt(sc.next());
                    View.printSpace();
                }
                if (aceitaFila == 1) {
                    aceita_Fila = true;
                }
                else {
                    aceita_Fila = false;
                }
                trazAqui.putLoja(userName,new Loja(userName,nome,new GPS(x, y),email,aceita_Fila,new ArrayList<>()));
                trazAqui.putLogin(userName,pw);
                View.registoComSucesso();
            }
        }
    }

    public int voluntarioOuTransportadora(String cod) {
	    Character letter = cod.charAt(0);
	    if (letter == 'v') {
	        return 0; // Caso Tenha Sido Um Voluntário.
        }
	    else {
	        return 1; // Caso Tenha Sido Uma Transportadora.
        }
    }

    public List<SimpleEntry<String,Integer>> getTopUsers (ITrazAqui trazAqui) {
        List<String> aux = new ArrayList<>();
        List<SimpleEntry<String,Integer>> res = new ArrayList<>();
        int ocup = 0;
        for (RegistoEncomenda r : trazAqui.exportRegistos().getRegistos()) {
            aux.add(r.getQuemEncomendou());
        }
        Collections.sort(aux);
        for (SimpleEntry<String, Integer> s : joinRepsU(aux)) {
            res.add(s);
            ocup++;
            if(ocup == 10) break;
        }
        return res;
    }

    private List<SimpleEntry<String,Integer>> joinRepsU (List<String> list) {
        List<SimpleEntry<String,Integer>> aux = new ArrayList<>();
        int i = 0;
        int j = 0;
        int l = list.size() - 1;
        for (i = 0; i < l;) {
            SimpleEntry<String, Integer> s = new SimpleEntry<String,Integer>(list.get(i), 1);
            for (j = i + 1; list.get(i).equals(list.get(j)) && j < l; j++) {
                s.setValue(s.getValue() + 1);
            }
            i = j;
            aux.add(s);
        }
        Collections.sort(aux,new CompareByUsage());
        return aux;
    }

    public List<SimpleEntry<String,Double>> getTopCarriers (ITrazAqui trazAqui) {
        List<SimpleEntry<String,Double>> aux = new ArrayList<>();
        List<SimpleEntry<String,Double>> res = new ArrayList<>();
        int ocup = 0;
        for (RegistoEncomenda r : trazAqui.exportRegistos().getRegistos()) {
            aux.add(new SimpleEntry<String,Double>(r.getQuemTransportou(),r.getDistanciaPercorrida()));
        }
        Collections.sort(aux,new CompareByName());
        for (SimpleEntry<String,Double> s : joinRepsT(aux)) {
            res.add(s);
            ocup++;
            if (ocup == 10) break;
        }
        return res;
    }

    private List<SimpleEntry<String,Double>> joinRepsT (List<SimpleEntry<String,Double>> list) {
        List<SimpleEntry<String,Double>> aux = new ArrayList<>();
        int i = 0;
        int j = 0;
        int l = list.size() - 1;
        for (i = 0; i < l;) {
            SimpleEntry<String,Double> s = new SimpleEntry<String,Double>(list.get(i).getKey(),list.get(i).getValue());
            for (j = i + 1; list.get(i).getKey().equals(list.get(j).getKey()) && j < l; j++) {
                s.setValue(s.getValue() + list.get(j).getValue());
            }
            i = j;
            aux.add(s);
        }
        Collections.sort(aux,new CompareByKM());
        return aux;
    }
}
