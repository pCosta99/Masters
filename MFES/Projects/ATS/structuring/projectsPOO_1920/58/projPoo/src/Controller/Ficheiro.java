package Controller;

import Models.*;
import jdk.jshell.execution.Util;

import java.io.*;

public class Ficheiro implements Serializable{


    /**
     * Método que lê do ficheiro logs e escreve cada linha desse numa String que depois é adicionada a um array de Strings.
     *
     * @return Devolve o array de Strings.
     * @throws IOException Exception.
     */
    public static String[] lerLogs() throws IOException {
        String[] logs = new String[1500];
        FileReader file = new FileReader("logs.txt");
        BufferedReader lerLogs = new BufferedReader(file);
        String linha = lerLogs.readLine();
        int i = 0;
        while (linha != null) {
            logs[i] = linha;
            i++;
            linha = lerLogs.readLine();
        }
        return logs;
    }

    /**
     * Função que lê linha a linha e regista os várias classes pertencentes ao Sistema.
     * @param s Recebe um Sistema.
     * @param logs Recebe um array de Strings.
     */
    public static void lerS(Sistema s, String[] logs) {
        String[] p1;
        String[] p2;
        for (String log : logs) { //lê cada linha do array de strings.
            if (log == null) break;
            p1 = log.split(":");
            p2 = p1[1].split(",");
            switch (p1[0]) {
                case "Utilizador":
                    s.registaUtilizador(p2[0],p2[1],Double.parseDouble(p2[2]),Double.parseDouble(p2[3]));
                    break;

                case "Voluntario":
                    s.registaVoluntario(p2[0],p2[1],Double.parseDouble(p2[2]),Double.parseDouble(p2[3]),Double.parseDouble(p2[4]));
                    break;

                case "Transportadora":
                    s.registaTransportadora(p2[0],p2[1],Double.parseDouble(p2[2]),Double.parseDouble(p2[3]),Integer.parseInt(p2[4]),Double.parseDouble(p2[5]),Double.parseDouble(p2[6]));
                    break;

                case "Loja":
                    s.registaLoja(p2[0],p2[1],Double.parseDouble(p2[2]),Double.parseDouble(p2[3]));
                    break;

                case "Encomenda":
                    Encomenda e = s.fazerEncomenda2(p2[0],p2[1],p2[2],Double.parseDouble(p2[3]),false);

                    for (int i = 4; i < p2.length; i += 4){
                        e.addProduto(p2[i],p2[i+1],Double.parseDouble(p2[i+2]),Double.parseDouble(p2[i+3]));
                    }
                    s.finalizarEncomenda(e);

                case "Aceite":
                    Voluntario v = s.aceitaEncomendaV(p2[0]);
                    if (v != null) {
                        s.entregaEncomenda(v);
                    }
                    else {
                        Transportadora t = s.aceitaEncomendaT(p2[0]);
                        s.entregaEncomenda(t);
                    }
            }
        }
    }
}

