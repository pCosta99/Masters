package Model;

import java.io.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.*;

import Exceptions.InvalidInputException;
import Exceptions.NoEntityException;
import Utilities.Ponto;

/**
 *   @class Leitura define a leitura de um ficheiro log para uma @class TrazAqui.
 */
public class Leitura{
    private static final String p_format = "(.+):([^<>]+)";
    private static final String p_utilizador = "(u\\d+),([^,]+),(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)";
    private static final String p_voluntario = "(v\\d+),([^,]+),(-?\\d+\\.\\d+),(-?\\d+\\.\\d+),(\\d+\\.\\d+)";
    private static final String p_transportadora = "(t\\d+),([^,]+),(-?\\d+\\.\\d+),(-?\\d+\\.\\d+),(\\d{9}),(\\d+\\.\\d+),(\\d+\\.\\d+)";
    private static final String p_loja = "(l\\d+),([^,]+),(-?\\d+\\.\\d+),(-?\\d+\\.\\d+)";
    private static final String p_encomenda = "(e\\d+),(u\\d+),(l\\d+),(\\d+\\.\\d+),(.*)"; 
    private static final String p_1enc = "(p\\d+),([^,]+),(\\d+\\.\\d+),(\\d+\\.\\d+)(,(.*))?";
    private static final String p_aceite = "(e\\d+)";

    static final String stdpath = "Model/log.txt";

    private static final Pattern _format = Pattern.compile(p_format);
    private static final Pattern _utilizador = Pattern.compile(p_utilizador);
    private static final Pattern _voluntario = Pattern.compile(p_voluntario);
    private static final Pattern _transportadora = Pattern.compile(p_transportadora);
    private static final Pattern _loja = Pattern.compile(p_loja);
    private static final Pattern _encomenda = Pattern.compile(p_encomenda);
    private static final Pattern _enc = Pattern.compile(p_1enc);
    private static final Pattern _aceite = Pattern.compile(p_aceite);

    private static final String email = "@trazaqui.com";
    private static final String password = "123";
    /**
     * Lê um utilizador.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     */
    private static void readUtilizador(String entry, IGestao g) throws InvalidInputException{
        Matcher m = _utilizador.matcher(entry);
        if(m.find()){
            g.registaUtilizador(
                    m.group(1), 
                    m.group(2), 
                    new Ponto(Double.parseDouble(m.group(3)), Double.parseDouble(m.group(4)))
                    );
            g.registaConta(m.group(1), m.group(1) + email, password);
        }
    }

    /**
     * Lê um voluntário.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     */
    private static void readVoluntario(String entry, IGestao g) throws InvalidInputException{
        Matcher m = _voluntario.matcher(entry);
        if(m.find()){
            g.registaVoluntario(
                    m.group(1), 
                    m.group(2),
                    new Ponto(Double.parseDouble(m.group(3)), Double.parseDouble(m.group(4))),
                    Double.parseDouble(m.group(5))
            );
            g.registaConta(m.group(1), m.group(1) + email, password);
        }
    }
    
    /**
     * Lê uma transportadora.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     */
    private static void readTransportadora(String entry, IGestao g) throws InvalidInputException{
        Matcher m = _transportadora.matcher(entry);
        if(m.find()){
            g.registaTransportadora(
                m.group(1), 
                m.group(2), 
                m.group(5),
                new Ponto(Double.parseDouble(m.group(3)), Double.parseDouble(m.group(4))),
                Double.parseDouble(m.group(6)),
                new BigDecimal(m.group(7))
            );
                g.registaConta(m.group(1), m.group(1) + email, password);
        }
    }
    
    /**
     * Lê uma loja.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     */
    private static void readLoja(String entry, IGestao g) throws InvalidInputException{
        Matcher m = _loja.matcher(entry);
        if(m.find()){
            g.registaLoja(
                    m.group(1), 
                    m.group(2),
                    new Ponto(Double.parseDouble(m.group(3)), Double.parseDouble(m.group(4)))
            );
            g.registaConta(m.group(1), m.group(1) + email, password);
        }
    }
    
    /**
     * Lê uma encomenda.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     * @throws NoEntityException se não existir um dada entidade.
     */
    private static void readNovaEncomenda(String entry, IGestao g) throws InvalidInputException, NoEntityException{
        Matcher m = _encomenda.matcher(entry);
        if(m.find()){
            Set<LinhaEncomenda> e = new HashSet<>();
            Matcher linEn = _enc.matcher(m.group(5));
            //for(; linEn.find(); linEn = _enc.matcher(linEn.group(6)))
            while (linEn.find()) {
                e.add(
                    new LinhaEncomenda(
                        linEn.group(1), 
                        linEn.group(2), 
                        (int) Math.round(Double.parseDouble(linEn.group(3))),
                        new BigDecimal(linEn.group(4)) 
                    )
                );
                if (linEn.group(5) == null) 
                    break; 
                linEn = _enc.matcher(linEn.group(6));
            }
            g.registaNovaEncomenda(
                m.group(1), 
                m.group(2), 
                m.group(3), 
                Double.parseDouble(m.group(4)), 
                e, 
                LocalDateTime.now(),
                null,
                false
            );
            g.incNEncUtilizador(m.group(2));
        }
    }

    /**
     * Lê uma encomenda aceite (pronta para a Gestao).
     * @throws InvalidInputException se algum dos inputs foi inválido.
     * @throws NoEntityException se não existir um dada entidade.
     */
    private static void readEncomendaAceite(String entry, IGestao g) throws InvalidInputException, NoEntityException{
        Matcher m = _aceite.matcher(entry);
        if(m.find())
            g.lojaTemEncomendaPronta(m.group(1));
    }


    /**
     * Lê um um ficheiro log num certo caminho.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     * @throws IOException quando ocorrem erros na leitura.
     * @throws NoEntityException se não existir um dada entidade.
     */
    public static IGestao readFromFilePath(String path) throws IOException, InvalidInputException, NoEntityException {
        IGestao g = new Gestao();
        DataInputStream in = new DataInputStream(new FileInputStream(path));
        BufferedReader br = new BufferedReader(new InputStreamReader(in));
        String strLine;
        while ((strLine = br.readLine()) != null) {
            Matcher m = _format.matcher(strLine);
                if(m.find()){
                    switch(m.group(1)){
                        case "Utilizador":     readUtilizador(m.group(2), g);           break;
                        case "Voluntario":     readVoluntario(m.group(2), g);           break;
                        case "Transportadora": readTransportadora(m.group(2), g);       break;
                        case "Loja":           readLoja(m.group(2), g);                 break;
                        case "Encomenda":      readNovaEncomenda(m.group(2), g);        break;
                        case "Aceite":         readEncomendaAceite(m.group(2), g);      break;
                        default:                                                        break;
                    }
                 }
             }
             br.close();
        return g;
    }


    /**
     * Lê um um ficheiro log padrão.
     * @throws InvalidInputException se algum dos inputs foi inválido.
     * @throws IOException quando ocorrem erros na leitura.
     * @throws NoEntityException se não existir um dada entidade.
     */
    public static IGestao readStandardPath() throws IOException, InvalidInputException, NoEntityException {
        return readFromFilePath(stdpath);
    }
}