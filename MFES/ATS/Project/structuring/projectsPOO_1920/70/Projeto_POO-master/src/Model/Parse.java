package Model;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.*;

public class Parse {

    /** Method that will parse the information read from the given file and add it to the TrazAqui app.
     *
     * @param s - TrazAqui object which information will be updated.
     * @param filename - Name of the file from where we'll get the info to be added to s (TrazAqui).
     */
    public void parse(TrazAqui s, String filename){
        List<String> linhas = lerFicheiro("input_files/" + filename); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    User u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    if(User.validate(u)){
                        s.addUser(u);
                        s.getLoginInfo(1).addRegister(u.getCodUser()+ "@email.com",u.getCodUser());
                    }
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    if(Loja.validate(l)){
                        s.addLoja(l);
                        s.getLoginInfo(2).addRegister(l.getStoreCode()+ "@email.com",l.getStoreCode());
                    }
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    if(Voluntario.validate(v)){
                        s.addVoluntario(v);
                        s.getLoginInfo(3).addRegister(v.getCodVoluntario()+ "@email.com",v.getCodVoluntario());
                    }
                    break;
                case "Transportadora":
                    EmpresaTransportadora et = parseTransportadora(linhaPartida[1]);
                    if(EmpresaTransportadora.validate(et)){
                        s.addEmpresaTransportadora(et);
                        s.getLoginInfo(4).addRegister(et.getCodTrans()+ "@email.com",et.getCodTrans());
                    }
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    if(Encomenda.validate(e)){
                        s.addEncomenda(e);
                    }
                    for(LinhaEncomenda le : e.getLista()){
                        s.getLojas().get(e.getCodigoLoja()).addProduct(new Produto(le.getReferencia(), le.getDescricao(), le.getPreco()));
                    }
                    break;
                case "Aceite":
                    String aceite = parseAceites(linhaPartida[1]);
                    s.addAceite(aceite);
                    break;
                default:
                    break;
            }

        }
    }

    /** Creates a new User with the given info.
     *
     * @param input - Line read form file containing relevant information.
     * @return User with the given info.
     */
    private User parseUtilizador(String input){
        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        Location loc = new Location(Double.parseDouble(campos[2]), Double.parseDouble(campos[3]));

        return new User(codUtilizador,nome,new ArrayList<>(),loc);
    }

    /** Creates a new Store with the given info.
     *
     * @param input - Line read form file containing relevant information.
     * @return Store with the given info.
     */
    private Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        // ele ao fazer parse das comprar de um utilizador numa loja devia por no catalogo da loja
        return new Loja(codLoja, nomeLoja, 0, new Location(),new ArrayList<>(), new LinkedList<>());
    }

    /** Creates a new Volunteer with the given info.
     *
     * @param input - Line read form file containing relevant information.
     * @return Volunteer with the given info.
     */
    private Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        Location loc = new Location(Double.parseDouble(campos[2]), Double.parseDouble(campos[3]));
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario,nomeVoluntario,loc, 0.0, true, raio, new ArrayList<>());
    }

    /** Creates a new Delivery Company with the given info.
     *
     * @param input - Line read form file containing relevant information.
     * @return Devilvery Company with the given info.
     */
    private EmpresaTransportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String code = campos[0];
        String nome = campos[1];
        Location loc = new Location(Double.parseDouble(campos[2]), Double.parseDouble(campos[3]));
        int nif = Integer.parseInt(campos[4]);
        double raio = Double.parseDouble(campos[5]);
        double taxaporkm = Double.parseDouble(campos[6]);
        return new EmpresaTransportadora(code,nome, nif,true,0.0,raio,taxaporkm,0.1,loc, 0,new ArrayList<>());
    }

    /** Creates a new Package with the given info.
     *
     * @param input - Line read form file containing relevant information.
     * @return Package with the given info.
     */
    private Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codigo = campos[0];
        String codUser = campos[1];
        String codLoja = campos[2];
        String codTrans = "";
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> lista = new ArrayList<>();
        int size = campos.length;
        int i;
        for(i = 4; i < size; i = i +4){
            lista.add(parseLinhaEncomenda(campos[i],campos[i+1],Double.parseDouble(campos[i+2]), Double.parseDouble(campos[i+3])));
        }

        return new Encomenda(codigo,codUser,codLoja,codTrans,peso,false,false,false, LocalDate.now(),0.0,lista);
    }

    /** Passes the read info from the Package to a line of that Package.
     *
     * @param code - Product code
     * @param descricao - Product description
     * @param quantidade - Product quantity
     * @param preco - Package price
     * @return - LinhaEncomenda with the given info
     */
    private LinhaEncomenda parseLinhaEncomenda(String code, String descricao, double quantidade, double preco){
        return new LinhaEncomenda(code,descricao,preco,quantidade);
    }

    /** This method only returns the String it receives.
     * In order to make the code more dynamic and easily adjustable to wanted changes, we'll keep the method to ease future changes.
     *
     * @param input - String received with the code of an accepted package.
     * @return The String it had received.
     */
    private String parseAceites(String input){
        return input;
    }

    /** Reads the given File and splits it's content.
     *
     * @param nomeFich - Name of the file that will be read.
     * @return List of Strings where every element is a line read from the file.
     */
    private List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        }
        catch(IOException exc) {
            System.out.println("Impossível abrir ficheiro "+nomeFich);
        }
        return lines;
    }
}