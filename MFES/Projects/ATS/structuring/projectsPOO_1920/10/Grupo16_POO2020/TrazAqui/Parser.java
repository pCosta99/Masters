package TrazAqui;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Classe que garante a leitura e tratamento da informação contida em ficheiros de logs.
 */
public class Parser implements Serializable{
    /**
     * BaseDados partilhada com o Menu
     */
    BaseDados bd;
    /**
     * Linhas lidas dos ficheiros de logs
     */
    List<String> linhas;

    /**
     *Construtor parametrizado
     * @param base BaseDados partilhada com o Menu
     */
    public Parser(BaseDados base) {

        linhas = new ArrayList<>();
        this.bd = base;
    }

    /**
     * Método que lê linhas de um ficheiro e guarda-as na List linhas.
     * @param fileName nome do ficheiro a ler.
     */
    public void lerFicheiro(String fileName) {
        try { this.linhas = Files.readAllLines(Paths.get(fileName), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
    }

    /**
     * Método que recebe uma linha do ficheiro de logs correspondente à descrição de um Utilizador. Faz split dos campos por vírgulas e cria um novo
     * objeto instância da Classe Utilizador com os campos lidos e convertidos para os tipos requeridos. A password por predifinição é "1234".
     * @param input Linha com campos de um Utilizador
     * @return objeto Utilizador com os campos lidos.
     */
    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Ponto p = new Ponto(gpsx,gpsy);
        return new Utilizador(codUtilizador,nome,"1234",p);
    }

    /**
     * Método que recebe uma linha do ficheiro de logs correspondente à descrição de uma Loja. Faz split dos campos por vírgulas e cria um novo
     * objeto instância da Classe Loja com os campos lidos e convertidos para os tipos requeridos. A password por predifinição é "1234".
     * @param input Linha com campos de uma Loja
     * @return objeto Loja com os campos lidos.
     */
    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gps1 = Double.parseDouble(campos[2]);
        double gps2 = Double.parseDouble(campos[3]);
        return new Loja(codLoja,nomeLoja,"1234",new Ponto(gps1,gps2),0);
    }
    /**
     * Método que recebe uma linha do ficheiro de logs correspondente à descrição de um Voluntario. Faz split dos campos por vírgulas e cria um novo
     * objeto instância da Classe Voluntario com os campos lidos e convertidos para os tipos requeridos. A password por predifinição é "1234".
     * @param input Linha com campos de um Voluntario
     * @return objeto Voluntario com os campos lidos.
     */
    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVol = campos[0];
        String nomeVol = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        float range = Float.parseFloat(campos[4]);
        return new Voluntario(codVol,nomeVol,"1234",new Ponto(gpsx,gpsy),range,0,0,false,false, new ArrayList<>());
    }
    /**
     * Método que recebe uma linha do ficheiro de logs correspondente à descrição de uma Empresa. Faz split dos campos por vírgulas e cria um novo
     * objeto instância da Classe Empresa com os campos lidos e convertidos para os tipos requeridos. A password por predifinição é "1234".
     * @param input Linha com campos de uma Empresa
     * @return objeto Empresa com os campos lidos.
     */
    public Empresa parseEmpresa(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double nif = Double.parseDouble(campos[4]);
        float range = Float.parseFloat(campos[5]);
        float precKm = Float.parseFloat(campos[6]);
        return new Empresa(cod,nome,"1234",new Ponto(gpsx,gpsy),range,0,0,false,precKm,false, new ArrayList<>(),nif);
    }

    public List<LinhaEncomenda> parseLinhaEncomenda(String input){
        String[] campos = input.split(",");
        List<LinhaEncomenda> res = new ArrayList<>();
        for(int i = 0; i< campos.length ; i+=4){
            String cod = campos[i];
            String descri = campos[i+1];
            double quantidade = Double.parseDouble(campos[i+2]);
            double valUnitario = Double.parseDouble(campos[i+3]);
            LinhaEncomenda enc = new LinhaEncomenda(cod,descri,0,quantidade,valUnitario);
            enc.setPreco(enc.calculaValorLinhaEnc());
            res.add(enc);
        }
        return res;
    }

    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",",5);
        String codEnc = campos[0];
        String codUtiliz = campos[1];
        String codLoja = campos[2];
        float peso = Float.parseFloat(campos[3]);
        return new Encomenda(codEnc,codLoja,codUtiliz,peso,parseLinhaEncomenda(campos[4]),LocalDateTime.now());
    }

    /**
     * Método para gerar um valor pseudo-aleatório de 1 a 6. Simula o lançamento de um dado.
     * @return inteiro de 1 a 6.
     */
    public int dices(){
        Random r = new Random();
        return r.nextInt(6-1)+1;
    }


    public boolean parse(String fileName){
        this.lerFicheiro(fileName);
        String[] linhaPartida;
        for(String line : this.linhas){
            linhaPartida = line.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    bd.regista(u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    bd.regista(l);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    bd.regista(v);
                    break;
                case "Transportadora":
                    Empresa t = parseEmpresa(linhaPartida[1]);
                    bd.regista(t);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    bd.userRequestLoja(e);
                    break;
                case "Aceite":
                    String code= linhaPartida[1];
                    Encomenda enc = this.bd.getEncomendaEmLoja(code);
                    this.bd.lojaRegistaEncomendaUrgente(enc);
                    if(dices() % 2 == 0){
                        Voluntario vol =(Voluntario) this.bd.getUsers().values().stream().filter(x -> x instanceof Voluntario).findAny().orElse(null);
                        this.bd.volAcceptEncomenda(enc,vol.getCodUser());
                    }else{
                        Empresa emp = (Empresa) this.bd.getUsers().values().stream().filter(x -> x instanceof Empresa).findAny().orElse(null);
                        this.bd.empAcceptEncomenda(enc,emp.getCodUser());
                        this.bd.userAcceptEntrega(enc,emp.getCodUser());
                    }
                    break;
                default:
                    System.out.println("Tentativa de Parse Falhada!");
                    return false;
                }
        }
        this.linhas.clear();
        return true;
    }

    public List<LinhaEncomenda> parseInventario(String fileName){
        this.lerFicheiro(fileName);
        return parseLinhaEncomenda(linhas.get(0));
    }



}
