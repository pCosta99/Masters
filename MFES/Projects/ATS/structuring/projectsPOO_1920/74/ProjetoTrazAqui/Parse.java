import java.util.Scanner;
import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.ArrayList;
import java.util.TreeSet;
import java.io.IOException;
import java.lang.String;
import java.nio.charset.StandardCharsets;
import java.io.Serializable;

public class Parse implements Serializable{
    private String fileName;
    
    /**
     * Construtor por omissao da classe
     */
    public Parse(){
        this.fileName = "logs.txt";
        
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String fName     
     */
    public Parse(String fName){
        this.fileName = fName;
        
    }
    /**
     * Construtor de copia da classe
     * Aceita como paramtros um objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */
    public Parse(Parse p){
        this.fileName = p.getFileName();
        
    }
    /**
     * Devolve a String fileName da classe
     * @return String codigo
     */
    public String getFileName(){
        return this.fileName;
    }
    /**
     * Atualiza a String fileName da classe
     * @param fName novo fileName da classe
     */
    public void setFileName(String fName){
        this.fileName = fName;
    }
    /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o.getClass() != this.getClass())
            return false;
        Parse p = (Parse) o;
        return this.fileName == p.getFileName(); 
    }
    /**
     * Faz clone da classe
     * @return  clone da classe Parse
     */
    public Parse clone(){
        return new Parse(this);
    }
    /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        return "Ficheiro que vai ser lido: " + this.fileName;
    }
    /**
     * Metodo que le um ficheiro e o carrega num objecto , que implementa a interface ISistema
     * @param ISistema sist
     */
    public void lerFicheiro(ISistema sist) {
        List<String> lines = new ArrayList<>();
        try { 
            lines = Files.readAllLines(Paths.get(this.fileName), StandardCharsets.UTF_8); 
            boolean isData = !lines.contains("Preambulo /Legenda");
            for(String linha : lines){
                if(linha.equals("Dados de LOGS:"))
                    isData = true;
                if(isData)
                    processarLinha(linha,sist);

            }


        }catch(IOException exc){
            System.out.println(exc.getMessage());
        }
    }

    /*private void adicionaEntidade(Entidade e, Sistema sist){
        if(e instanceof Utilizador)
            sist.adicionaUtilizador((Utilizador) e);
        else if(e instanceof Loja)
            sist.adicionaLoja((Loja) e);
        else if(e instanceof Voluntario)
            sist.adicionaVoluntario((Voluntario) e);
        else if(e instanceof Encomenda)
            sist.adicionaEncomenda(e);
    }*/

    /**
     * Metodo que dado uma linha do ficheiro verifica que entidade representa e faz as respetivas inserçoes no objecto , que 
     * implementa a interface ISistema
     * @param String c , IStistema sist
     * @return Entidade correspondente
     */ 
	private void processarLinha(String c,ISistema sist){
        String [] elementos = c.split(":");
        switch(elementos[0]){
            case "Utilizador":
                Utilizador u = parseUtilizador(elementos[1]);
                try{
                    sist.adicionaEntidade(u);
                }catch(AddEntidadeRepetidaException e){
                    System.out.println(e.getMessage());
                }
                break;
            case "Encomenda":
                try{
                    Encomenda e = parseEncomenda(elementos[1]);
                    Entidade loj = sist.getEntidade(e.getCodLoja());
                    Coordenada loja = loj.getGps();
                    Entidade entity = sist.getEntidade(e.getCodUtilizador());
                    Utilizador util = (Utilizador) entity;
                    Coordenada utilizador = util.getGps();
                    e.setLocalizacaoLoja(loja);
                    e.setLocalizacaoUtilizador(utilizador);
                    String ret = sist.fazPedido(e,new ArrayList<>());
                    for(LinhaDeEncomenda l : e.getProdutos()){
                        Produto p = new Produto(l.getCodigo(), l.getDescricao(), l.getPreco());
                        sist.adicionaProdutoLojas(p);
                        
                    }
                }catch(EntidadeNaoExistenteException e){
                    System.out.println(e.getMessage());
                }catch (TransporteNaoExistenteException e) {
                    System.out.println(e.getMessage());
                }
                break;
            case "Transportadora":
                Transportadora t = parseTransportadora(elementos[1]);
                try{
                    sist.adicionaEntidade(t);
                }catch(AddEntidadeRepetidaException e){
                    System.out.println(e.getMessage());
                }
                break;
            case "Voluntario":
                Voluntario v = parseVoluntario(elementos[1]);
                try{
                    sist.adicionaEntidade(v);
                }catch(AddEntidadeRepetidaException e){
                    System.out.println(e.getMessage());
                }
                break;
            case "Loja":
                Loja l = parseLoja(elementos[1]);
                try{
                    sist.adicionaEntidade(l);
                }catch(AddEntidadeRepetidaException e){
                    System.out.println(e.getMessage());
                }
                break;
            case "Aceite":
                try{
                    sist.encomendaAceite(elementos[1]);
                }catch(EntidadeNaoExistenteException e){
                    System.out.println(e.getMessage());
                }catch(PedidoNaoExistenteException f){
                    System.out.println(f.getMessage());
                }
                break;
        }
        
    }
    /**
     * Metodo que utiliza a String passado como argumento para criar um novo Utilizador
     * @param String str
     * @return Utilizador criado
     */ 
    public Utilizador parseUtilizador(String str){
        String [] elem = str.split(",");
        Coordenada gps = new Coordenada(Double.parseDouble(elem[2]),Double.parseDouble(elem[3]));
        String email = elem[0] + "@mail.com";
        return new Utilizador(elem[1] , elem[0] , email , elem[0] , gps, new TreeSet<>());
    }
    /**
     * Metodo que utiliza a String passado como argumento para criar uma nova Transportadora
     * @param String str
     * @return Transportadora criada
     */ 
    public Transportadora parseTransportadora(String str){
        String [] elem = str.split(",");
        Coordenada gps = new Coordenada(Double.parseDouble(elem[2]),Double.parseDouble(elem[3]));
        String email = elem[0] + "@mail.com";
        return new Transportadora(elem[1],elem[0],gps,elem[4],Double.parseDouble(elem[5]),Double.parseDouble(elem[6]));
    }
    /**
     * Metodo que utiliza a String passado como argumento para criar um novo Voluntario
     * @param String str
     * @return Voluntario criado
     */ 
    public Voluntario parseVoluntario(String str){
        String [] elem = str.split(",");
        Coordenada gps = new Coordenada(Double.parseDouble(elem[2]),Double.parseDouble(elem[3]));
        return new Voluntario(elem[0],elem[1],gps,Double.parseDouble(elem[4]));
    }
    /**
     * Metodo que utiliza a String passado como argumento para criar uma nova Loja
     * @param String str
     * @return Loja criada
     */ 
    public Loja parseLoja(String str){
        String [] elem = str.split(",");
        Coordenada gps = new Coordenada(Double.parseDouble(elem[2]),Double.parseDouble(elem[3]));
        return new Loja(elem[0],elem[1],gps);
    }
    /**
     * Metodo que utiliza a String passado como argumento para criar uma nova Encomenda
     * @param String str
     * @return Encomenda criada
     */ 
    public Encomenda parseEncomenda(String str){
        String [] elem = str.split(",");
        Encomenda ret = new Encomenda(elem[0], elem[1], elem[2], Double.valueOf(elem[3]));
        for(int i = 4; i < elem.length; i += 4){
            LinhaDeEncomenda aux = new LinhaDeEncomenda(elem[i], elem[i+1], Double.valueOf(elem[i+2]), Double.valueOf(elem[i+3]));
            ret.adicionaLinhaDeEncomenda(aux);
        }
        return ret;
    }
}
