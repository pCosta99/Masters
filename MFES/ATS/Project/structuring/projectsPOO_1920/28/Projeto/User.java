import java.io.Serializable;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Esta classe abstrata guarda a informaçao comum ao Utilizador, a Loja, aos Voluntarios e as Transportadoras
 */

public abstract class User implements Serializable {
    private String id; //codido de User
    private String nome; // nome do User
    private String password; // password de acesso ao User
    private GPS coordenadas; // as coordenadas do User
    private RegistoEncomendaLista registos; // os Lista dos RegistoEncomenda do user 

    public User() {
        this.id = "";
        this.nome = "";
        this.password = "";
        this.coordenadas = new GPS ();
        this.registos = new RegistoEncomendaLista();
    }

    public User(String id, String nome ,String password,GPS coordenadas) {
        this.id = id;
        this.nome = nome;
        this.password = password;
        this.coordenadas = coordenadas.clone();
        this.registos = new RegistoEncomendaLista();
    }

    public User(User u){
        this.id = u.getId();
        this.nome = u.getNome();
        this.password = u.getPassword();
        this.coordenadas = u.getCoordenadas();
        this.registos = new RegistoEncomendaLista();
    }

    public String getId() {
        return this.id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getNome (){
        return this.nome;
    }

    public void setNome (String nome) {
        this.nome = nome;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public GPS getCoordenadas() {
        return this.coordenadas.clone();
    }

    public void setCoordenadas(GPS coordenadas) {
        this.coordenadas = coordenadas.clone();
    }

    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof User)) {
            return false;
        }
        User user = (User) o;
        return this.id.equals(user.getId()) && this.coordenadas.equals(user.getCoordenadas());
    }

    public String toString() {
        
        StringBuilder sb = new StringBuilder();
        sb.append("\nRegisto Encomendas:").append(registos.toString());
        return sb.toString();
    }

    public abstract User clone ();
    
    public void addToRegisto (String entregador, Encomenda encomenda, int classificacao, LocalDate dataDaEntrega, double kmsPercorridos, double taxaDeEntrega, double tempoDeEntrega){
        RegistoEncomenda nova = new RegistoEncomenda(entregador, encomenda, classificacao, dataDaEntrega, kmsPercorridos,taxaDeEntrega,tempoDeEntrega);
        registos.add(nova);
    }


    /**
     * Usado para imprimir o User para um ficheiro CSV
     */
    public String paraCSV (){

        StringBuilder sb = new StringBuilder();

        sb.append(this.id).append(",");
        sb.append(this.nome).append(",");
        sb.append(this.getCoordenadas().getLatitude()).append(",");
        sb.append(this.getCoordenadas().getLongitude());
        return sb.toString();
    }

    /**
     * metodo que recebe o codigo do entregador uma data inicial e uma data final 
     * e devolve uma lista de registosEncomenda feitos pelo entregador nesse periodo de tempo ligados ao User
     */
    public List<RegistoEncomenda> verRegistos (String codEntregador, LocalDate inicio, LocalDate fim){
        return registos.verRegistos(codEntregador, inicio, fim);
    }

    /**
     * metodo que devolve a lista de RegistosEncomenda ligadas ao User
     */
    public List<RegistoEncomenda> verRegistosGeral (){
        return registos.verRegistosGeral();
    }

    /**
     * metodo que devolve a lista de RegistosEncomenda nao Classificados ligadas ao User
     * pode receber uma ListaVaziaExceptio neste caso envia a exception para a funçao que a chamou
     */
    public List<RegistoEncomenda> naoClassificado () throws ListaVaziaException {
        return registos.naoClassificado();
    }

    /**
     * metodo que recebe um codigo de encomenda e uma classificaçao e vai aos RegistosEncomendaLista 
     * e classifica 
     */
    public void classificarEntrega (String codEnc, int classificacao){
        registos.classificaEntrega(codEnc,classificacao);
    }

    /**
     * metodo que devolve  as classificaçoes no RegistoEncomendaLista
     */
    public String verClassificacoes (){
        return registos.verClassificacoes();
    }

    /**
     * metodo que devolve a classicaçao media no RegistoEncomendaLista
     */
    public String verClassificacoesMedia (){
        return registos.verClassificacaoMedia();
    }


    /**
     * enum que simula o estado do covid que influencia o tempo de espera entre atender dois clientes
     */
    public enum EstadoCovid {

        Emergencia(5) , Precaucao(3) , Normalidade (1); // 5 / 3 / 1 minutos de media de espera entre atender 2 clientes , abranda tempo de recolher entrega
    
        private int tempoEspera;
    
        EstadoCovid (int t){
            this.tempoEspera = t;
        }
    
        /**
         * metodo que permite ver o tempo de espera  
         */
        public int getTempo(){
            return this.tempoEspera;
        }
    }

    /**
     * enum que simula a metereologia que influencia o tempo de viagem
     */
    public enum Meteorologia{

        Sol(0) , Chuva(1) , Nevoeiro(2) , Neve(4); // Neve , Nevoeiro , Chuva abrandam  uns minutos no tempo de chegar a loja
    
        
        private int tempoChegada;


        Meteorologia (int t){
            this.tempoChegada = t;
        }
        
        /**
         * metodo que permite ver a meteriologia
         */
        public int getMeteorologia(){
            return this.tempoChegada;
        }



    }

    /**
     * metodo que recebe um map com as contas, o numero n de elementos no Top, o comparador c e o nome da classe dos elementos do top
     * e devlove o Top de numero de Kms percorridos n de uma classe apartir do comparador c 
     */
    public String verTopNKm ( Map<String,User> contas ,  int n , Comparator <Transportador>  c , String classe ){

        Function <Map<String,User> , List <Transportador>> top = map -> map.values()
                                                                  .stream()
                                                                  .filter( user -> user.getClass().getName().equals(classe))
                                                                  .map (user -> (Transportador) user)
                                                                  .sorted(c)
                                                                  .collect(Collectors.toList());
        List<Transportador> lista = top.apply(contas);
        int conta = 0;
        Iterator<Transportador> i = lista.iterator();
        StringBuilder sb = new StringBuilder();

        while ( conta < n && i.hasNext()){

            Transportador t = (Transportador) i.next();
            sb.append(t.toString()).append("\n");
            conta++;

        }

        return sb.toString();
    }

    /**
     * metodo que recebe um map com as contas, o numero n de elementos no Top, o comparador c e o nome da classe dos elementos do top
     * e devlove o Top n de uma classe apartir do comparador c 
     */
    public String verTopN ( Map<String,User> contas ,  int n , Comparator <User>  c , String classe ){

        Function <Map<String,User> , List <User>> top = map -> map.values()
                                                                  .stream()
                                                                  .sorted(c)
                                                                  .collect(Collectors.toList());
        

                                               
                                     
    
        List <User> lista = top.apply(contas);
        
        if (classe != "User"){
            lista = lista.stream().sequential().filter( user -> user.getClass().getName().equals(classe)).collect(Collectors.toList());
        }

        int conta = 0;
        Iterator<User> i = lista.iterator();
        StringBuilder sb = new StringBuilder();

        while ( conta < n && i.hasNext()){

            User u = (User) i.next();
            sb.append(u.toString()).append("\n");
            conta++;

        }

        return sb.toString();

    }
}
