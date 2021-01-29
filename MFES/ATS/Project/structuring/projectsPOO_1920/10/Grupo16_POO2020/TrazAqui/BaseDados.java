package TrazAqui;


import java.io.*;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.stream.Collectors;

import static java.time.LocalTime.NOON;

/**
 * Classe que guarda, de forma organizada, todos os dados relevantes para o uso contínuo da aplicação TrazAqui Garantido a possibilidade de se complexificar.
 */
public class BaseDados implements Serializable {

    /**
     * Macro para comunicar a interação realizada por parte de um objeto instância da Classe Loja
     */
    public static final int LOJA    = 1;
    /**
     * Macro para comunicar a interação realizada por parte de um objeto instância da Classe User
     */
    public static final int USER    = 2;
    /**
     * Macro para comunicar a interação realizada por parte de um objeto instância da Classe Voluntario
     */
    public static final int VOL     = 3;
    /**
     * Macro para comunicar a interação realizada por parte de um objeto instância da Classe Empresa
     */
    public static final int EMPRESA = 4;
    /**
     * Macro para comunicar a interação realizada por parte de um objeto instãncia da Classe Encomenda
     */
    public static final int ENCOMENDA = 5;
    /**
     * Macro para estabelecer a velocidade média a que um Transportador viaja
     */
    public static final double VELOCIDADE = 50;

    /**
     * HashMap que associa uma String com o Código de um Utilizador a uma encomenda já aceite por uma Loja e pronta a ser entregue.
     */
    private HashMap<String,Set<Encomenda>> requests;
    /**
     * HashMap que associa o código de qualquer User do sistema ao seu objeto em concreto
     */
    private HashMap<String,Login> users;

    /**
     * TreeMap que associa código de uma Encomenda a uma Entry com a duração e a taxa de entrega da mesma.
     */
    private TreeMap<String, Map.Entry<Duration,Double>> duracaoEntrega;

    /**
     * Construtor vazio da Classe BaseDados
     */
    public BaseDados() {
        this.requests       = new HashMap<>();
        this.users          = new HashMap<>();
        this.duracaoEntrega = new TreeMap<>();
    }

    /**
     * Construtor parametrizado da Classe BaseDados
     * @param requests Mapeamento entre códigos Utilizador e Encomendas à espera de transporte
     * @param users Mapeamento entre códigos User e o objeto em concreto
     * @param de Mapeamento entre Código de Encomenda e um par (duração,taxa)
     */
    public BaseDados(HashMap<String,Set<Encomenda>> requests, HashMap<String,Login> users,TreeMap<String, Map.Entry<Duration,Double>> de){
        this.requests = new HashMap<>();
        this.users =  new HashMap<>();
        for(String cod : requests.keySet()){
            Set<Encomenda> aux = new TreeSet<>();
            for(Encomenda enc: requests.get(cod)) aux.add(enc.clone());
            this.requests.put(cod, aux);
        }
        for(String cod : users.keySet()){
            this.users.put(cod,users.get(cod).clone());
        }
        this.duracaoEntrega = de;
    }

    /**
     * Construtor de cópia da Classe BaseDados
     * @param b objeto a copiar
     */
    public BaseDados(BaseDados b){
        this.requests = new HashMap<>();
        this.users =  new HashMap<>();
        for(String cod : b.getRequests().keySet()){
            Set aux = new TreeSet<Encomenda>();
            for(Encomenda enc: requests.get(cod)) aux.add(enc.clone());
            this.requests.put(cod, aux);
        }
        for(String cod : b.getUsers().keySet()){
            this.users.put(cod,users.get(cod).clone());
        }
    }

    /**
     * Método de clone de uma BaseDados
     * @return retorna uma cópia do objeto sobre a qual o método é evocado.
     */
    @Override
    public BaseDados clone(){
        return new BaseDados(this);
    }

    /**
     * Verifica se dois objetos BaseDados são iguais
     * @param o Objeto a comparar
     * @return true caso sejam iguais, falso caso contrário.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BaseDados baseDados = (BaseDados) o;
        return Objects.equals(requests, baseDados.requests) &&
                Objects.equals(users, baseDados.users);
    }

    /**
     * Getter do HashMap de users da Classe BaseDados.
     * @return retorna o mapeamento entre o código de qualquer tipo de User e o seu respetivo objeto.
     */

    public HashMap<String,Login> getUsers(){
        HashMap<String,Login> aux = new HashMap<>();
        for(String cod : users.keySet()){
            aux.put(cod,users.get(cod));
        }
        return aux;
    }

    /**
     * Getter do HashMap de users da Classe BaseDados
     * @return cópia do HashMap requests
     */
    public HashMap<String,Set<Encomenda>> getRequests(){
        HashMap<String,Set<Encomenda>> res = new HashMap<>();
        for(String cod : requests.keySet()){
            Set aux = new TreeSet<Encomenda>();
            for(Encomenda enc: requests.get(cod)) aux.add(enc.clone());
            res.put(cod, aux);
        }
        return res;
    }

    /**
     * Método que retorna um objeto encomeda guardado no HashMap requests.
     * @param codEncomenda Código da Encomenda
     * @return objeto Encomenda encontrado ou null caso não exista
     */
    public Encomenda getEncomenda(String codEncomenda){
        Set<Encomenda> mergeSet = this.requests.values().stream().reduce(new TreeSet<Encomenda>(),(a,b) -> mergeSet(a,b));
        return mergeSet.stream().filter(enc -> enc.getCod().equals(codEncomenda)).findFirst().orElse(null);
    }

    /**
     * Método que faz get de um Objeto Encomenda que esteja na Queue de uma Loja
     * @param code Código da encomenda
     * @return objeto Encomenda caso esteja em alguma Queue
     */
    public Encomenda getEncomendaEmLoja(String code){
       return this.users.values().stream().filter( x -> x instanceof Loja).filter(x -> ((Loja) x).verificaPedidoExiste(code)).map(l -> ((Loja) l).verificaPedido(code)).findAny().orElse(null);
    }

    /**
     * Método que devolve o Nome da Empresa ou Voluntário que realizaram determinada Encomenda
     * @param enc Encomenda a ser verificada
     * @return String do Nome da Empresa ou Voluntário
     */
    public String getNameEntregadorByEncomenda(Encomenda enc){
        return this.users.values().stream().filter(u -> u instanceof Entregas).filter(e -> ((Entregas) e).verificarSeEntregou(enc)).map(Login::getUsername).findFirst().orElse(null);
    }
    /**
     * Este método retorna uma lista de todos os utilizadores registados de forma a apenas ser necessário percorrê-la para o Menu as mostrar no ecrâ.
     * @return Lista de objetos instância de qualquer uma das subclasses de Login
     */
    public List<Login> printUsers() {
        return new ArrayList<>(users.values().stream().map(Login::clone).collect(Collectors.toList()));
    }

    /**
     * Método que procura o preço por Km de uma Empresa.
     * @param code Código da Empresa
     * @return Preço por Km da Empresa
     */
    public double getPrecoKMbycode(String code){
        return ((Empresa) this.users.get(code)).getPrecoKm();
    }

    /**
     * Método que retorna todos os objetos Loja registados
     * @return List de objetos Loja
     */
    public List<Loja> printLojas(){
        return users.values().stream().filter(x -> x instanceof Loja).map(x -> ((Loja) x.clone())).collect(Collectors.toList());
    }

    /**
     * Este método recebe um objeto cuja classe deverá ser subclasse de Login.
     * @param log objeto a registar na BaseDados
     */
    public void regista(Login log){
        users.put(log.getCodUser(),log);
    }

    /**
     * Método que verifica se um User já está registado.
     * @param code código do User
     * @param password password do User
     * @return retorna true caso esteja registado e falso caso contrário
     */
    public boolean validaAcesso(String code, String password)  {

        boolean res = false;
        if(users.containsKey(code)){
            if(users.get(code).getPassword().equals(password)) res = true;
        }
        return res;
    }

    /**
     * Método que verifica se uma usuário já está registado.
     * @param code Código do User a validar
     * @return retorna true caso esteja rejistado e falso caso contrário
     */
    public boolean validaAcesso(String code){
        return users.containsKey(code);
    }

    /**
     * Método que Insere um pedido de Encomenda por parte de um Utilizador a uma loja. Coloca um objeto Encomenda associado a uma key String,
     *  representativa do código do objeto Utilizador que a requeriu, no HashMap requests.
     * @param cod Código do Utilizador
     * @param enc Encomenda aceite pela loja
     */
    public void aceitaPedido(String cod,Encomenda enc){
        requests.putIfAbsent(cod,new TreeSet<Encomenda>());
        requests.get(cod).add(enc);
    }

    /**
     * Método que insere um pedido de uma encomenda por parte de um Utilizador a uma Loja. Insere um objeto Encomenda na PriorityQueue de encomendas
     * da Loja.
     * @param enc Encomenda a ser aceite pela Loja
     */
    public void userRequestLoja(Encomenda enc){
        Loja l =(Loja) this.users.get(enc.getLoja());
        l.addPedido(enc);
    }

    /**
     * Método que atualiza a quantidade de pessoas em fila de espera para serem atentidas por uma Loja. No contexto específico do código, a variável queue toma um novo valor.
     * @param fila novo valor atualizado de pessoas na fila
     * @param codLoja código da Loja
     * @return antigo número de pessoas na fila (antes da atualização)
     */
    public int atualizaFilaLoja(int fila,String codLoja){
        Loja l= (Loja) this.users.get(codLoja);
        int last = l.getQueue();
        l.setQueue(fila);
        this.users.put(codLoja,l.clone());
        return last;
    }


    /**
     *Método que regista a encomenda feita por um Utilizador a uma loja. Guarda essa encomenda no HashMap requests de um objeto BaseDados associado ao códgio do
     * Utilizador que a encomendou (key). Representa a passagem do estado de preparação para o estado de "pronta a ser entregue" de uma Encomenda. A encomenda aceite
     * é removida da variável PriorityQueue pedidos do objeto da classe Loja que a aceitou.
     * @param codLoja Código da Loja
     * @return Clone Encomeda pronta a ser entregue
     */
    public Encomenda lojaRegistaEncomenda(String codLoja){
        Loja l = (Loja) this.users.get(codLoja);
        Encomenda e = l.removePedido();
        aceitaPedido(e.getUser(),e);
        return e;
    }

    /**
     * Método Utilizado para uma loja registar uma Encomenda ignorando a ordem da Fila de Espera.
     * @param enc Encomenda a registar
     */
    public void lojaRegistaEncomendaUrgente(Encomenda enc){
        Loja l = (Loja) this.users.get(enc.getLoja());
        l.removePedido(enc);
        aceitaPedido(enc.getUser(),enc.clone());
    }

    /**
     * Método que representa a aceitação, por parte de um voluntário, do serviço de entrega de determinada Encomenda. Remove o objeto Encomenda do HashMap
     * requests variável de instância da BaseDados sob a qual é evocado e acrescenta uma cópia desse objeto Encomenda à variável de instância
     * ArrayList historico, tanto do objeto Utilizador que a solicitou como do Objeto Voluntário que, abstratamente, a entregou.
     * @param e Encomenda a aceitar pelo Voluntário
     * @param vol código do Voluntário
     */
    public void volAcceptEncomenda(Encomenda e, String vol){
        for(Set<Encomenda> set : this.requests.values()){
            if(set.contains(e)){
                set.remove(e);
                break;
            }
        }
        ((Utilizador)this.users.get(e.getUser())).addEncomenda(e);
        ((Voluntario)this.users.get(vol)).addEncomenda(e);
        insertDuracaoEntrega(e,vol);
    }

    /**
     * Método que representa a aceitação, por parte de uma Empresa, do serviço de entrega de determinada Encomenda. O Utilizador do sistema
     * tem a liberdade de aceitar ou não tal serviço de entrega, portanto o método apenas adiciona o objeto Encomenda em questão ao "HashMap entregas"
     * variável de instância do objeto Utilizador, para que possa haver uma decisão posterior relativa à Entrega.
     * @param e Encomenda cujo envio foi aceite pela Empresa
     * @param emp Código da Empresa
     */
    public void empAcceptEncomenda(Encomenda e, String emp){
        ((Utilizador)this.users.get(e.getUser())).addEntrega(e,emp);
    }

    /**
     * Método que permite que um utilizador aceite o serviço de Entrega de uma Encomenda por deterimnada Empresa. Remove o objeto Encomenda do HashMap requests do objeto instância de BaseDados.
     * Remove objeto Encomenda e a String correspondente ao código da Empresa em questão, do "HashMap entregas" variável de instância do objeto Utilizador.
     * Adiciona uma cópia do objeto Encomenda à variável de instância "private ArrayList historico" tanto do objeto Utilizador como do objeto Empresa.
     * @param e Encomenda a ser aceite
     * @param ent Código da Empresa
     */
    public void userAcceptEntrega(Encomenda e, String ent){
        for(Set<Encomenda> set : this.requests.values()){
            if(set.contains(e)){
                set.remove(e);
                break;
            }
        }
        ((Utilizador)this.users.get(e.getUser())).addEncomenda(e);
        ((Empresa)this.users.get(ent)).addEncomenda(e);
        ((Utilizador)this.users.get(e.getUser())).removeEntrega(ent);
        insertDuracaoEntrega(e,ent);
    }

    /**
     * Este método gera valores do tipo float pseudo-aleatórios consoante a fase do dia em que é chamado. Deverá fazer uma estimativa baseada nos horários de mais
     * movimento do nosso sistema de entregas.
     * @return float que representa o coeficiente de entropia do distema
     */
    public float semiEntropiaTemporal(){
        Random r = new Random();
        if(LocalTime.now().compareTo(NOON)<0){
            return r.nextFloat()+1f;
        }
        if(LocalTime.now().compareTo(NOON) >=0 && LocalTime.now().compareTo(LocalTime.parse("14:00"))<=0){
            return r.nextFloat()+0.2f;
        }
        if(LocalTime.now().compareTo(LocalTime.parse("19:30")) >=0 && LocalTime.now().compareTo(LocalTime.parse("22:00"))<=0){
            return r.nextFloat()+0.2f;
        }
        return r.nextFloat()+0.6f;

    }

    /**
     * Método que insere o mapeamento entre o código de uma Encomenda e o par (duração de uma entrega,taxa associada) (0 no caso de um Voluntário) no HashMap duracaoEntrega.
     * @param e Encomenda a registar
     * @param codeTransport Código da Empresa Transportadora
     */
    public void insertDuracaoEntrega(Encomenda e,String codeTransport){

        Duration dur = calculaDuracao(e,codeTransport);
        double pesoCoef = e.getPeso()/1000;

        if(this.users.get(codeTransport) instanceof Voluntario){
            this.duracaoEntrega.put(e.getCod(),new AbstractMap.SimpleEntry<Duration, Double>(dur, (double) 0));
        }else{
            double taxa = this.getPrecoKMbycode(codeTransport);
            Map.Entry<Duration,Double> put = new AbstractMap.SimpleEntry<>(dur,taxa +taxa*pesoCoef);
            this.duracaoEntrega.put(e.getCod(),put);
        }
    }

    /**
     * Método que Calcula a duração de uma Entrega. Soma as distância entre a Empresa e a Loja e da Loja ao Utilizador, divide essa distância pela velocidade
     * média do sistema multiplicada pelo coeficiente de entropia.
     * @param e Encomenda cuja distância se pretende calcular
     * @param codEntregas Código do Entregador
     * @return Duração
     */
    public Duration calculaDuracao(Encomenda e,String codEntregas){
        Ponto emp = this.users.get(codEntregas).getPonto();
        Ponto ploja = this.users.get(e.getLoja()).getPonto();
        Ponto puser =this.users.get(e.getUser()).getPonto();
        double distLojaUser = ploja.distancia(puser);
        double distEntLoja = emp.distancia(ploja);
        long t = (long) ((distEntLoja+distLojaUser)/VELOCIDADE*semiEntropiaTemporal());
        LocalDateTime fin = e.getTempo().plusHours(t);
        return Duration.between(e.getTempo(),fin);
    }

    /**
     * Método que retorna a Duração associada à entrega de uma encomenda
     * @param cod Código da Encomenda
     * @return Duração
     */
    public Duration getDuracaoEntrega(String cod){
        return this.duracaoEntrega.get(cod).getKey();
    }

    /**
     * Método que retorna o Preço associada à entrega de uma encomenda
     * @param cod Código da Encomenda
     * @return Preço
     */
    public double getPrecoEntrega(String cod){
        return this.duracaoEntrega.get(cod).getValue()*this.duracaoEntrega.get(cod).getKey().toHours();
    }

    /**
     * Este método é responsável por gerar um código sempre que um utilizador novo se pretende registar. Dependendo do tipo de Login (Utilizador, Empresa,Loja ou
     * Voluntário) o código deverá começar por uma lentra distinta ('u','e','l' e 'v' respetivamente) e deverá ser procedido de dois digitos pseudo-aleatórios,
     * a função é chamada recursivamente até gerar um código obrigatoriamente diferente de qualquer outro que já tenha sido registado.
     * @param macro Macro descritiva do tipo de Login
     * @return Código gerado
     */

    public String generateCode(int macro) {
        Random r = new Random();
        int rand = r.nextInt(99);

        String code;
        if(rand < 10){
            code = String.format("%0d",rand);
        }else{
            code = String.valueOf(rand);
        }

        String res;
        switch (macro) {
            case LOJA:
                res =  "l".concat(code);
                break;
            case USER:
                res = "u".concat(code);
                break;
            case VOL:
                res = "v".concat(code);
                break;
            case EMPRESA:
                res = "t".concat(code);
                break;
            case ENCOMENDA:
                res = "e".concat(code);
            default:
                res = null;
        }
        if(macro == 5){
            boolean flag = false;
            for(Login l: this.users.values()){
                if(l instanceof Entregas){
                    if(((Entregas) l).verificarSeEntregou(code)) flag = true;
                }
                if(l instanceof Loja){
                    if(((Loja) l).getPedidos().contains(code)) flag = true;
                }
            }
            for(Set<Encomenda> set : this.requests.values()){
                for(Encomenda e : set){
                    if(e.getCod().equals(code)) {
                        flag = true;
                        break;
                    }
                }
            }
            if(flag){
                return generateCode(5);
            }
            else{
                return code;
            }
        }
        else{
            if(!validaAcesso(res)){
                return res;
            }
            else{
                return generateCode(macro);
            }
        }
    }

    /**
     * Através de um Comparator criado dentro da própria função, definimos a maneira de verificar se determinado Utilizador
     * acedeu mais vezes ao sistema, ou seja se realizou mais encomendas do que outro. Através de uma stream sob os values do hashmap que guarda todos os
     * Users registados, filtramos aqueles que são utilizadores, ordenamos através do comparator criado, limitamos a um máximo de 10 e retornamos uma lista dos mesmos.
     * @return Lista de Utilizadores
     */

    public List<Utilizador> top10User(){
        Comparator<Utilizador> c = Comparator.comparingInt(Utilizador::getSize);
        return this.users.values().stream().filter(x -> x instanceof Utilizador).map(x -> (Utilizador) x).sorted(c).limit(10).collect(Collectors.toList());
    }

    /**
     * Método para calcular o total de Km percorridos por uma empresa transportadora. Primeiramente acedemos à lista de todas as encomendas
     * relizadas por determinada Empresa  (List historico), criamos uma stream dessa lista e mapeamos todos os elementos de forma a obter, para cada encomenda,
     * a distância entre a loja e o comprador e, por fim, somamos todas essas distâncias através de um reduce.
     * @param codEmpresa - Código da Empresa
     * @return retorna um double que corresponde ao total de Kilómetros percorridos.
     */
    public double totalKm (String codEmpresa){
        Empresa e = (Empresa) this.users.get(codEmpresa);
        ArrayList<Encomenda> historico =  e.getHistorico();
        return historico.stream().map(enc -> users.get(enc.getLoja()).getPonto().distancia(users.get(enc.getUser()).getPonto())).reduce((double) 0,(a, b)->a+b);
    }

    /**
     * Método genérico para fazer o Merge entre dois Set de qualquer tipo
     * @param a primeiro set
     * @param b segundo set
     * @param <T> tipo dos Sets
     * @return o Set final resultante da união
     */
    public static <T> Set<T> mergeSet(Set<T> a, Set<T> b)
    {

        Set<T> mergedSet = new HashSet<T>();

        mergedSet.addAll(a);
        mergedSet.addAll(b);

        return mergedSet;
    }


    /**
     * Método que filtra encomendas possíveis de transportar pelo raio de distância.
     * @param codEntregas Código de Entregador
     * @return Lista de Encomendas possíveis de entregar
     */
    public List<Encomenda> getEcomendasRaio(String codEntregas){
        Ponto emp = this.users.get(codEntregas).getPonto();
        double mp = ((Entregas)this.users.get(codEntregas)).getRange();
        Set<Encomenda> mergeSet = this.requests.values().stream().reduce(new TreeSet<Encomenda>(),(a,b) -> mergeSet(a,b));

        return mergeSet.stream().filter(enc -> this.users.get(enc.getUser()).getPonto().distancia(emp)
                                      <= mp && this.users.get(enc.getLoja()).getPonto().distancia(emp) <= mp).
                                        collect(Collectors.toList());
    }

    /**
     * Método que calcula o total de Km percorridos por uma empresa e multiplica esse valor pelo seu preço por Km (private float precoKm).
     * Primeiramente acedemos à lista de todas as encomendas  relizadas por determinada Empresa  (List historico), criamos uma stream
     * dessa lista, filtramos as encomendas realizadas dentro do período estabelecido, mapeamos todos os elementos de forma a obter a distância entre a loja e o comprador,
     * somamos todas essas distâncias através de um reduce e multiplicamos esse valor pela referida variável "precoKm"
     * @param codEmpresa Código da empresa
     * @param init Data inicial
     * @param max Data máxima
     * @return Total faturado no período estabelecido
     */
    public double totalFaturado(String codEmpresa, LocalDateTime init, LocalDateTime max){
        Empresa e = (Empresa) this.users.get(codEmpresa);
        ArrayList<Encomenda> historico =  new ArrayList<Encomenda>( e.getHistorico().stream().filter(enc -> (enc.getTempo().compareTo(init)>=0) ||
                                                                         (enc.getTempo().compareTo(init)<=0)).collect(Collectors.toList()));

        return historico.stream().map(enc -> this.duracaoEntrega.get(enc.getCod())).map(a-> a.getKey().toMinutes()*a.getValue()).reduce((double) 0,Double::sum);
    }

    /**
     * Este método permite que um Utilizador classifique um Voluntário ou uma Empresa após a entrega de determinada encomenda. Procura no HashMap de users do sistema
     * Objetos instância da classe Entregas, verifica se o objeto realizou determinada entrega.
     * @param enc Encomenda entregue
     * @param clss classificação a dar
     */
    public void classificaEntrega(Encomenda enc, float clss){
        for(Login l : this.users.values()){
            if(l instanceof Entregas){
                if(((Entregas) l).verificarSeEntregou(enc)){
                    ((Entregas) l).setClassificacao(clss);
                    break;
                }
            }
        }
    }

    /**
     * Método que cria um comparator através de uma expressão lambda que garante a semântica da comparação pretendida. Neste caso, o comparator tira partido da função
     * previamente definida 'totalKm(String cod)' que, quando aplicado sob um objeto instância da class Empresa, retorna o total de Km percorridos em entregas. Para este critério,
     * uma empresa deverá ser comparativamente superior a outra que tenha menos Km percorridos e igual caso tenham o mesmo número de Km. Obtemos este comportamento, aplicado
     * o referido comparator a uma stream previamente filtrada por objetos Empresa e limitamos o restultado de tal composição de operações da stream a 10 elementos.
     * @return Lista de no máximo 10 objetos Empresa
     */
    public List<Empresa> top10Empresa(){
        Comparator<Empresa> c = (Empresa e1,Empresa e2) -> {
            if(totalKm(e1.getCodUser()) > totalKm(e2.getCodUser())) return 1;
            else if (totalKm(e1.getCodUser()) < totalKm(e2.getCodUser())) return -1;
            else return 0;
        };

        return this.users.values().stream().filter(x -> x instanceof Empresa).map(x -> (Empresa) x).sorted(c).limit(10).collect(Collectors.toList());
    }

    /**
     * Método que permite a serialização de um objeto que seja instância da classe BaseDados. Por predefinição guarda o estado da base de dados sob a qual o método é chamado
     * para um ficheiro com o nome "BaseDadosGravada".
     * @throws IOException Exceção de IO
     */
    public void binaryWrite() throws IOException {
        try{
            FileOutputStream file =new FileOutputStream("BaseDadosGravada");
            ObjectOutputStream binOut = new ObjectOutputStream(file);
            binOut.writeObject(this);
            binOut.flush();
            binOut.close();
        }catch (IOException e){
            System.out.println(e.getMessage());
        }
    }

    /**
     * Método que permite a leitura de um ficheiro em binário, previamente gerado, que contém um objeto instância da classe BaseDados. Por predefinição, tal como o método 'binaryWrite', lê
     * de um ficheiro cujo nome é BaseDadosGravada. No contexto da aplicação, guarda em disco o estado do sistema.
     * @return Um objeto BaseDados
     * @throws IOException Exceção de IO
     * @throws ClassNotFoundException Exceção caso a classe não exista
     */
    public BaseDados binaryRead() throws IOException, ClassNotFoundException {
        try{
            BaseDados bd;
            FileInputStream file = new FileInputStream("/home/luisaraujo/floobits/share/luisaraujo/TrazAqui/BaseDadosGravada");
            ObjectInputStream binIn = new ObjectInputStream(file);
            bd = (BaseDados) binIn.readObject();
            binIn.close();
            return bd;
        }catch (IOException | ClassNotFoundException e){
            System.out.println(e.getMessage());
            return null;
        }
    }
}
