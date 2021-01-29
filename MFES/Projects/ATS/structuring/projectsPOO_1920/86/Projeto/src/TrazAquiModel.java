import java.io.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;


/**
 * Model do programa. Contem métodos que retornam para o Controller os valores necessários ou faz os calculos necessários
 * que são pedidos pelo controller. Contém tambem todas as outras classes usadas no projeto e faz a gestao das interaçoes
 * entre elas.
 */
public class TrazAquiModel implements Serializable {
    Map<String,Utilizador> utilizadores;
    Map<String,Voluntario> voluntarios;
    Map<String,Transportadoras> transportadoras;
    Map<String,Loja> lojas;
    EncomendasAceites aceites;
    Login login;

    /**
     * Método que inicializa a classe a partir de um ficheiro de logs.
     * @param path Caminho para o ficheiro de logs.
     */
    public TrazAquiModel(String path){
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.lojas = new HashMap<>();
        this.aceites = new EncomendasAceites();
        this.login = new Login();

        Parser p = new Parser();
        p.parse(this,path);

        this.utilizadores.values().forEach(a->this.login.addUtilizador(a));
        this.voluntarios.values().forEach(a->this.login.addVoluntario(a));
        this.transportadoras.values().forEach(a->this.login.addTransportadora(a));
        this.lojas.values().forEach(a->this.login.addLoja(a));
    }

    public TrazAquiModel() {
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.lojas = new HashMap<>();
        this.aceites = new EncomendasAceites();
    }

    /**
     * Método que retorna uma copia de todos os clientes do sistema.
     * @return Map em que a key é o codigo do utilizador e o value é o utilizador.
     */
    public Map<String, Utilizador> getUtilizadores() {
        Map<String,Utilizador> r = new HashMap<>();
        for(Map.Entry<String,Utilizador> e:this.utilizadores.entrySet())
            r.put(e.getKey(),e.getValue().clone());
        return r;
    }

    /**
     * Método que retorna uma copia de todos os voluntarios do sistema.
     * @return Map em que a key é o codigo do voluntario e o value é o voluntario.
     */
    public Map<String, Voluntario> getVoluntarios() {
        Map<String,Voluntario> r = new HashMap<>();
        for(Map.Entry<String,Voluntario> e:this.voluntarios.entrySet())
            r.put(e.getKey(),e.getValue().clone());
        return r;
    }

    /**
     * Método que retorna uma copia de todos os transportadores do sistema.
     * @return Map em que a key é o codigo do transportador e o value é o transportador.
     */
    public Map<String, Transportadoras> getTransportadoras() {
        Map<String,Transportadoras> r = new HashMap<>();
        for(Map.Entry<String,Transportadoras> e:this.transportadoras.entrySet())
            r.put(e.getKey(),e.getValue().clone());
        return r;
    }

    /**
     * Método que retorna uma copia de todas as lojas do sistema.
     * @return Map em que a key é o codigo da loja e o value é a loja.
     */
    public Map<String, Loja> getLojas() {
        Map<String,Loja> r = new HashMap<>();
        for(Map.Entry<String,Loja> e:this.lojas.entrySet())
            r.put(e.getKey(),e.getValue().clone());
        return r;
    }


    //--------------Logins---------------

    /**
     * Método que cria um novo utilizador para o sistema, adicionando-o à lista de utilizadores e aos logins.
     * @param id Id do utilizador, tem que ser unico.
     * @param nome Nome do utilizador.
     * @param x Longitude do utilizador.
     * @param y Latitude do utilizador.
     * @param pass Palavra passe do utilizador.
     * @throws DuplicateUserException Execeção para o caso de o utilizador ja existar no lista de utilizador.
     */
    public void criaUtilizador(String id, String nome, double x, double y, String pass) throws DuplicateUserException{
        if(this.utilizadores.containsKey(id)) throw new DuplicateUserException("Utilizador já existe");
        else{
            Utilizador e = new Utilizador(id,nome,new GPS(x,y),new ArrayList<>());
            this.utilizadores.put(id,e);
            this.login.addUtilizadorPass(e,pass);
        }
    }

    /**
     * Método que cria um novo voluntario para o sistema, adicionando-o à lista de voluntarios e aos logins.
     * @param id Id do voluntario, tem que ser unico.
     * @param nome Nome do voluntario.
     * @param x Longitude do voluntario.
     * @param y Latitude do voluntario.
     * @param raio Raio de operação.
     * @param pass Palavra passe do voluntario.
     * @throws DuplicateUserException Execeção para o caso de o voluntario ja existar no lista de voluntarios.
     */
    public void criaVoluntario(String id,String nome,double x,double y,double raio,String pass)throws DuplicateUserException{
        if(this.voluntarios.containsKey(id)) throw new DuplicateUserException("Utilizador já existe");
        else{
            Voluntario v = new Voluntario(id,nome,new GPS(x,y),raio);
            this.voluntarios.put(id,v);
            this.login.addVoluntarioPass(v,pass);
        }
    }

    /**
     * Método que cria um novo transportador para o sistema, adicionando-o à lista de transportadores e aos logins.
     * @param id Id do transportador, tem que ser unico.
     * @param nome Nome do transportador.
     * @param x Longitude do transportador.
     * @param y Latitude do transportador.
     * @param nif Nif da transportadora.
     * @param precoKm Preco por kilometro.
     * @param precoPeso Preco por kiligrama.
     * @param raio Raio de operação.
     * @param pass Palavra passe do transportador.
     * @throws DuplicateUserException Execeção para o caso de o transportador ja existar no lista de transportadores.
     */
    public void criaTransporter(String id,String nome,double x,double y,int nif,double raio,double precoKm, double precoPeso,String pass) throws DuplicateUserException {
        if(this.transportadoras.containsKey(id)) throw new DuplicateUserException("Utilizador já existe");
        else{
            Transportadoras t = new Transportadoras(id,nome,new GPS(x,y),nif,raio,precoKm,precoPeso);
            this.transportadoras.put(id,t);
            this.login.addTransportadoraPass(t,pass);
        }
    }

    /**
     * Método que cria uma nova loja para o sistema, adicionando-o à lista das lojas e aos logins.
     * @param id Id da loja, tem que ser unico.
     * @param nome Nome da loja.
     * @param x Longitude da loja.
     * @param y Latitude da loja.
     * @param pass Palavra passe da loja.
     * @throws DuplicateUserException Execeção para o caso da loja ja existar no lista das lojas.
     */
    public void criaLoja(String id,String nome,double x,double y,String pass) throws DuplicateUserException{
        if(this.lojas.containsKey(id)) throw new DuplicateUserException("Utilizador já existe");
        else{
            Loja l = new Loja(id,nome,new GPS(x,y));
            this.lojas.put(id,new Loja(id,nome,new GPS(x,y)));
            this.login.addLojaPass(l,pass);
        }
    }

    /**
     * Método que adiciona um utilizador à lista de utilizadores.
     * @param e Utilizador a adicionar.
     */
    public void addUtilizador(Utilizador e){
        this.utilizadores.put(e.getCodigo(),e.clone());
    }
    /**
     * Método que adiciona um Voluntario à lista de Voluntarios.
     * @param v Voluntario a adicionar.
     */
    public void addVoluntario(Voluntario v){
        this.voluntarios.put(v.getCodigo(),v.clone());
    }

    /**
     * Método que adiciona um Transportador à lista de Transportadores.
     * @param t Transportador a adicionar.
     */
    public void addTransportadora(Transportadoras t){
        this.transportadoras.put(t.getCodigo(),t.clone());
    }

    /**
     * Método que adiciona uma loja à lista de lojas.
     * @param t Loja a adicionar.
     */
    public void addLoja(Loja l){
        this.lojas.put(l.getCodigo(),l.clone());
    }

    /**
     * Método que coloca uma encomenda aceita numa loja e nas encomendas aceites.
     * @param e Encomenda a adicionar.
     */
    public void addEncomenda(Encomenda e){
        this.lojas.get(e.getCodLoja()).addToAceites(e);
        this.aceites.add(e);
    }

    /**
     * Métpdp que coloca uma encomenda na fila de espera de uma loja.
     * @param e Encomenda a adicionar
     */
    public void addEncomendaLoja(Encomenda e){
        this.lojas.get(e.getCodLoja()).addToFila(e);
    }

    /**
     * Método que adiciona uma encomenda às encomendas aceites e move-a da fila de espera da loja para a lista das aceites da loja.
     * @param e Encomenda a adicionar.
     */
    public void addAceites(Encomenda e){
        this.aceites.add(e);
        this.lojas.get(e.getCodLoja()).removeFila(e);
    }

    /**
     * Método que remove uma encomenda de uma loja que esta na lista das lojas.
     * @param e Encomenda que contem o código da loja á qual vai ser removida.
     */
    public void removeEncomenda(Encomenda e){
        this.aceites.remove(e.getCodLoja(), e);
    }

    /**
     * Método para fazer login dado um mail e uma password.
     * @param mail Mail a usar.
     * @param pass Password da entidade.
     * @return Retorna uma entidade em caso do login ter sido feito com sucesso.
     */
    public Entidade login(String mail,String pass){
        return this.login.loginPass(mail,pass);
    }

    /**
     * Método que serve para saber/definir, qual a entidade que fez login.
     * @param e Entidade que fez login.
     */
    public void setLoginEnt(Entidade e){
        this.login.setEnt(e.clone());
    }

    /**
     * Método que retorna a ultima entidade a fazer login.
     * @return Entidade que fez login.
     */
    public Entidade getLoginEnte(){
        return this.login.getEnt();
    }

    //-------------------------------------------------------------------

    /**
     * Método que filtra todas encomendas que foram feitar por um certo tipo de transporte (Voluntário ou Transportadora).
     * @param trans String que indica qual o elemento a filtar.
     * @param encomendas Lista de encomendas a ser filtrada.
     * @return Lista de encomendas filtrada por transporte.
     */
    public List<Encomenda> filterTransportador(String trans,List<Encomenda> encomendas){
        List<Encomenda> res = new ArrayList<>();
        if(trans.equals("Voluntario")){
            List<Voluntario> v = new ArrayList<>(this.voluntarios.values());
            for (Encomenda e:encomendas){
                boolean flag = false;
                for(int i = 0;i<v.size() && !flag;i++){
                    Voluntario vt = v.get(i);
                    if(vt.getHistorico().containsKey(e.getCodEncomenda())){
                        res.add(e.clone());
                        flag = true;
                    }
                }
            }
        }
        else{
            List<Transportadoras> v = new ArrayList<>(this.transportadoras.values());
            for (Encomenda e:encomendas){
                boolean flag = false;
                for(int i = 0;i<v.size() && !flag;i++){
                    Transportadoras vt = v.get(i);
                    if(vt.getHistorico().containsKey(e.getCodEncomenda())){
                        res.add(e.clone());
                        flag = true;
                    }
                }
            }
        }
        return res;
    }

    /**
     * Método que retorna um set com os transporte cujo raio de ação contem o utilizador pedido, verificando também se a
     * encomenda/transporte são médicos.
     * @param e Utilizador que pede encomenda.
     * @param enc Encomenda que o utilizador pede.
     * @return Set com os transportes disponiveis para fazer a entrega.
     */
    public Set<Transportes> transportesValidosUtilizador(Utilizador e,Encomenda enc){
        Loja l = this.lojas.get(enc.getCodLoja());

        List<Transportes> v = this.voluntarios.values().stream()
                                                       .filter(Voluntario::isFree)
                                                       .filter(a->a.distancia(e)<=a.getRaio())
                                                       .filter(a->{
                                                           if(enc.isEncomendaMedica()) return a.isTransporteMedico();
                                                           else return true;
                                                                })
                                                       .collect(Collectors.toList());
        List<Transportes> t = this.transportadoras.values().stream()
                                                           .filter(Transportadoras::isFree)
                                                           .filter(a->a.distancia(e)<=a.getRaio())
                                                           .filter(a->{
                                                               if(enc.isEncomendaMedica()) return a.isTransporteMedico();
                                                               else return true;
                                                                  })
                                                           .collect(Collectors.toList());

        Set<Transportes> res = new TreeSet<>((transportes, t1) -> (int) (transportes.distancia(l) - (t1.distancia(l))));
        res.addAll(v);
        res.addAll(t);
        return res;
    }

    /**
     * Método que atribui a uma determinada encomenda o codigo de transporte que a entregou.
     */
    public void atribuiTransporte(Loja l,Encomenda e,Transportes t){
        this.aceites.alteraTransporte(l,e,t);
    }

    /**
     * Método que calcula a lista de todas as encomenda que estão prontas a ser entregues a um certo utilizador.
     * @param cod Código do utilizador.
     * @return Lista com as encomenda que o cliente pode receber.
     */
    public List<Encomenda> listaEncomendasUtilizador(String cod){
        return this.aceites.encomendasPendentes(cod);
    }

    /**
     * Método que manda um pedido de entrega a um voluntario.
     * @param e Encomenda que foi requesitada.
     * @param v Voluntario ao qual foi proposta a entrega.
     */
    public void mandaPedido(Encomenda e,Voluntario v){
        this.voluntarios.get(v.getCodigo()).setFree(false);
        this.voluntarios.get(v.getCodigo()).setEnc(e);
    }

    /**
     * Método que faz entrega de uma encomenda, alterando a sua data de levantamento e entrega, assim como removendo
     * esta da loja e adicionando a ao histórico do utilizador e do transportador/voluntario que a entregou.
     * @param l Loja em que estava a encomenda.
     * @param e Encomenda a ser entregue.
     * @param t Transporte que fez a encomenda.
     */
    public void fazEntrega(Loja l,Encomenda e,Transportes t){
        this.atribuiTransporte(l,e,t);
        this.lojas.get(e.getCodLoja()).removeAceites(e);
        Encomenda enc = this.aceites.remove(l.getCodigo(),e);
        //this.lojas.get(l.getCodigo()).removeAceites(e);
        if(t instanceof Transportadoras) {
            this.transportadoras.get(t.getCodigo()).setEnc(enc);
            this.transportadoras.get(t.getCodigo()).entregaEncomenda(this.utilizadores.get(e.getCodUtilizador()), e.getCodUtilizador(),this.lojas.get(e.getCodLoja()));
        }
        else{
            this.voluntarios.get(t.getCodigo()).setEnc(enc);
            this.voluntarios.get(t.getCodigo()).entregaEncomenda(this.utilizadores.get(e.getCodUtilizador()), e.getCodUtilizador(), this.lojas.get(e.getCodLoja()));
        }
    }

    /**
     * Método que gera um código de encomenda válido e que não esteja a ser usado naquele momento.
     * @return Código de uma encomenda.
     */
    public String gereCodEncomenda(){
        List<String> ut = this.utilizadores.values().stream()
                                                    .map(Utilizador::codHistorico)
                                                    .filter(Objects::nonNull)
                                                    .flatMap(Collection::stream)
                                                    .collect(Collectors.toList());
        List<String> enc = this.aceites.codHistorico();
        Set<String> set = new HashSet<>();
        set.addAll(ut);
        set.addAll(enc);
        boolean flag = false;
        String res = "e";
        while (!flag){
            res = "e" + (1000 + (int) (Math.random() * ((9999 - 1000) + 1)));
            if(!set.contains(res)){
                flag = true;
            }
        }
        return res;
    }

    /**
     * Método que calcula a distancia percorrida por un transportador a uma loja e posteriormente a um utilizador.
     */
    public double getDistancia(Loja l, Utilizador ut, Transportes t){
        return t.distancia(l) + l.getCoordenadas().distancia(ut.getCoordenadas());
    }

    /**
     * Método que retorna uma copia das encomendas pendentes que estao numa loja.
     * @param l Codigo da loja
     * @return Lista das encomendas pendentes.
     */
    public List<Encomenda> getPendentesLoja(String l){
        return this.lojas.get(l).getFila();
    }

    /**
     * Método que permite classificar um transporte adicionando uma classificação é lista de classificações.
     * @param t Transporte a classificar
     * @param av Classificação
     */
    public void classificaTransportadora(Transportes t,double av){
        if(t instanceof Transportadoras){
            this.transportadoras.get(t.getCodigo()).addToClassificacao(av);
        }
        else{
            this.voluntarios.get(t.getCodigo()).addToClassificacao(av);
        }
    }

    /**
     * Método que adiciona um certo numero de kms a um Transporte.
     */
    public void aumentaKm(Transportes t,double km){
        if(t instanceof Transportadoras){
            this.transportadoras.get(t.getCodigo()).addToNKms(km);
        }
        else{
            this.voluntarios.get(t.getCodigo()).addToNKms(km);
        }
    }

    /**
     * Método que retorna a classificação média de um certo transporte.
     * @param t Transporte o qual se quer a média.
     * @return Retorna a média da classificação ou NaN caso nao tenha nenhuma classificação
     */
    public double getClassificacao(Transportes t){
        if(t instanceof Transportadoras){
            return this.transportadoras.get(t.getCodigo()).mediaClassificacao();
        }
        else{
            return this.voluntarios.get(t.getCodigo()).mediaClassificacao();
        }
    }

    /**
     * Método que adiciona um produto ao catálogo de uma loja.
     * @param codLoja Codigo da loja a adicionar o produto.
     * @param cod Codigo do produto.
     * @param nome Nome do produto.
     * @param preco Preco do produto.
     */
    public void addToCatalogoLoja(String codLoja, String cod,String nome,Double preco){
        this.lojas.get(codLoja).getListaProdutos().putIfAbsent(cod, new AbstractMap.SimpleEntry<>(nome, preco));
    }

    /**
     * Método que calcula o total faturado por uma empresa transportadora num certo periodo de tempo.(Com o mesmo nome de empresa).
     * @param t Transportadora à qual vai ter usada o nome para fazer os calculos.
     * @param date Data inicial.
     * @param date2 Data final.
     * @return Total faturado por uma empresa.
     */
    public double totalFaturado(Transportadoras t, LocalDateTime date, LocalDateTime date2){
        return this.transportadoras.values().stream().filter(a->a.getNome().equals(t.getNome())).map(a->a.totalFaturado(date,date2)).mapToDouble(Double::doubleValue).sum();
    }

    /**
     * Método que calcula a listas empresas transportadores ordenada pelo numero de kms realizados.
     * @return Lista ordenada de Map.Entry em que a key é o nome da empresa e o value é o numero de kms.
     */
    public List<Map.Entry<String,Double>> topMelhoresTransportes(){
        return this.transportadoras.values().stream().collect(Collectors.groupingBy(Entidade::getNome))
                                                     .entrySet().stream().map(a -> new AbstractMap.SimpleEntry<>(a.getKey(),
                                                                                       a.getValue().stream().map(Transportadoras::getnKms).mapToDouble(Double::doubleValue).sum()))
                                                     .sorted((a,b) -> b.getValue().compareTo(a.getValue()))
                                                     .collect(Collectors.toList());
    }

    /**
     * Método que calcula a lista dos codigos de clientes ordenada pelo número de encomendas recebidas.
     * @return Lista ordenada de Map.Entry em que a key é o codigo do utilizador e o value é o numero de encomendas recebidas.
     */
    public List<Map.Entry<String,Integer>> topMelhoresClientes(){
        return this.utilizadores.values().stream().map(a -> new AbstractMap.SimpleEntry<>(a.getCodigo(),
                                                                 a.getEncomendas().size()))
                                                  .sorted((a,b) -> b.getValue().compareTo(a.getValue()))
                                                  .collect(Collectors.toList());
    }

    /**
     * Método que calcula o tempo esperado da entrega de uma encomenda.
     * @param e Encomenda a ser entregue.
     * @param t Transporte que vai fazer a entrega.
     * @return Retorna o tempo esperado de entrega.
     */
    public double tempoTransporte(Encomenda e,Transportes t){
        return Encomenda.tempoPrevisto(t.getCoordenadas(),this.lojas.get(e.getCodLoja()),this.utilizadores.get(e.getCodUtilizador()).getCoordenadas());
    }

    /**
     * Método que muda a capacidade de um transporte fazer entrega de encomendas médicas.
     * @param t Transporte a alterar.
     * @param b Boolean que indica se pode ou não fazer entregas médicas.
     */
    public void alteraTransportesMedicos(Transportes t,boolean b){
        if(t instanceof Transportadoras){
            this.transportadoras.get(t.getCodigo()).setTransporteMedico(b);
        }
        else{
            this.voluntarios.get(t.getCodigo()).setTransporteMedico(b);
        }
    }

    /**
     * Método que permite guardar o estado do programa num ficheiro objeto.
     * @param file Path para o ficheiro
     * @throws IOException Exceção de erro ao escrever no ficheiro.
     */
    public void save(String file) throws IOException {
        FileOutputStream r = new FileOutputStream(file);
        ObjectOutputStream o = new ObjectOutputStream(r);
        o.writeObject(this);
        o.flush();
        o.close();
    }

    /**
     * Método que carrega um estado a partir de um ficheiro objeto
     * @param file
     * @return Retorna a classe que se encontrava guardada no ficheiro.
     * @throws IOException Erro a ler o ficheiro.
     * @throws ClassNotFoundException O ficheiro lido é inválido para a classe atual.
     */
    public static TrazAquiModel load(String file) throws IOException,ClassNotFoundException{
        FileInputStream r = new FileInputStream(file);
        ObjectInputStream o = new ObjectInputStream(r);
        TrazAquiModel model = (TrazAquiModel) o.readObject();
        o.close();
        return model;
    }
}
