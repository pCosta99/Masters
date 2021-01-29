import javax.xml.crypto.dsig.TransformService;
import java.awt.geom.Point2D;
import java.io.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class Service implements Serializable {
    private Map<String,Utilizador> utilizadores;
    private Map<String,Voluntario> voluntarios;
    private Map<String,Loja> lojas;
    private Map<String,Transportadora> transportadoras;
    private Map<String,Encomenda> encomendas;


    public Service() {
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.lojas = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.encomendas = new HashMap<>();
    }

    public Service( Map<String,Utilizador> u,
                    Map<String,Voluntario> v,
                    Map<String,Loja> l,
                    Map<String,Transportadora> t,
                    Map<String,Encomenda> e) {
        setUtilizadores(u);
        setVoluntarios(v);
        setLojas(l);
        setTransportadoras(t);
        setEncomendas(e);
    }

    public Service(Service s) {
        setUtilizadores(s.getUtilizadoress());
        setVoluntarios(s.getVoluntarios());
        setLojas(s.getLojas());
        setTransportadoras(s.getTransportadoras());
        setEncomendas(s.getEncomendas());
    }

    /**
     * Devolve a listagem de Utilizadores que o serviço possui
     */
    public Map<String, Encomenda> getEncomendas() {
        Map<String,Encomenda> aux = new HashMap<>();
        this.encomendas.entrySet().forEach(v -> aux.put(v.getKey(),v.getValue().clone()));
        return aux;
    }

    /**
     * Atribui ao serviço uma listagem de Utilizadores.
     */
    public void setEncomendas(Map<String, Encomenda> encomends) {
        this.encomendas = new HashMap<>();
        encomends.entrySet().forEach(v -> this.encomendas.put(v.getKey(),v.getValue().clone()));
    }



    /**
     * Devolve a listagem de Utilizadores que o serviço possui
     */
    public Map<String, Utilizador> getUtilizadoress() {
        Map<String,Utilizador> aux = new HashMap<>();
        this.utilizadores.entrySet().forEach(v -> aux.put(v.getKey(),v.getValue().clone()));
        return aux;
    }

    /**
     * Atribui ao serviço uma listagem de Utilizadores.
     */
    public void setUtilizadores(Map<String, Utilizador> utilizadors) {
        this.utilizadores = new HashMap<>();
        utilizadors.entrySet().forEach(v -> this.utilizadores.put(v.getKey(),v.getValue().clone()));
    }

    /**
     * Devolve a listagem de Voluntarios que o serviço possui
     */
    public Map<String, Voluntario> getVoluntarios() {
        Map<String,Voluntario> aux = new HashMap<>();
        this.voluntarios.entrySet().forEach(v -> aux.put(v.getKey(),v.getValue().clone()));
        return aux;
    }

    /**
     * Atribui ao serviço uma listagem de Voluntarios.
     */
    public void setVoluntarios(Map<String, Voluntario> voluntaris) {
        this.voluntarios = new HashMap<>();
        voluntaris.entrySet().forEach(v -> this.voluntarios.put(v.getKey(),v.getValue().clone()));
    }

    /**
     * Devolve a listagem de lojas que o serviço possui
     */
    public Map<String, Loja> getLojas() {
        Map<String,Loja> aux = new HashMap<>();
        this.lojas.entrySet().forEach(v -> aux.put(v.getKey(),v.getValue().clone()));
        return aux;
    }

    /**
     * Atribui ao serviço uma listagem de Lojas.
     */
    public void setLojas(Map<String, Loja> lojs) {
        this.lojas = new HashMap<>();
        lojs.entrySet().forEach(v -> this.lojas.put(v.getKey(),v.getValue().clone()));
    }

    /**
     * Devolve a listagem de transportadoras que o serviço possui
     */
    public Map<String, Transportadora> getTransportadoras() {
        Map<String,Transportadora> aux = new HashMap<>();
        this.transportadoras.entrySet().forEach(v -> aux.put(v.getKey(),v.getValue().clone()));
        return aux;
    }

    /**
     * Atribui ao serviço uma listagem de transportadoras.
     */
    public void setTransportadoras(Map<String, Transportadora> transportadors) {
        this.transportadoras = new HashMap<>();
        transportadors.entrySet().forEach(v -> this.transportadoras.put(v.getKey(),v.getValue().clone()));
    }


    /**
     * Adicionar utilizador ao sistema
     * @param u Utilizador
     * @throws PessoaRepetidaException Caso o utilizador já exista
     */
    public void adicionaCliente(Utilizador u) throws PessoaRepetidaException{
        if(!utilizadores.containsKey(u.getCodigo())){
            this.utilizadores.put(u.getCodigo(),u.clone());
        } else {
            throw new PessoaRepetidaException("Utilizador com o codigo "+u.getCodigo()+" já existe!");
        }
    }

    /**
     * Adicionar voluntario ao sistema
     * @param v voluntario
     * @throws PessoaRepetidaException Caso o voluntario já exista
     */
    public void adicionaVoluntario(Voluntario v) throws PessoaRepetidaException{
        if(!voluntarios.containsKey(v.getCodigo())){
            this.voluntarios.put(v.getCodigo(),v.clone());
        } else {
            throw new PessoaRepetidaException("Voluntario com o codigo "+v.getCodigo()+" já existe!");
        }
    }

    /**
     * Adicionar Empresa transportadora ao sistema
     * @param t Empresa transportadora
     * @throws EmpresaRepetidaException Caso transportadora já exista
     */
    public void adicionaEmpresaTransportadora(Transportadora t) throws EmpresaRepetidaException{
        if(!transportadoras.containsKey(t.getCodigo())){
            this.transportadoras.put(t.getCodigo(),t.clone());
        } else {
            throw new EmpresaRepetidaException("Transportadora com o codigo "+t.getCodigo()+" já existe!");
        }
    }

    /**
     * Adicionar loja ao sistema
     * @param l Loja
     * @throws EmpresaRepetidaException Caso Loja já exista
     */
    public void adicionaLoja(Loja l) throws EmpresaRepetidaException{
        if(!lojas.containsKey(l.getCodigo())){
            this.lojas.put(l.getCodigo(),l.clone());
        } else {
            throw new EmpresaRepetidaException("Loja com o codigo "+l.getCodigo()+" já existe!");
        }
    }

    /**
     * Adicionar uma encomenda a uma loja por parte do cliente
     * @param e Ecomenda
     * @throws LojaInexistenteException Caso Loja não exista


    public void adicionaEncomendaLoja(Encomenda e) throws LojaInexistenteException{
        if(lojas.containsKey(e.getCodLoja())) {
            Loja l = lojas.get(e.getCodLoja());
            l.addEncomenda(e);
        }
        else{
            throw new LojaInexistenteException("Loja com o codigo " + e.getCodLoja()+ " não existe!");
        }
    }*/
    /**
     * Login
     * @param credenciais Credenciais de login, Email e password
     * @return String com o tipo de ator do sistema
     */
    public String login(String[] credenciais) {
        String email = credenciais[0];
        String password  = credenciais[1];
        String r = "Credenciais Erradas";
       if(this.utilizadores.values().stream().filter(u -> u.getEmail().equals(email)).count() ==1 &&
               this.utilizadores.values().stream().filter(u -> u.getPassword().equals(password)).count() ==1)
            r = "utilizador";
       if(this.voluntarios.values().stream().filter(u -> u.getEmail().equals(email)).count() ==1 &&
               this.voluntarios.values().stream().filter(u -> u.getPassword().equals(password)).count() ==1)
           r = "voluntario";
       if(this.lojas.values().stream().filter(u -> u.getEmail().equals(email)).count() ==1 &&
               this.lojas.values().stream().filter(u -> u.getPassword().equals(password)).count() ==1)
           r = "loja";
        if(this.transportadoras.values().stream().filter(u -> u.getEmail().equals(email)).count() ==1 &&
                this.transportadoras.values().stream().filter(u -> u.getPassword().equals(password)).count() ==1)
            r = "transportadora";
        return r;
    }

    // dado email devolve o codUtilizador
    public String getUtilizadorEmail(String email){
        String s = "No User";
        for(Utilizador u : this.utilizadores.values()) {
            if (u.getEmail().equals(email)) ;
            s = u.getCodigo();
        }
        return s;
    }
    // dado email devolve o codUtilizador
    public String getLojaEmail(String email){
        String s = "No User";
        for(Loja u : this.lojas.values()) {
            if (u.getEmail().equals(email)) ;
            s = u.getCodigo();
        }
        return s;
    }

    // dado email devolve o codVOl
    public String getVoluntarioEmail(String email){
        String s = "No User";
        for(Voluntario u : this.voluntarios.values()) {
            if (u.getEmail().equals(email)) ;
            s = u.getCodigo();
        }
        return s;
    }
    // dado email devolve o codVOl
    public String getTransportadoraEmail(String email){
        String s = "No User";
        for(Transportadora u : this.transportadoras.values()) {
            if (u.getEmail().equals(email)) ;
            s = u.getCodigo();
        }
        return s;
    }



    /**
     * Login
     * @param registo Credenciais de registo, Tipo de ator, Email e password
     */
    public int registo(String[] registo) throws PessoaRepetidaException,EmpresaRepetidaException,AtorInvalidoException,NumberFormatException{
        String tipo = registo[0];
        String email = registo[1];
        String password  = registo[2];
        int r = -1;
        if(this.utilizadores.values().stream().filter(u -> u.getEmail().equals(email)).count()>=1 && tipo.equals("utilizador"))
            throw new PessoaRepetidaException( "utilizador com o email " + email + "ja existe!");
        if(this.voluntarios.values().stream().filter(u -> u.getEmail().equals(email)).count()>=1  && tipo.equals("voluntario"))
            throw new PessoaRepetidaException( "voluntario com o email " + email + "ja existe!");
        if(this.lojas.values().stream().filter(u -> u.getEmail().equals(email)).count()>=1 && tipo.equals("loja"))
            throw new EmpresaRepetidaException( "loja com o email " + email + "ja existe!");
        if(this.transportadoras.values().stream().filter(u -> u.getEmail().equals(email)).count()>=1  && tipo.equals("transportadora"))
            throw new EmpresaRepetidaException( "transportadora com o email " + email + "ja existe!");

            boolean rand;
            String cod;
            Random random = new Random();
            int val;
            switch (tipo){
                case "utilizador":
                    rand = true;
                    do{
                        val= random.nextInt(1000);
                        cod = "u"+val;
                        List<String> listU= new ArrayList<>();
                        this.utilizadores.values().stream().map(u -> listU.add(u.getCodigo()));
                        if(!listU.contains(cod))
                            rand = false;
                    }while(rand);
                    try{Utilizador u = new Utilizador(cod,registo[3],Double.parseDouble(registo[4]),Double.parseDouble(registo[5]),email,password,0);
                    adicionaCliente(u);}
                    catch(NumberFormatException e){
                        throw new NumberFormatException("por favor insira um formato valido na latitude longitude");
                    }

                    break;
                case "voluntario":
                    rand = true;
                    do{
                        val = random.nextInt(1000);
                        cod = "v"+val;
                        List<String> listV= new ArrayList<>();
                        this.voluntarios.values().stream().map(v -> listV.add(v.getCodigo()));
                        if(!listV.contains(cod))
                            rand = false;
                    }while(rand);
                    try{ Voluntario v = new Voluntario(cod,registo[3],Double.parseDouble(registo[4]),Double.parseDouble(registo[5]),Double.parseDouble(registo[6]),
                                                   email,password,new Classificacoes(),0);
                    adicionaVoluntario(v);}
                     catch(NumberFormatException e){
                    throw new NumberFormatException("por favor insira um formato valido na latitude, longitude e/ou raio");
                }

                    break;
                case "loja":
                    rand = true;
                    do{
                        val= random.nextInt(1000);
                        cod = "l"+val;
                        List<String> listU= new ArrayList<>();
                        this.lojas.values().stream().map(l -> listU.add(l.getCodigo()));
                        if(!listU.contains(cod))
                            rand = false;
                    }while(rand);
                    try{ Loja l = new Loja(cod,registo[3],Double.parseDouble(registo[4]),Double.parseDouble(registo[5]),email,password,new ArrayList<>());
                    adicionaLoja(l);}
                     catch(NumberFormatException e){
                    throw new NumberFormatException("por favor insira um formato valido na latitude longitude");
                }

                    break;
                case "transportadora":
                    rand = true;
                    do{
                        val = random.nextInt(1000);
                        cod = "t"+val;
                        List<String> listT= new ArrayList<>();
                        this.transportadoras.values().stream().map(t -> listT.add(t.getCodigo()));
                        if(!listT.contains(cod))
                            rand = false;
                    }while(rand);
                    try{ Transportadora t = new Transportadora(cod,registo[3],Double.parseDouble(registo[4]),Double.parseDouble(registo[5]),registo[8],
                                                          Double.parseDouble(registo[6]),Double.parseDouble(registo[7]),email,password,new Classificacoes(),0,0);

                    adicionaEmpresaTransportadora(t);}
                    catch(NumberFormatException e){
                    throw new NumberFormatException("por favor insira um formato valido na latitude, longitude, raio e/ou preço por km");

                }

                    break;
                case "0" :
                    r = 0;
                    break;
                default:
                    throw new AtorInvalidoException("O tipo de ator que escolher nao é admissivel!");
            }
            return r;
    }



    // classificar um voluntário ou uma transportadora, testado e a fucnionar
    public void classificarEntrega(String id, int classificacao) throws AtorInvalidoException {
        Classificacoes aux = this.voluntarios.get(id).getclassificacoes();
        if (this.voluntarios.containsKey(id)){
            aux.adicionaClassificacao(classificacao);
            this.voluntarios.get(id).setClassificacoes(aux);
        }
        else if (this.transportadoras.containsKey(id)){
            aux.adicionaClassificacao(classificacao);
            this.transportadoras.get(id).setClassificacoes(aux);
        }
        else throw new AtorInvalidoException("Não existe nenhum Voluntário/Transportadora com o id " + id + " !");
    }



    // Falta povoar o histórico dos utilizadores para ver se funciona
    public List<String> melhoresUtilizadores() {
        Comparator<Utilizador> u = (Utilizador u1, Utilizador u2) -> Integer.compare(u2.getNrEncomendas(), u1.getNrEncomendas());

        return utilizadores.values().stream()
                .filter(Objects::nonNull)
                .sorted(u)
                .map(Utilizador::toString)
                .limit(10)
                .collect(Collectors.toList());
    }

    // devolve os codigos de todas lojas
    public String getLojasCodNome(){
        return this.lojas.values().stream()
                   .map(l -> l.getCodigo()).collect(Collectors.toList()).toString();
    }

    // devolve os codigos de todas encomendas disponiveis para transporte
    public String getEncomendasParaTransporte(String trans){
        return this.encomendas.values().stream()
                .filter(e -> (e.getEstado()==4 || e.getEstado()==0) && dentroEspacoTrans(e.getCodLoja(),trans) )
                .map(l -> l.getCodEncomenda()).collect(Collectors.toList()).toString();
    }

    // devolve os codigos de todas encomendas disponiveis para transporte
    public String getEncomendasParaTransporteVol(String trans){
        return this.encomendas.values().stream()
                .filter(e -> (e.getEstado()==4 || e.getEstado()==0) && dentroEspacoVol(e.getCodLoja(),trans) )
                .map(l -> l.getCodEncomenda()).collect(Collectors.toList()).toString();
    }

    public boolean dentroEspacoVol(String loja, String vol){
        Loja l =this.lojas.get(loja);
        Voluntario v = this.voluntarios.get(vol);
        return dentroEspaço(l.getGpsx(),l.getGpsy(),v.getGpsx(),v.getGpsy(),v.getRaio());
    }

    public boolean dentroEspacoTrans(String loja, String trans){
        Loja l =this.lojas.get(loja);
        Transportadora v = this.transportadoras.get(trans);
        return dentroEspaço(l.getGpsx(),l.getGpsy(),v.getGpsx(),v.getGpsy(),v.getRaio());
    }

    public boolean dentroEspaço(double x,double y, double xx, double yy, double raio){
        Point2D.Double p1 = new Point2D.Double(x,y);
        Point2D.Double p2 = new Point2D.Double(xx,yy);


        double dist = p1.distance(p2);
        return dist<=raio;
    }

    // dado um codigo de loja, devolve os produtos todos da mesma
    public String getProdutosLoja(String cod) throws LojaInexistenteException{
        if(this.lojas.containsKey(cod))
            return this.lojas.get(cod).getProdutos().toString();
        else {
            throw new LojaInexistenteException(" A loja que escolheu nao existe!");
        }
    }

        // atualiza produtos na compra
    public Produto atualizaProdutos(String codProd, double quantidade, String codLoja)  throws QuantidadeEmExcessoException,ProdutoInexistenteException{
         return this.lojas.get(codLoja).compraDeProduto(codProd,quantidade);
    }

    // devolve lista de encomendas pendentes de determinada loja
    public String getEncomendasLoja(String codLoja){
        return this.encomendas.values().stream().filter(en -> en.getCodLoja().equals(codLoja)).collect(Collectors.toList()).toString();

    }

    //Altera estado de encomenda para pronta para entrega
    public void alteraEstado(String enc, String loj){
        this.encomendas.get(enc).setEstado(4);
    }

    //Altera estado de encomenda
    public void alteraEstadoTransportadora(String enc, String trans){
        this.encomendas.get(enc).setEstado(4);
        this.encomendas.get(enc).setCodTransport(trans);
    }

    //Altera estado de encomenda
    public void alteraEstadoVoluntario(String enc, String trans){
        this.encomendas.get(enc).setEstado(2);
        this.encomendas.get(enc).setCodTransport(trans);
    }

    // adciona uma encomenda a uma loja por parte de um cliente
    public void addEncomenda(String cliente, String loja, LinhaEncomenda e) {
        String enc;
        Random rand = new Random();
        do{
            int r = rand.nextInt(1000)+1;
            enc = "e"+r;
        }while(this.encomendas.containsKey(enc));
        Encomenda ne = new Encomenda(enc,cliente,loja,e.getPeso(),e.clone(), LocalDate.now(),1,"");
        this.utilizadores.get(cliente).addEncomenda();
        addEncomenda(ne);
    }

    public void addEncomenda(Encomenda ne){
        this.encomendas.put(ne.getCodEncomenda(),ne.clone());
    }

    // para ver as transportadoras que fizeram mais kms
    public List<String> transportadorasMaisKms(){
        Comparator<Transportadora> t = (Transportadora t1, Transportadora t2) -> {
            if (t1.getKms() < t2.getKms()) return -1;
            if (t1.getKms() > t2.getKms()) return 1;
            return 0;
        };
        return transportadoras.values().stream()
                .filter(Objects::nonNull)
                .map(l->(Transportadora)l)
                .sorted(t)
                .map(Transportadora::toString)
                .limit(10)
                .collect(Collectors.toList());
    }


    // total faturado por uma empresa

   // public double totalFaturadoEmpresa(String id, LocalDate inicio, LocalDate fim)





    // guarda o estado atual em binário
    public void guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this); // guarda todo o objeto de uma só vez
        oos.flush();
        oos.close();
    }

    //
    public Service carregaEstado(String nomeFicheiro) throws IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Service e = (Service) ois.readObject();
        ois.close();
        return e;
    }





    public Service clone(){
        return new Service(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Serviço{\n")
               .append(this.utilizadores.toString())
               .append(this.lojas.toString())
               .append(this.transportadoras.toString())
               .append(this.voluntarios.toString())
                .append(this.encomendas.toString())
               .append("\n}\n");
        return sb.toString();
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Service servico = (Service) o;
        return utilizadores.equals(servico.utilizadores) &&
                voluntarios.equals(servico.voluntarios) &&
                transportadoras.equals(servico.transportadoras) &&
                lojas.equals(servico.lojas) &&
                encomendas.equals(servico.encomendas);
    }

}
