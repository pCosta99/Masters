import java.io.*;
import java.util.Set;
import java.util.TreeSet;

public class BDGeral implements Serializable, BDGeralInterface {
    private final BDVoluntarios voluntarios;
    private final BDUtilizador utilizadores;
    private final BDLojas lojas;
    private final BDTransportes transportes;
    private final BDProdutos produtos;
    private EncomendasAceites encomendasAceites;

    /**
     * Construtores
     */

    public BDGeral(){
        this.voluntarios = new BDVoluntarios();
        this.utilizadores = new BDUtilizador();
        this.lojas = new BDLojas();
        this.transportes = new BDTransportes();
        this.produtos = new BDProdutos();
    }

    public BDGeral(BDGeral b){
        this.voluntarios = b.getVoluntarios();
        this.utilizadores = b.getUtilizadores();
        this.lojas = b.getLojas();
        this.transportes = b.getTransportes();
        this.produtos = b.getProdutos();
        this.encomendasAceites = b.getEncomendasAceites();
    }

    /* Getters */
    public BDLojas getLojas() {
        return this.lojas.clone();
    }

    public EncomendasAceites getEncomendasAceites() {
        return encomendasAceites.clone();
    }

    public BDTransportes getTransportes() {
        return this.transportes.clone();
    }

    public BDUtilizador getUtilizadores() {
        return this.utilizadores.clone();
    }

    public BDVoluntarios getVoluntarios() {
        return this.voluntarios.clone();
    }

    public BDProdutos getProdutos() { return this.produtos.clone();}

    /**
     * Setter de Encomendas Aceites
     * @param encomendasAceites encomendas que foram aceites
     */

    public void setEncomendasAceites(EncomendasAceites encomendasAceites) {
        this.encomendasAceites = encomendasAceites;
    }

    /**
     * Método que adiciona uma novo voluntário ao sistema
     * @param v é o Voluntário a ser adicionado
     */
    public void addVoluntario(Voluntario v){
        if(this.voluntarios.existe(v)){
            System.out.println("Já existe esse voluntário");
        }
        else this.voluntarios.add(v);
    }

    /**
     * Método que atualiza um voluntário do sistema, tornando o disponível para entregar uma encomenda
     * @param v é o Voluntário disponível
     */
    public void addVoluntarioDisponivel(Voluntario v){
        this.voluntarios.add(v);
    }


    /**
     * Método que atualiza uma empresa de transportes do sistema, tornando a disponível para entregar uma encomenda
     * @param e Empresa de transportes
     */

    public void addEmpresaDisponivel(EmpresaTransportes e){
        this.transportes.add(e);
    }

    /**
     * Métoodo que adiciona um novo utilizador doméstico
     * @param u é o Utilizador a ser adicionado
     */

    public void addUser(Utilizador u){
        if(this.utilizadores.existe(u)){
            System.out.println("Já existe esse utilizador");
        }
        else this.utilizadores.add(u);
    }

    /**
     * Método que adiciona uma nova empresa de transportes
     * @param t é a empresa de transportes
     */

    public void addTransporte(EmpresaTransportes t){
        if(this.transportes.existe(t)){
            System.out.println("Já existe essa empresa de transportes");
        }
        else this.transportes.add(t);

    }

    /**
     * Método que adiciona uma nova loja
     * @param l é a loja
     */

    public void addLoja(Loja l){
        if(this.lojas.existe(l))
            System.out.println("Já existe esse voluntário");
        else this.lojas.add(l);
    }

    /**
     * Método que adiciona um novo produto, que pode depois ser encomendado
     * @param p é o novo produto a ser adicionado
     */

    public void addProduto(LinhaEncomenda p){
        if (!this.produtos.existe(p.getDescricao())) this.produtos.add(p);
    }

    public String toString(){
        return "=================== TOTAL DE UTILIZADORES REGISTADOS NO SISTEMA ===================" +
                "-------------Utilizadores----------------------- \n" +
                this.utilizadores.toString() + "\n" +
                "-------------Empresas de transportes------------ \n" +
                this.transportes.toString() + "\n" +
                "-------------Voluntários------------------------ \n" +
                this.voluntarios.toString() + "\n" +
                "-------------Lojas------------------------------\n" +
                this.lojas.toString() + "\n" +
                "-------------Produtos------------------------------\n" +
                this.produtos.toString() + "\n";
    }

    /**
     * Método que atualiza uma loja do sistema, adicionando-lhe uma nova encomenda
     * @param e é a nova encomenda a ser adicionada
     * @param j é a loja a ser atualizada
     */

    public void updateLoja(Encomenda e, Loja j){
        this.lojas.updateLoja(e, j);
    }

    /**
     * Método que atualiza uma loja do sistema, desta vez, retirando-lhe uma encomenda em stock
     * @param e é a encomenda a ser retirada
     * @param j é a loja a ser atualizada
     */
    public void updateLoja2(Encomenda e ,Loja j){
        this.lojas.updateLoja2(e,j);
    }

    /**
     * Método que atualiza uma loja
     * @param j loja
     */
    public void updateLoja3(Loja j){
        this.lojas.updateLoja3(j);
    }

    /**
     * Método que adiciona uma nova encomenda a um utilizador
     * @param e é a nova encomenda realizada
     * @param u é o utilizador a ser atualizado
     */
    public void updateUser(Encomenda e, Utilizador u){
        this.utilizadores.updateUser(e, u);
    }

    /**
     * Método que atualiza um voluntário do sistema
     * @param v é o voluntário a ser atualizado
     */
    public void updateVoluntario2(Voluntario v){
        this.voluntarios.updateVoluntario2(v);
    }

    /**
     * Método que atualiza a classificação de um voluntário
     * @param classificacao é classificação recebida
     * @param v é o voluntário
     */
    public void updateVoluntario(Double classificacao, Voluntario v){
        this.voluntarios.updateVoluntario(v, classificacao);
    }

    /**
     * Método que atualiza um utilizador
     * @param u utilizador
     */
    public void updateUser2(Utilizador u){
        this.utilizadores.updateUser2(u);
    }

    /**
     * Método que atualiza as encomendas aceites do sistema
     * @param cod é o código de encomenda da nova encomenda aceite
     */
    public void updateAceites(String cod){
        this.encomendasAceites.updateAceites(cod);
    }

    /**
     * Método que atualiza a classificação de uma empresa de transportes
     * @param classificacao é a nova classificação
     * @param e é a empresa de transportes
     */
    public void updateTransportes(Double classificacao, EmpresaTransportes e){
        this.transportes.updateTransporte(e, classificacao);
    }

    public void updateEncomendaVoluntario(Encomenda enc) throws EncomendaNotFoundException {
        Voluntario v = this.voluntarios.encontraEnc(enc.getCodigo()).clone();
        v.updateEncomendaPreparada(enc);
        updateVoluntario2(v);
    }

    public void updateEncomendaTransportes(Encomenda enc) throws EncomendaNotFoundException{
        EmpresaTransportes et = this.transportes.encontraEnc(enc.getCodigo()).clone();
        et.updateEncomendaPreparada(enc);
        updateTransportes2(et);
    }

    /**
     * Método que atualiza uma empresa de transportes do sistema
     * @param e é a empresa a ser atualizada
     */

    public void updateTransportes2(EmpresaTransportes e){
        this.transportes.updateTransportes2(e);
    }

    public BDGeral clone(){
        return new BDGeral(this);
    }

    /**
     * Método que efetua a tentativa de login de um utilizador doméstico
     * @param email email
     * @param password password
     * @return Utilizador, caso o login tenha sido efetuado
     * @throws UserNotFoundException caso nao exista utilizador
     */
    public Utilizador loginUser(String email, String password) throws UserNotFoundException{
        Utilizador aux;
        aux = this.utilizadores.tryLogin(email, password);
        if(aux == null) throw new UserNotFoundException();
        else return aux;
    }

    /**
     * Método que efetua a tentativa de login de um Voluntário
     * @param email email
     * @param password password
     * @return Voluntario, caso o login tenha sido efetuado
     * @throws VoluntarioNotFoundException caso nao existam voluntarios
     */

    public Voluntario loginVoluntario(String email, String password) throws VoluntarioNotFoundException{
        Voluntario aux;
        aux = this.voluntarios.tryLogin(email, password);
        if(aux == null) throw new VoluntarioNotFoundException();
        else return aux;
    }

    /**
     * Método que efetua a tentativa de login de uma loja
     * @param email email
     * @param password password
     * @return Loja, caso o login tenha sido efetuado
     * @throws LojaNotFoundException caso nao exista loja
     */

    public Loja loginLoja(String email, String password) throws LojaNotFoundException{
        Loja aux;
        aux = this.lojas.tryLogin(email, password);
        if(aux == null) throw new LojaNotFoundException();
        else return aux;
    }

    /**
     * Método que efetua a tentativa de login de uma empresa de transportes
     * @param email email
     * @param password password
     * @return Empresa de transportes, caso o login tenha sido efetuado
     * @throws TransporteNotFoundException caso nao exista transportes
     */

    public EmpresaTransportes loginEmpresa(String email, String password) throws TransporteNotFoundException{
        EmpresaTransportes aux;
        aux = this.transportes.tryLogin(email, password);
        if(aux == null) throw new TransporteNotFoundException();
        else return aux;
    }

    /**
     * Método que devolve os users que mais encomendas realizaram
     * @return retorna as 10 encomendas do top
     */
    public Set<Pair> top10Encomendas(){
        Set<Pair> result = new TreeSet<>(new ComparaQuantidadePair());
        for(String voluntario: this.voluntarios.getVoluntarios().keySet()){
            Pair aux = new Pair();
            Voluntario v = this.voluntarios.getVoluntarios().get(voluntario).clone();
            aux.setFst(v.getNome());
            aux.setSecond(v.getHistorico().size());
            if(aux.getSnd() != 0) {
                result.add(aux);
            }
        }

        for(String empresa: this.transportes.getTransportes().keySet()){
            Pair aux = new Pair();
            EmpresaTransportes et = this.transportes.getTransportes().get(empresa).clone();
            aux.setFst(et.getNome());
            aux.setSecond(et.getRegistos().size());
            if(aux.getSnd() != 0) {
                result.add(aux);
            }
        }

        return result   ;
    }

    /**
     * Método que devolve as empresas que mais kms percorreram
     * @return top 10 de kms percorridos
     */
    public Set<Pair> top10KmsPercorridos (){
        Set<Pair> result = new TreeSet<>(new ComparaQuantidadePair());

        for(String empresa: this.transportes.getTransportes().keySet()){
            Pair aux = new Pair();
            EmpresaTransportes et = this.transportes.getTransportes().get(empresa).clone();
            aux.setFst(et.getNome());
            aux.setSecond(et.getKms(this));
            if(aux.getSnd() != 0) {
                result.add(aux);
            }
        }
        return result   ;
    }

    /**
     * Método que grava os dados num ficheiro binário
     * @param filename nome do ficheiro
     * @throws IOException exceçao de io
     * @throws FileNotFoundException ficheiro nao existe
     */

    public void gravarFicheiro(String filename) throws IOException, FileNotFoundException {
        FileOutputStream fos = new FileOutputStream(filename);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    /**
     * Método que carrega os dados de um ficheiro binário
     * @param filename nome do ficheiro
     * @return uma bd geral
     * @throws IOException exceçao de io
     * @throws ClassNotFoundException nao existe a classe
     */
    public BDGeral lerFicheiro(String filename) throws IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream(filename);
        ObjectInputStream ois = new ObjectInputStream(fis);
        BDGeral d = (BDGeral) ois.readObject();
        ois.close();
        return d;
    }



}
