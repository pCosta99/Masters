package model;

import exceptions.*;

import java.io.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class TrazAqui implements Serializable {

    private Map<String,User> utilizadores;
    private Map<String,EmpresaTransporte> catEmpresas;
    private List<Loja> catLojas;
    private Set<Voluntario> VolDesocupados; //Voluntarios desocupados
    private Set<Voluntario> VolOcupados;  //Voluntarios ocupados
    private List<Encomenda> gestaoEncomendas; //pedidos
    
    public TrazAqui(){
        this.utilizadores = new HashMap<String,User>();
        this.catEmpresas = new HashMap<String,EmpresaTransporte>();
        this.catLojas = new ArrayList<>();
        this.VolDesocupados = new HashSet<>();
        this.VolOcupados = new HashSet<>();
    }
    
    public TrazAqui(Map<String,User> u,Map<String, EmpresaTransporte> cE, List<Loja> cL, User s, HashSet<Voluntario> vD, HashSet<Voluntario> vO){
        this.utilizadores = u;
        this.catEmpresas = cE;
        this.catLojas = cL;
        this.VolDesocupados = vD;
        this.VolOcupados = vO;
    }
    
       public TrazAqui(TrazAqui a){
        setUtilizadores(a.getUtilizadores());
        setEmpresasT(a.getEmpresas());
        //setlojas(a.getlojas());
        //setUser(a.getUser());
        setVolD(a.getVolD());
        setVolO(a.getVolO());
    }

    /*
    Getter utilizadores
     */
    public Map<String,User> getUtilizadores(){
        Map<String,User> novo = new HashMap<>(this.utilizadores.size());
        for(Map.Entry<String,User> entry : this.utilizadores.entrySet()){
            novo.put(entry.getKey(),entry.getValue());
        }
        return novo;
    }

    /*
    Get utilizadores Voluntarios
     */

    public List<Voluntario> getUtilizadoresVoluntarios(){
        List<Voluntario> voluntarios = new ArrayList<>();
        for(User u: this.utilizadores.values()){
            if(u instanceof Voluntario) voluntarios.add((Voluntario) u.clone());
        }

        return voluntarios;
    }
    /*
    Set de um map de utilizadores retirar o putall
     */
    public void setUtilizadores (Map<String,User> u){
        this.utilizadores.putAll(u);
    }

    /*
    Get de uma empresa transportadora especifica


     */

    public EmpresaTransporte getEmpresaEspecifica(String s) throws ExcecaoEmpresaInexistente {
        if(this.catEmpresas.containsKey(s)) return this.catEmpresas.get(s);

        else

            throw new ExcecaoEmpresaInexistente("Empresas Inexistente");

    }




    /*
    Get das Empresas todas
     */

    public Map<String,EmpresaTransporte> getEmpresas(){

        return this.catEmpresas.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey,value->value.getValue()));
    }


    /*
    Set das Empresas todas
     */
    public void setEmpresasT (Map<String,EmpresaTransporte> c){
        this.catEmpresas.putAll(c); //alterar isto nestor disse para nao usar o putAll
    }

    /*
    Get uma loja especifica
     */
    public Loja getLojaEspecifica(String s) throws ExcecaoLojaInexistente {
        for(Loja l: this.catLojas){
            if(l.getCod().equals(s)) return l;
        }
        throw new ExcecaoLojaInexistente("Loja Insxistente");
    }


    /*
    Get das lojas todas
     */
    public List<Loja> getLojas(){
        List<Loja> nova = new ArrayList<>();
        for(Loja s: catLojas){
            nova.add(s.clone());
        }
        return nova;
    }

    /*
    Get de Lojas com estatuto especial
     */

    public List<Loja> getLojascomEspecial(){
        List<Loja> especiais = new ArrayList<>();
        for(Loja s: catLojas){
            if(s.getEstatuto()) especiais.add(s.clone());
        }
        return especiais;
    }



    /*
    Set das lojas todas
     */
    public void setLojas (List<Loja> c){
        this.catLojas = c;
    }


    /*
    Get da lista dos Voluntários Desocupados
     */
    public Set<Voluntario> getVolD(){
        Set<Voluntario> r = new HashSet<>();
        
        for(Voluntario s: VolDesocupados) r.add(s.clone());
        return r;
    }

    /*
    Altera a lista dos Voluntários Desocupados
     */
    public void setVolD(Set<Voluntario> a){
        this.VolDesocupados = new HashSet<>();
        for(Voluntario s: VolDesocupados) this.VolDesocupados.add(s); 
    }
   

    /*
    Get da lista dos Voluntarios Ocupados
     */
    public Set<Voluntario> getVolO(){
        Set<Voluntario> r = new HashSet<Voluntario>();
        
        for(Voluntario s: VolOcupados) r.add(s.clone());
        return r;
    }

    /*
    Altera a lista dos Voluntarios Ocupados
     */
    public void setVolO(Set<Voluntario> a){
        this.VolOcupados = new HashSet<Voluntario>();
        for(Voluntario s: VolOcupados) this.VolOcupados.add(s); 
    }



    /*
    Regista nova EmpresaTransporte
     */
    public void registaEmpresaTransporte(EmpresaTransporte empresa) throws ExcecaoRegisto {

        if (this.catEmpresas.containsKey(empresa.getId())) {
            throw new ExcecaoRegisto("Empresa já existente no catálogo");

        } else {
            this.catEmpresas.put(empresa.getId(), empresa);

        }
    }


    /*
    Regista nova Loja
     */
    public void registaLoja(Loja loja) throws ExcecaoRegisto{

        if (this.catLojas.contains(loja)){
            throw new ExcecaoRegisto("Loja já existente no catálogo");

        }else {
            this.catLojas.add(loja.clone());

        }

    }


    /*
    Lista de Produtos de cada Loja
     */


    public List<Produto> getListaProdutos(Loja loja) throws ExcecaoLojaInexistente{
        List<Produto> lista = new ArrayList<>();
        if(this.catLojas.contains(loja)) lista = this.getLojaEspecifica(loja.getCod()).getprodutos();

        else return null;
            throw new ExcecaoLojaInexistente("Loja Inexistente");

    }




/*--------------------------------------------Tratamentos de User (Cliente)--------------------------------*/


    /*
    Login
     */
    public User login(String id,String password) throws ExcecaoLogin {

        if(utilizadores.containsKey(id)) {
            User usuario = utilizadores.get(id);
            if (usuario.getPassword().equals(password)) return usuario;

            else throw new ExcecaoLogin("Password Incorreta");
        }else throw new ExcecaoLogin("Id de User não existente!");

    }

    /*
    Add e remove User
     */

    public void addUser(User novo) throws ExcecaoUserExistente{
        if(this.utilizadores.containsKey(novo.getId()))
            throw new ExcecaoUserExistente("User já Existente!");

        else this.utilizadores.put(novo.getId(),novo);

    }

    /*
    Get user especifico
     */

    public User getUser(String id) throws ExcecaoUserInvalido {
        User user = this.utilizadores.get(id);
        if(user == null) throw new ExcecaoUserInvalido("User nao existente");

        return user;
    }



    /*
    Coordenadas do cliente
     */


    /*
    Determinar a listagens dos 10 Clientes que mais utilizam o sistema (em número de encomendas transportadas);
     */

    public List<Cliente> lista10ClientesMaisAtivos(){
        List<Cliente> mativos = new ArrayList<Cliente>((Collection<? extends Cliente>) new ComparatorNrEncomendasEmpresas());
        Cliente cliente = null;

        for(User c : utilizadores.values()){
            if(c instanceof Cliente ){
                cliente = (Cliente) c;
                mativos.add(cliente);
            }
        }
        return mativos.stream().limit(10).collect(Collectors.toList());
    }

    public List<Voluntario> listaVoluntariosMaisAtivos(){
        List<Voluntario> mativos = new ArrayList<Voluntario>((Collection<? extends Voluntario>) new ComparatorNrEncomendasEmpresas());
        Voluntario voluntario = null;

        for(User c : utilizadores.values()){
            if(c instanceof Voluntario ){
                voluntario = (Voluntario) c;
                mativos.add(voluntario);
            }
        }
        return mativos.stream().limit(10).collect(Collectors.toList());
    }

    /*
    top 10 empresas mais ativas
    */
    public List<EmpresaTransporte> lista10EmpresasMaisAtivas()  {
        List<EmpresaTransporte> mativos = new ArrayList<EmpresaTransporte>((Collection<? extends EmpresaTransporte>) new ComparatorNrEncomendasEmpresas());


        for(EmpresaTransporte c : catEmpresas.values()){
            EmpresaTransporte empresa =  c;
            mativos.add(empresa);
        }
        return mativos.stream().limit(10).collect(Collectors.toList());
    }



/*----------------------------------------Tratamento de Empresas ------------------------------------------*/

    /*
    Facturação por empresa especifica
    */
    public double facturacaoEmpresa(EmpresaTransporte e){
        return this.catEmpresas.get(e).facturacaoTotal();
    }

    /*
    Facturaçao de uma empresa num determinado período
     */

    public double facturacaoPeriodo(EmpresaTransporte e, LocalDateTime incio, LocalDateTime fim) throws ExcecaoSemFacturacao {
        double total=0;
        if(this.catEmpresas.containsKey(e.getId())){
            List<Encomenda> encomendas = this.catEmpresas.get(e.getId()).registoEncomendaemPeriodo(incio,fim);

            for(Encomenda enc: encomendas){
                total+= enc.custoReal();

            }
        }else throw new ExcecaoSemFacturacao("Facturacao a 0");

        return total;
    }


    public List<Encomenda> getListaEncomendaporPeriodoEmpresa(EmpresaTransporte e, LocalDateTime inicio, LocalDateTime fim){
        return this.catEmpresas.get(e.getId()).registoEncomendaemPeriodo(inicio,fim);
    }

    public List<Encomenda> getListaEncomendaporPeriodoCliente(Cliente c,LocalDateTime inicio, LocalDateTime fim){
        return this.utilizadores.get(c.getId()).registoEncomendaemPeriodo(inicio,fim);
    }

    /*
    Get de uma Lista de Empresas
     */

    public List<EmpresaTransporte> getListaEmpresa(){
         List<EmpresaTransporte> listaEmpresas = new ArrayList<EmpresaTransporte>(catEmpresas.values());
         return listaEmpresas;

    }

    /*
    Get do registo de encomendas total
     */

    public List<Encomenda> getListEncomendasEmpresa(EmpresaTransporte e) throws ExcecaoRegistoDeEncomendasNull{
        if(this.catEmpresas.containsKey(e.getId())) return this.catEmpresas.get(e.getId()).getRegistoEncomendas();

        else throw new ExcecaoRegistoDeEncomendasNull("Empresa Inexistente");
    }


    /*
    ADDS
     */
    public void addEncomendaAEmpresa(EmpresaTransporte e, Encomenda em){
        this.catEmpresas.get(e.getId()).addEncomendaEmpresa(em.clone());
    }

    public void addEncomendaPendingAVoluntario(Voluntario v,Encomenda em){
        this.utilizadores.get(v.getId()).addEncomendaPending(em.clone());
    }

    public void addEncomendaHistoricoAVoluntario(Voluntario v,Encomenda em){
        this.utilizadores.get(v.getId()).addEncomendaRegisto(em.clone());
    }


    public void addEncomendaHistoricoACliente(Cliente v,Encomenda em){
        this.utilizadores.get(v.getId()).addEncomendaRegisto(em);
    }

    public void addEncomendaPendingACliente(Cliente v,Encomenda em){
        this.utilizadores.get(v.getId()).addEncomendaPending(em);
    }

    public void addEncomendaLoja(Loja l,Encomenda em) throws ExcecaoLojaInexistente {
        this.getLojaEspecifica(l.getCod()).addEpending(em);
    }


    /*
    REMOVES
     */


    public void removeEncomendaPendingVoluntario(Voluntario v, Encomenda em){
        this.utilizadores.get(v.getId()).removeEncomendaPending(em);
    }

    public void removeEncomendaPendingCliente(Cliente v, Encomenda em){
        this.utilizadores.get(v.getId()).removeEncomendaPending(em);
    }






    

    /*----------------------------------------Tratamento de Voluntarios --------------------------------------*/

    /*


    A escolha por parte de um cliente de um Voluntario vai depender de ele ser o mais proximo de uma loja e
    de trabalhar no raio da loja e do proprio cliente;

    */

    /*
    Função que nos dá o voluntario que mais perto da loja está
     */

    public Voluntario voluntarioMaisPerto(Set<Voluntario> vols,Coordenadas c) throws Exception { // coordenadas estas que vao pertencer à Loja
        Voluntario volmaisproximo = null;


        for(Voluntario vol: vols) {
            if (volmaisproximo == null) {

                volmaisproximo = vol.clone();

            }else {
                if(volmaisproximo.getCoor().distancia(c) > vol.getCoor().distancia(c)) volmaisproximo = vol.clone();
            }
        }

        if(volmaisproximo != null) return volmaisproximo;

        else throw new Exception("Sem Voluntários!");


    }




    /*
    Fazendo a encomenda (o Cliente) e escolhendo que prefere Voluntario o metodo devolve o vol mais indicado para que este aceite
    ou não
     */
    public Voluntario voluntarioEscolhido(Coordenadas destino,Loja loja) throws Exception {
        Set<Voluntario> aceitaveis = new HashSet<>();

        for(Voluntario vol: this.VolDesocupados){ //lista de todos os aceitaveis
            if(dentroDeRaio(vol,destino,loja.getLocal())) aceitaveis.add(vol.clone());
        }

        return voluntarioMaisPerto(aceitaveis,loja.getLocal());

    }

    /*
    -Tempo medio que um Voluntário deverá demorar a entregar a encomenda.

    -Fazendo nesta viagem o percurso LocalizacaoVoluntario->Loja + Tempo de espera na loja + Loja -> LocalizacaoCliente
     */

    public double distanciaVoluntario(Voluntario v, Coordenadas q){
        return v.getCoor().distancia(q);
    }


    /*
    Função que verifica se um Voluntario é capaz de transportar a encomenda da loja até ao cliente
     */
    public boolean dentroDeRaio(Voluntario v,Coordenadas cliente, Coordenadas loja){
        return  distanciaVoluntario(v,cliente) <= v.getRaio() &&  distanciaVoluntario(v,loja) <= v.getRaio();


    }

    /*
    Funções que mudam o estado de um Voluntario para ocupado ou desocupado e o adiciona à lista correspondente
     */
    public void ocupado(Voluntario v) throws Exception{
        if(this.VolOcupados.contains(v)) throw new Exception("Voluntario já ocupado!");

        else{
            this.VolOcupados.add(v);
            v.ocupado();
        }
    }

    public void desocupado(Voluntario v) throws Exception{
        if(this.VolDesocupados.contains(v)) throw new Exception("Voluntario já desocupado!");

        else{
            this.VolDesocupados.add(v);
            v.desocupado();
        }
    }

    /*
    tempo medio de viagem de um voluntario de transporte de encomenda
     */

    public double tempoViagemdeVoluntario(Voluntario v, Cliente cliente, Loja loja){
        return (v.getCoor().distancia(loja.getLocal()) + loja.getLocal().distancia(cliente.getCoordenadas()) / v.getVelocidadeM())
                + loja.getFilaE()*loja.getTempoMedioAtendimento();
    }



    /*----------------------------------------Tratamento de Encomendas --------------------------------------*/

    /*
    Add de Encomendas para guardar em trazAqui
     */




    /*
    Pedido de Encomenda

    Que vai adicionar a encomenda nova à lista de pending de Voluntario e respetiva Loja
     */

    public Encomenda pedidoEncomendaParaVoluntario(Cliente c,Voluntario v, Coordenadas dest, boolean especial,Loja l, List<Produto> produtos) throws ExcecaoLojaInexistente {

            Encomenda pedido = new Encomenda("v"+v.getId(),c,LocalDateTime.now(),especial,dest, 1, produtos);//TAXA A 1 SIGNIFICA QUE O VOLUNTARIO NAO GANHA NADA
            c.getRegistoEncomendasHistorico().add(pedido);
            this.addEncomendaLoja(l,pedido);
            this.addEncomendaPendingACliente(c,pedido);
            return pedido;


    }

    public Encomenda pedidoEncomendaParaEmpresa(Cliente c, EmpresaTransporte e, Coordenadas dest,boolean especial,Loja l,List<Produto> produtos) throws ExcecaoLojaInexistente {

            Encomenda pedido = new Encomenda("e"+l.getCod(),c,LocalDateTime.now(),especial,dest, e.getTaxaKm(), produtos);//TAXA A 1 SIGNIFICA QUE O VOLUNTARIO NAO GANHA NADA
            c.getRegistoEncomendasHistorico().add(pedido);
            this.addEncomendaAEmpresa(e,pedido);
            this.addEncomendaPendingACliente(c,pedido);

            return pedido;


    }


    public Produto procuraProdutoLoja(Loja loja,String idProduto) throws ExcecaoProdutoInexistente, ExcecaoLojaInexistente{

        if(this.catLojas.contains(loja))
            return this.getLojaEspecifica(loja.getCod()).getProduto(idProduto);

        else throw new ExcecaoProdutoInexistente("Produto nao existe na Loja");
    }

    public Produto procuraProdutoLoja2(String idProduto) throws ExcecaoProdutoInexistente{
        List<Loja> lojas = this.getLojas();
        Produto p = null;
        for(Loja l: lojas){
            if(l.getprodutos().contains(idProduto)) return p = l.getProduto(idProduto);
        }


        throw new ExcecaoProdutoInexistente("Produto nao existe");
    }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TrazAqui trazAqui = (TrazAqui) o;
        return Objects.equals(utilizadores, trazAqui.utilizadores) &&
                catEmpresas.equals(trazAqui.catEmpresas) &&
                catLojas.equals(trazAqui.catLojas)  &&
                Objects.equals(VolDesocupados, trazAqui.VolDesocupados) &&
                Objects.equals(VolOcupados, trazAqui.VolOcupados);
    }

    @Override
    public int hashCode() {
        return Objects.hash(VolDesocupados, VolOcupados);
    }



    /*
    Get do Historico de encomendas de voluntario e cliente para a view
     */
    public List<Encomenda> getEncomendaPending(Voluntario vol) throws ExcecaoRegistoDeEncomendasNull{
        //Voluntario cl = (Voluntario) utilizadores.get(vol.getId());
        Encomenda uma = vol.getRegistoEncomendasPending().get(0);
        List<Encomenda> unica = new ArrayList<>();
        unica.add(uma);

        if(unica == null) throw new ExcecaoRegistoDeEncomendasNull("Sem encomendas Pending");

        return unica;
    }

    public List<Encomenda> getHistoricoCliente(Cliente cliente){
        Cliente cl = (Cliente) utilizadores.get(cliente.getId());
        return cl.getRegistoEncomendasHistorico();
    }

    public List<Encomenda> getHistoricoVoluntario(Voluntario voluntario){
       Voluntario cl = (Voluntario) utilizadores.get(voluntario.getId());
        return cl.getRegistoEncomendasHistorico();
    }




    /*
    Estado e gravaçao
     */

    public void save(String fName) throws IOException {
        FileOutputStream a = new FileOutputStream(fName);
        ObjectOutputStream r = new ObjectOutputStream(a);
        r.writeObject(this);
        r.flush();
        r.close();
    }

    public static TrazAqui read(String fName) throws IOException, ClassNotFoundException {
        FileInputStream r = new FileInputStream(fName);
        ObjectInputStream a = new ObjectInputStream(r);
        TrazAqui app = (TrazAqui) a.readObject();
        a.close();
        return app;
    }


}

