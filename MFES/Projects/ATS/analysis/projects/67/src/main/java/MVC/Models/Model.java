package MVC.Models;

import MVC.Models.BaseModels.*;
import MVC.Models.Catalogs.*;
import MVC.Comparators.MaisKmsComparator;
import MVC.Comparators.NumeroVendasComparator;
import MVC.Exceptions.InvalidInputException;
import MVC.Exceptions.NaoExisteException;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class Model implements Serializable {
    private Encomendas encomendas;
    private Lojas lojas;
    private Transportadoras transportadoras;
    private Utilizadores utilizadores;
    private Voluntarios voluntarios;

    /**
     * Construtor Model por defeito.
     */
    public Model(){
        this.encomendas = new Encomendas();
        this.lojas = new Lojas();
        this.transportadoras = new Transportadoras();
        this.utilizadores = new Utilizadores();
        this.voluntarios = new Voluntarios();
    }

    /**
     * Método que carrega todos as dados do Ficheiro de Logs para as Variáveis de Instância.
     */
    public void carregaLog() throws NaoExisteException {
        this.encomendas = new Encomendas();
        this.lojas = new Lojas();
        this.transportadoras = new Transportadoras();
        this.utilizadores = new Utilizadores();
        this.voluntarios = new Voluntarios();
        Parse p = new Parse();
        List<String> encAceites = new ArrayList<>();
        p.parse(this.utilizadores,this.voluntarios,this.transportadoras,this.lojas,this.encomendas,encAceites);
        for(String s : encAceites){
            boolean r = this.entregaEncomenda(s);
            if(r) {
                Encomenda e = this.encomendas.getEncomenda(s);
                String codEnt = e.getCodEntregador();
                String codU = e.getCodUser();
                String cod = e.getCodEnc();
                if (codEnt.charAt(0)=='v')
                    this.utilizadorAceitaEncomenda(codU,cod,true);
            }
        }
    }


    /**
     * Método que cria uma LinhaEncomenda a partir de um Produto existente numa Loja.
     * @param loja Código da Loja.
     * @param prod Código do Produto.
     * @param quant Quantidade do Produto.
     * @return LinhaEncomenda resultante.
     */
    public LinhaEncomenda criaLinha(String loja, String prod,double quant) throws NaoExisteException, InvalidInputException {
        if(this.lojas.getLoja(loja).existeProduto(prod) && (quant >= 0)){
            Produto p = this.lojas.getProdutoLoja(prod,loja);
            return new LinhaEncomenda(p.getCod(),p.getNome(),quant,quant*p.getPrecoPorQuant());
        }
        else if (quant >= 0){
            throw new NaoExisteException(prod);
        }
        else {
            throw new InvalidInputException(String.valueOf(quant));
        }
    }

    /**
     * Método que cria uma Encomenda e devolve o Código da Entregadora da Encomenda.
     * @param codU Código do Utilizador.
     * @param codL Código da Loja.
     * @param le Lista de LinhaEncomenda.
     * @return Código de Encomenda
     */
    public String criaEncomenda(String codU,String codL,List<LinhaEncomenda> le) throws NaoExisteException {
        boolean r = true;
        Random rand = new Random();
        String cod = "";
        for (int i=1;i<10000 && r;i++){
            cod = "e"+i;
            if (!this.encomendas.existsEncomenda(cod))
                r = false;
        }
        if (!r) {
            Encomenda e = new Encomenda(cod, codU, codL, rand.nextDouble() + rand.nextInt(99), le);
            e.setMedica(this.lojas.isEncomendaMed(le, codL));
            this.encomendas.addEncomenda(e);
            r = this.entregaEncomenda(cod);
            if (r) {
                String codEnt = this.encomendas.getEncomenda(cod).getCodEntregador();
                if (codEnt.charAt(0) == 'v')
                    this.utilizadorAceitaEncomenda(codU, cod, true);
                return codEnt;
            }
        }
        return "";
    }

    /**
     * Método que realiza a Entrega da Encomenda.
     * @param cod Código da Encomenda.
     * @return True caso seja entregue com sucesso, false caso contrário.
     */
    private boolean entregaEncomenda(String cod){
        Encomenda e = this.encomendas.getEncomenda(cod);
        Encomenda ne;
        String codLoja = e.getCodLoja();
        Loja l = this.lojas.getLoja(codLoja);
        GPS gpsLoja = l.getGPS();
        double duracaoLoja =  l.getDuracaoFila();
        ne = this.voluntarios.entregaEncomendaVoluntario(e,gpsLoja,duracaoLoja);
        if(!ne.getCodEntregador().equals("")) {
            this.utilizadores.addEncomendaUtilizador(e.getCodEnc(), e.getCodUser());
            this.encomendas.addEncomenda(ne);
            return true;
        }
        ne = this.transportadoras.entregaEncomendaTransportadora(e,gpsLoja,duracaoLoja);
        if(!ne.getCodEntregador().equals("")) {
            this.utilizadores.addEncomendaUtilizador(e.getCodEnc(), e.getCodUser());
            this.encomendas.addEncomenda(ne);
            return true;
        }
        this.encomendas.removeEncomenda(cod);
        return false;
    }


    /**
     * Método em que um Utilizador aceita uma Encomenda.
     * @param codU Código do Utilizador.
     * @param codE Código da Encomenda
     * @param b True caso aceita a Encomenda, false caso contrário.
     * @return True caso exista um Utilizador que realize a encomenda, false caso contrário.
     */
    public boolean utilizadorAceitaEncomenda(String codU,String codE,boolean b) throws NaoExisteException{
        if(this.encomendas.existsEncomenda(codE)){
            Encomenda e = this.encomendas.getEncomenda(codE);
            boolean userEnvolvidoNaEncomenda = false;
            if(e.getCodUser().equals(codU)) {
                userEnvolvidoNaEncomenda = true;
                this.utilizadores.aceitaEncomendaUtilizador(codU, codE, b);
                if (b) {
                    this.lojas.addEncomendaLoja(e.getCodEnc(), e.getCodLoja());
                    String codEntrega = e.getCodEntregador();
                    if (codEntrega.charAt(0) == 'v') {
                        this.voluntarios.setEstaLivreVoluntario(codEntrega, false); // Voluntario vai entregar a encomenda.
                        this.voluntarios.addEncomendaVoluntario(codE, codEntrega); // Voluntario está a entregar a encomenda.
                        this.voluntarios.setEstaLivreVoluntario(codEntrega, true); // Voluntario entregou encomenda.
                    }
                    if (codEntrega.charAt(0) == 't') {
                        this.transportadoras.decTransportadora(codEntrega); // Transportadora vai entregar a encomenda.
                        this.transportadoras.addEncomendaTransportadora(codE, codEntrega, e.getDistancia()); // Transportadora está a entregar a encomenda.
                        this.transportadoras.incTransportadora(codEntrega); // Transportadora entregou a encomenda
                    }
                }
            }
            return userEnvolvidoNaEncomenda;
        }
        else
            throw new NaoExisteException(codE);
    }


    /**
     * Método em que um Utilizador classifica uma Encomenda.
     * @param codU Código do Utilizador.
     * @param codE Código da Encomenda.
     * @param nota Classificação dada.
     * @return True caso exista um Utilizar que possa classificar a Encomenda, false caso contrário.
     */

    public boolean utilizadorClassificaEncomenda(String codU, String codE, int nota) throws NaoExisteException{
            if(this.encomendas.existsEncomenda(codE)) {
                this.encomendas.classificaEncomenda(codE, nota);
                Encomenda e = this.encomendas.getEncomenda(codE);
                boolean userEnvolvidoNaEncomenda = false;
                if (e.getCodUser().equals(codU)) {
                    userEnvolvidoNaEncomenda = true;
                    this.utilizadores.classificaEncomenda(codU, codE);
                    String codEntrega = e.getCodEntregador();
                    if (codEntrega.charAt(0) == 'v')
                        this.voluntarios.classificaVoluntario(codEntrega, nota);
                    else {
                        if (codEntrega.charAt(0) == 't')
                            this.transportadoras.classificaTransportadora(codEntrega, nota);
                    }
                }
                return userEnvolvidoNaEncomenda;
            }
            else
                throw new NaoExisteException(codE);
    }

    /**
     * Método que verifica se o Login é válido.
     * @param cod Código do Utilizador.
     * @param senha Password.
     * @return True caso seja válido, false caso contrário.
     */
    public boolean loginValido(String cod, String senha){
        if(cod.charAt(0) == 'u'){
            if(this.utilizadores.existeUtilizador(cod))
                return this.utilizadores.getUtilizador(cod).getPass().equals(senha);
        }
        if(cod.charAt(0) == 'v'){
            if(this.voluntarios.existeVoluntario(cod))
                return this.voluntarios.getVoluntario(cod).getPass().equals(senha);
        }
        if(cod.charAt(0) == 't'){
            if(this.transportadoras.existeTransportadora(cod))
                return this.transportadoras.getTransportadora(cod).getPass().equals(senha);
        }
        if(cod.charAt(0) == 'l'){
            if(this.lojas.existeLoja(cod))
                return this.lojas.getLoja(cod).getPass().equals(senha);
        }
        return false;
    }

    /**
     * Método que devolve todas as Encomendas realizadas por um Entregador.
     * @param s Código do Entregador.
     * @return Lista resultante.
     */
    public List<Encomenda> getEncomendasEntregador(String s) {
        if(s.charAt(0)=='v')
            return this.encomendas.getListaEncomendas(this.voluntarios.getVoluntario(s).getCodencomendas());
        else
            return this.encomendas.getListaEncomendas(this.transportadoras.getTransportadora(s).getCodencomendas());
    }

    /**
     * Método que devolve uma Lista com todas as Lojas Existentes.
     * @return Lista resultante.
     */
    public List<Loja> getListaLojas(){
        return this.lojas.getListaLojas();
    }

    /**
     * Método que devolve uma Lista com todos os Produtos de uma determinada Loja.
     * @param l Código da Loja.
     * @return Lista resultante.
     */
    public List<Produto> getProdutosLoja(String l) throws NaoExisteException{
        if (this.lojas.existeLoja(l))
            return this.lojas.getLoja(l).getListaProdutos();
        else
            throw new NaoExisteException(l);
    }

    /**
     * Método que define se um Entregador está livre ou não.
     * @param s Código do Entregador.
     * @param b True caso esteja livre, false caso contrário.s
     */
    public void setEstaLivreEntregador(String s, Boolean b){
        if(s.charAt(0)=='v')
            this.voluntarios.setEstaLivreVoluntario(s,b);
        if(s.charAt(0)=='t')
            this.transportadoras.setEstaLivreTransportadora(s,b);
    }

    /**
     * Método que devolve o Utilizador a partir do Código.
     * @param cod Código do Utilizador.
     * @return Utilizador resultante.
     */
    public Utilizador getUtilizador(String cod){
        return this.utilizadores.getUtilizador(cod);
    }


    /**
     * Método que cria e adiciona um Utilizador ao Catálogo de Utilizador e devolve o Código do mesmo.
     * @param nome Nome do Utilizador.
     * @param lat Coordenada X da Localização do Utilizador.
     * @param lon Coordenada Y do Localização do Utilizador.
     * @return Código do Utilizador criado.
     */
   public String addUtilizador(String nome,double lat, double lon){
       String cod = "";
        boolean r = true;
        for (int i=1;i<100 && r;i++){
            cod = "u"+i;
            if(!this.utilizadores.existeUtilizador(cod)){
                r=false;
                this.utilizadores.addUtilizador(new Utilizador(cod,nome,lat,lon));
            }
        }
        return cod;
   }

    /**
     * Método que cria e adiciona um Voluntario ao Catálogo de Voluntario e devolve o Código do mesmo.
     * @param nome Nome do Voluntario.
     * @param lat Coordenada X da Localização do Voluntario.
     * @param lon Coordenada Y do Localização do Voluntario.
     * @param raio Raio de Entrega.
     * @param b True caso possa realizar Encomendas Médicas, false caso contrário.
     * @return Código do Voluntario criado.
     */
    public String addVoluntario(String nome,double lat, double lon,double raio,boolean b){
        String cod = "";
        boolean r = true;
        for (int i=1;i<100 && r;i++){
            cod = "v"+i;
            if(!this.voluntarios.existeVoluntario(cod)){
                r=false;
                if(!b)
                    this.voluntarios.addVoluntario(new Voluntario(cod,nome,lat,lon,raio));
                else
                    this.voluntarios.addVoluntario(new VoluntarioMed(cod,nome,lat,lon,raio,true));
            }
        }
        return cod;
    }

    /**
     * Método que cria e adiciona uma Transportadora ao Catálogo de Transportadora e devolve o Código da mesma.
     * @param nome Nome da Transportadora.
     * @param lat Coordenada X da Localização do Transportadora.
     * @param lon Coordenada Y do Localização do Transportadora.
     * @param nif NIF da Transportadora.
     * @param raio Raio de Entrega.
     * @param p Preço por Km da Transportadora.
     * @param capacidade Capacidade de encomendas da Transportadora
     * @param b True caso possa realizar Encomendas Médicas, false caso contrário.
     * @return Código da Transportadora criada.
     */
    public String  addTransportadora(String nome,double lat, double lon,String nif,double raio,double p,int capacidade,boolean b){
        String cod = "";
        boolean r = true;
        for (int i=1;i<100 && r;i++){
            cod = "t"+i;
            if(!this.transportadoras.existeTransportadora(cod)){
                r=false;
                if(!b)
                    this.transportadoras.addTransportadora(new Transportadora(cod,nome,lat,lon,nif,raio,p,capacidade));
                else
                    this.transportadoras.addTransportadora(new TransportadoraMed(cod,nome,lat,lon,nif,raio,p,true, capacidade));
            }
        }
        return cod;
    }

    /**
     * Método que cria e adiciona uma Loja ao Catálogo de Loja e devolve o Código da mesma.
     * @param nome Nome da Loja.
     * @param lat Coordenada X da Localização da Loja.
     * @param lon Coordenada Y da Localização da Loja.
     * @return Código da Loja criada.
     */
    public String addLoja(String nome,double lat, double lon){
        String cod = "";
        boolean r = true;
        for (int i=1;i<100 && r;i++){
            cod = "l"+i;
            if(!this.lojas.existeLoja(cod)){
                r=false;
                this.lojas.addLoja(new Loja(cod,nome,lat,lon));
            }
        }
        return cod;
    }

    /**
     * Método que grava o Estado do Programa para o Ficheiro default.
     * @throws IOException Exception necessária.
     */
    public void gravaEstado() throws IOException {
        gravaEstado("dados/a.dat");
    }

    /**
     * Método que grava o Estado do Programa para um Ficheiro à escolha do User.
     * @param filename Caminho para o Ficheiro.
     * @throws IOException Exception necessária.
     */
    public void gravaEstado(String filename) throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(filename));
        o.writeObject(this);
        o.flush();
        o.close();
    }

    /**
     * Método que Clona um Model para dentro do Model.
     * @param g Model a Copiar.
     */
    private void cloneToSelf(Model g){
        this.voluntarios = g.voluntarios;
        this.utilizadores = g.utilizadores;
        this.lojas = g.lojas;
        this.transportadoras = g.transportadoras;
        this.encomendas = g.encomendas;
    }

    /**
     * Método que carrega o Estado do Programa de um Ficheiro default.
     * @throws IOException Exception necessária.
     * @throws ClassNotFoundException Exception necessária.
     */
    public void carregaEstado() throws IOException, ClassNotFoundException {
        carregaEstado("dados/a.dat");
    }

    /**
     * Método que carrega o Estado do Programa de um Ficheiro à escolha do User.
     * @param filename Caminho para o Ficheiro.
     * @throws IOException Exception necessária.
     * @throws ClassNotFoundException Exception necessária.
     */
    public void carregaEstado(String filename) throws IOException, ClassNotFoundException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream(filename));
        this.cloneToSelf((Model) o.readObject());
        o.close();
    }

    /**
     * Método que devolve todas as Encomendas por aceitar de um Utilizador.
     * @param codU Código do Utilizador.
     * @return Lista resultante.
     */
    public List<Encomenda> getListaEncomendasPorAceitar(String codU){
        return this.encomendas.getListaEncomendas(this.utilizadores.getUtilizador(codU).getPorAceitar());
    }

    /**
     * Método que devolve todas as Encomendas por classficicar de um Utilizador.
     * @param codU Código do Utilizador.
     * @return Lista resultante.
     */
    public List<Encomenda> getListaEncomendasPorClassificar(String codU){
        return this.encomendas.getListaEncomendas(this.utilizadores.getUtilizador(codU).getPorClassificar());
    }

    /**
     * Método que devolve todas as Encomendas já classificadas de um Utilizador.
     * @param codU Código do Utilizador.
     * @return Lista resultante.
     */
    public List<Encomenda> getListaEncomendasClassificadas(String codU){
        return this.encomendas.getListaEncomendas(this.utilizadores.getUtilizador(codU).getCodencomendas());
    }

    /**
     * Método que cria e adiciona um Produto a uma determinada Loja.
     * @param codL Código da Loja.
     * @param prod Descrição do Produto.
     * @param preco Preço do Produto.
     * @param isMedic True se o produto for uma Encomenda médica, false caso contrário.
     * @return True caso seja criado o Produto, false caso contrário.
     */
    public boolean addProduto(String codL,String prod,double preco,boolean isMedic){
        Loja l = this.lojas.getLoja(codL);
        String cod = "";
        boolean r = false;
        for (int i=1;i<100 && !r;i++){
            cod = "p"+i;
            if(!l.existeProduto(cod)){
                r=true;
                Produto p = new Produto(cod,prod,preco);
                p.setMedicamento(isMedic);
                this.lojas.addProdutoLoja(p,codL);
            }
        }
        return r;
    }

    /**
     * Método que retorna o Top 10 Utilizadores do TrazAqui.
     * @return Lista Ordenada dos Utilizadores.
     */
    public List<Utilizador> getTop10Utilizadores(){
        Set<Utilizador> s = new TreeSet<>(new NumeroVendasComparator());
        Map<String,Utilizador> us = this.utilizadores.getDataUtilizadores();
        s.addAll(us.values());
        int max = Math.min(10,s.size());
        return s.stream().collect(Collectors.toList()).subList(0,max);
    }

    /**
     * Método que retorna o Top 10 Transportadoras do TrazAqui.
     * @return Lista Ordenada das Transportadoras.
     */
    public List<Transportadora> getTop10Transportadoras(){
        Set<Transportadora> s = new TreeSet<>(new MaisKmsComparator());
        Map<String,Transportadora> ts = this.transportadoras.getTransportadoras();
        s.addAll(ts.values());
        int max = Math.min(10,s.size());
        return s.stream().collect(Collectors.toList()).subList(0,max);
    }

    /**
     * Método que devolve a Faturação Total de uma determinada Transportadora.
     * @param codTrans Código da Transportadora.
     * @return Faturação Total.
     */
    public double getFaturacao(String codTrans){
        List<String> codEncomendas = this.transportadoras.getTransportadora(codTrans).getCodencomendas();
        List<Encomenda> listEncomendas = this.encomendas.getListaEncomendas(codEncomendas);
        double dinheiro=0;
        for (Encomenda e : listEncomendas){
            dinheiro += e.getPreco();
        }
        return dinheiro;
    }

    /**
     * Método que verifica se existe um Entregador com o Código dado.
     * @param s Código do Entregador.
     * @return True caso exista, false caso contrário.
     */
    public boolean existeEntregador(String s){
        return this.transportadoras.existeTransportadora(s) || this.voluntarios.existeVoluntario(s);
    }

}
