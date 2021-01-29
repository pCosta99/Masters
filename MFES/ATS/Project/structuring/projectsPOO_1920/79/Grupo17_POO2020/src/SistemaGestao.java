

import java.io.*;
import java.io.Serializable;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public  class SistemaGestao implements Serializable
{

    /** conjunto de entidades do sistema */
    private Map<String,Entidade> utilizadoresSistema;

    /** conjunto de encomendas aceites pelo sistema */
    private Map<String,Encomenda> pedidosEncomenda;


    /**
     * Contrutor por omissão de Sistema de gestão
     */
    SistemaGestao(){
        this.utilizadoresSistema = new TreeMap<>();
        this.pedidosEncomenda = new TreeMap<>();
    }

    /**
     * Adicionar uma entidade ao sistema
     * @param e entidade
     */
    public void addUtilizadorSistema(Entidade e){this.utilizadoresSistema.put(e.getCodigo(),e.clone());}


    /**
     * Adicionar uma encomenda ao sistema
     * @param e encomenda
     */
    public void addEncomendaSistema(Encomenda e){this.pedidosEncomenda.put(e.getCodEncomenda(),e.clone());}


    /**
     * Devolve uma Entidade dado o seu codigo
     * @param cod codigo da entidade
     * @return Entidade
     * @throws ConjuntoVazioException
     */
    public Entidade getUtilizadorSistema(String cod) throws ConjuntoVazioException {
        if(!this.utilizadoresSistema.containsKey(cod)) throw new ConjuntoVazioException();
        else return this.utilizadoresSistema.get(cod).clone();
    }


    /**
     * Devolve uma Encomenda dado o seu codigo
     * @param codEnc codigo da encomenda
     * @return Encomenda
     * @throws ConjuntoVazioException
     */
    public Encomenda getEncomendaSistema(String codEnc) throws ConjuntoVazioException {
        if(!this.pedidosEncomenda.containsKey(codEnc)) throw new ConjuntoVazioException();
        else return this.pedidosEncomenda.get(codEnc).clone();
    }


    /**
     * Metodo toString
     * @return String
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizadores deste Sistema:\n").append(this.utilizadoresSistema).append("\n")
                .append("Encomendas deste Sistema:\n").append(this.pedidosEncomenda).append("\n");
        return sb.toString();
    }

    /**
     * Metodo que devolve uma String com as entidades
     * @return String
     */
    public String toStringEntidades(){
        StringBuilder sb = new StringBuilder();
        for(Entidade e : this.utilizadoresSistema.values()){
            sb.append(e.toStringEcra()).append(" | ");
        }
        return sb.toString();
    }

    /**
     * Metodo que devolve uma String com as encomendas
     * @return String
     */
    public String toStringEncomendas(){
        StringBuilder sb = new StringBuilder();
        for(Encomenda e : this.pedidosEncomenda.values()){
            sb.append(e.toStringEcra()).append(" | ");
        }
        return sb.toString();
    }


    /**
     * Metodo que cria/regista um utilizador
     * @param nome nome da entidade
     * @param codigo identificador da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    public void registaUtilizador(String nome,String codigo,double xGPS,double yGPS,String email, String pass){
        Utilizador u = new Utilizador(nome,codigo,xGPS,yGPS,new TreeMap<>(),email,pass);
        addUtilizadorSistema(u);
    }


    /**
     * Metodo que cria/regista uma loja
     * @param codLoja identificador da entidade
     * @param nomeLoja nome da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    public void registaLoja(String codLoja,String nomeLoja,double xGPS,double yGPS,String email, String pass){
        Loja l = new Loja(codLoja,nomeLoja,xGPS,yGPS,new TreeMap<>(),email,pass);
        addUtilizadorSistema(l);
    }


    /**
     * Metodo que cria/regista uma transportadora
     * @param codEmpresa identificador da entidade
     * @param nomeEmpresa nome da entidade
     * @param nif Numero de identificação fisca
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param taxa preço por kilometro
     * @param raio raio de circulação da entidade
     * @param capacidade capacidade
     * @param aceitaMed verifica se a entidade de entrega aceita o transport de produtos médicos
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    public void registaTransportadora(String codEmpresa,String nomeEmpresa,String nif,double xGPS,double yGPS,double taxa,double raio,int capacidade,boolean aceitaMed,String email, String pass){
        Transportadora t = new Transportadora(codEmpresa,nomeEmpresa,nif,xGPS,yGPS,taxa,raio,capacidade,aceitaMed,new TreeMap<>(),email,pass,0,0,0);
        addUtilizadorSistema(t);
    }


    /**
     * Metodo que cria/regista um voluntario
     * @param nome nome da entidade
     * @param codVoluntario identificador da entidade
     * @param xGPS coordenada x da entidade
     * @param yGPS coordenada y da entidade
     * @param raio raio de circulação da entidade
     * @param aceitaMed verifica se a entidade de entrega aceita o transport de produtos médicos
     * @param email e-mail da entidade
     * @param pass palavra-passe da entidade
     */
    public void registaVoluntario(String nome,String codVoluntario,double xGPS,double yGPS,double raio,boolean aceitaMed,String email, String pass){
        Voluntario v = new Voluntario(nome,codVoluntario,xGPS,yGPS,raio,aceitaMed,new TreeMap<>(),email,pass,0,0,0,true,new String(),new Encomenda(),new String(), LocalDateTime.of(2000,1,1,0,0),LocalDateTime.of(2000,1,1,0,0));
        addUtilizadorSistema(v);
    }



    /**
     * Metodo que verifica se as credenciais sao válidas.Se forem válidas, devolve o codigo da entidade, senao devolve "".
     * @param email e-mail da entidade
     * @param password palavra-passe da entidade
     * @return codigo da entidade ou String vazia
     */
    public String validaCredenciais(String email,String password){
        for(Entidade e : this.utilizadoresSistema.values())
            if(e.getEmail().equals(email) && e.getPass().equals(password)) return e.getCodigo();
        return new String();
    }



    /**
     * Metodo que verifica se o codigo é valido
     * @param cod codigo da encomenda
     * @return booleano
     */
    public boolean codigoValido(String cod){return this.utilizadoresSistema.containsKey(cod);}


    /**
     * Metodo que devolve o numero de entidades de um certo tipo
     * @param tipo tipo da entidade
     * @return numero de entidades de um certo tipo
     */
    public int nmrEntidadesT(String tipo){
        return (int) this.utilizadoresSistema.values().stream()
                .filter(e->e.getClass().getSimpleName().equals(tipo))
                .count();
    }


    /**
     * Metodo que devolve o nome de todas as lojas do sistema
     * @return nome de todas as lojas do sistema
     * @throws ConjuntoVazioException
     */
    public List<Par> nomesLojas() throws ConjuntoVazioException{
        if(nmrEntidadesT("Loja")<=0) throw new ConjuntoVazioException();
        else {
            List<Par> res = new ArrayList<>();
            for (Entidade e : this.utilizadoresSistema.values()) {
                if (e instanceof Loja) {
                    Par p = new Par(e.getNome(), e.getCodigo());
                    res.add(p.clone());
                }
            }
            return res;
        }
    }


    /**
     * Metodo que filtra as entidades de entrega que podem fazer uma certa encomenda
     * @param en encomenda a fazer
     * @param u utlizador associado á encomenda a fazer
     * @param l loja associada á encomenda a fazer
     * @return entidades de entrega que podem fazer uma certa encomenda
     */
    public List<Entidade> filtraTransportadoras(Encomenda en,Utilizador u,Loja l) {
        List<Entidade> res = new ArrayList<>();
        boolean prodMed = en.eEncomendaMedica();
        for (Entidade e : this.utilizadoresSistema.values()) {
            if (e instanceof EntidadeEntrega) {
                EntidadeEntrega ee = (EntidadeEntrega) e;
                if (prodMed){
                    if (ee.dentroAlcance(u.getXGPS(), u.getYGPS(), l.getXGPS(), l.getYGPS()) && ee.getEstado() && ee.aceitaMed()){
                        if (ee instanceof Voluntario) {
                            Voluntario v = (Voluntario) ee;
                            if (v.getDisposicao()) res.add(e.clone());

                        } else {
                            Transportadora t = (Transportadora) ee;
                            if(t.temEspaco()) res.add(e.clone());
                        }
                    }
                } else {
                    if (ee.dentroAlcance(u.getXGPS(), u.getYGPS(), l.getXGPS(), l.getYGPS()) && ee.getEstado()){
                        if (ee instanceof Voluntario) {
                            Voluntario v = (Voluntario) ee;
                            if (v.getDisposicao()) res.add(e.clone());

                        } else {
                            Transportadora t = (Transportadora) ee;
                            if(t.temEspaco()) res.add(e.clone());
                        }
                    }
                }
            }
        }
        return res;
    }


    /**
     * Metodo que remove um produto de uma loja dado o codigo da loja e o codigo de produto
     * @param codLoja codigo da loja
     * @param codProd codigo de produto
     * @throws CodigoInvalidoException
     */
    public void removeProd(String codLoja,String codProd) throws CodigoInvalidoException{
        Loja l = (Loja) this.utilizadoresSistema.get(codLoja);
        if(!l.getCatalogo().containsKey(codProd)) throw new CodigoInvalidoException(codProd);
        else{
            l.removeProduto(codProd);
            l.setCatalogo(l.getCatalogo());
        }
    }


    /**
     * Metodo que sinaliza que um Voluntario está ou não disposto para recolher entrega
     * @param codVol codigo do voluntario
     * @param disposicao diposição
     */
    public void volAlteraDisp(String codVol,String disposicao){
        Voluntario v = (Voluntario) this.utilizadoresSistema.get(codVol);
        v.setDisposicao(disposicao.equals("d"));
    }



    /**
     * Metodo que devolve uma String com os tempos relativos a uma encomenda de um transportador
     * @param codTransp codigo de transportador
     * @param codEnc codigo de encomenda
     * @return String
     * @throws CodigoInvalidoException
     */
    public String getTempos(String codTransp,String codEnc) throws CodigoInvalidoException{
        if(!this.utilizadoresSistema.get(codTransp).getRegistos().containsKey(codEnc))
            throw new CodigoInvalidoException(codEnc);
        else
            return this.utilizadoresSistema.get(codTransp).getRegistos().get(codEnc).imprimeTempos();
    }


    /**
     * Metodo que devolve uma String com  classificação, número total de kilometros e numero de serviços de um transportador
     * @param codTransp codigo de tranportador
     * @return
     */
    public String getParametros(String codTransp){
        EntidadeEntrega ee = (EntidadeEntrega) this.utilizadoresSistema.get(codTransp);
        return ee.imprimeClassKmServicos();
    }


    /**
     * Metodo que determina a listagem dos 10 utilizadores que mais utilizam o sistema (em numero de encomendas feitas)
     * @return lista dos 10 utilizadores que mais utilizam o sistema
     * @throws ConjuntoVazioException
     */
    public List<String> utilizadoresMaisUsados() throws ConjuntoVazioException{
        if(nmrEntidadesT("Utilizador")<=0) throw new ConjuntoVazioException();
        else {
            Set<Utilizador> ts = new TreeSet<>(new ComparatorNumeroEncUti());
            for(Entidade a : this.utilizadoresSistema.values())
                if(a instanceof Utilizador){
                    Utilizador e = (Utilizador) a;
                    ts.add(e.clone());
                }
            return ts.stream().limit(10).map(Entidade::getCodigo).collect(Collectors.toList());
        }
    }


    /**
     * Metodo que determina a listagem das 10 empresas transportadoras que mais utilizam o sistema (em kilometros realizados)
     * @return lista das 10 empresas transportadoras que mais utilizam o sistema
     * @throws ConjuntoVazioException
     */
    public List<String> transportadorasMaisUsados() throws ConjuntoVazioException {
        if (nmrEntidadesT("Transportadora") <= 0) throw new ConjuntoVazioException();
        else {
            Set<Transportadora> ts = new TreeSet<>(new ComparatorNumeroKmTrans());
            for (Entidade a : this.utilizadoresSistema.values())
                if (a instanceof Transportadora) {
                    Transportadora e = (Transportadora) a;
                    ts.add(e.clone());
                }
            return ts.stream().limit(10).map(Entidade::getCodigo).collect(Collectors.toList());
        }
    }


    /**
     * Metodo que devolve os codigos de todas as encomendas feitas ate ao momento por um transportador
     * @param codTransp codigo de transportador
     * @return codigos de todas as encomendas feitas ate ao momento por um transportador
     * @throws ConjuntoVazioException
     */
    public Set<String> codEncs(String codTransp) throws ConjuntoVazioException{
        EntidadeEntrega ee = (EntidadeEntrega) getUtilizadorSistema(codTransp);
        if(ee.getRegistos().size()<=0) throw new ConjuntoVazioException();
        else
            return ee.getRegistos().keySet();
    }


    /**
     * Metodo que devolve o total faturado por uma empresa transportadora num determinado periodo
     * @param inicio limite inferior da data e hora
     * @param fim limite superior da data e hora
     * @param codTransp codigo da transportadora
     * @return total faturado por uma empresa transportadora num determinado periodo
     * @throws DateTimeException
     */
    public double faturacaoTransp(LocalDateTime inicio,LocalDateTime fim,String codTransp) throws DateTimeException {
        double res = 0;
        Transportadora t = (Transportadora) this.utilizadoresSistema.get(codTransp);
        List<Encomenda> l = t.getRegistos().values().stream()
                .filter(a-> a.getInicio().isAfter(inicio) && a.getFim().isBefore(fim))
                .collect(Collectors.toList());

        for(Encomenda e : l) {
            if (e.getPesoEnc() <= 1) res += t.getTaxa() * e.getKmDeslocacao();
            else res += t.getTaxa() * e.getPesoEnc() * e.getKmDeslocacao();
        }

        return res;
    }


    /**
     * Metodo que retorna uma String com as coordenadas da loja e utilizador de uma encomenda
     * @param codUti codigo de utilizador
     * @param codLoja codigo de loja
     * @return String
     */
    public String imprimeCoord(String codUti,String codLoja){
        Utilizador u = (Utilizador) this.utilizadoresSistema.get(codUti);
        Loja l = (Loja) this.utilizadoresSistema.get(codLoja);
        StringBuilder s = new StringBuilder();
        double coordXU = u.getXGPS();
        double coordYU = u.getYGPS();
        double coordXL = l.getXGPS();
        double coordYL = l.getYGPS();
        s.append("Coordenadas do destinatario: x->").append(coordXU).append(" , y->").append(coordYU).append("\nCoordenadas da loja: x->").append(coordXL).append(" , y->").append(coordYL).append("\n");
        return s.toString();
    }


    /**
     * Metodo que insere um conjunto de entidades a este sistema.
     * @param entidades entidades a serem inseridas no sistema
     */
    public void insereEntidades(Map<String,Entidade> entidades){
        for(Entidade e : entidades.values()) this.utilizadoresSistema.put(e.getCodigo(),e.clone());
    }


    /**
     * Metodo que insere um conjunto de encomendas a este sistema.
     * @param encomendas encomendas a serem inseridas no sistema
     */
    public void insereEncomendas(Map<String,Encomenda> encomendas){
        for(Encomenda e :encomendas.values()) this.pedidosEncomenda.put(e.getCodEncomenda(),e.clone());
    }


    /**
     * Metodo que lê o ficheiro logs e introduz as entidades no sistema de gestão
     * @param fileName nome do ficheiro
     * @throws IOException
     * @throws FileNotFoundException
     */
    public void lerCSV(String fileName) throws IOException,FileNotFoundException {
        Parse p = new Parse();
        p.parse(fileName);
        insereEntidades(p.getEntidades());
        insereEncomendas(p.getEncomendas());
        for(String codEnc : p.getEncomendasAceites()) {
            Encomenda e = p.getEncomendas().get(codEnc);
            Utilizador u = (Utilizador) this.utilizadoresSistema.get(e.getDestinatario());
            u.insereEncAceitesLogs(e);
        }
        for(Encomenda enc : p.getEncomendas().values()){
            Loja l = (Loja) this.utilizadoresSistema.get(enc.getVendedor());
            Utilizador u = (Utilizador) this.utilizadoresSistema.get(enc.getDestinatario());
            l.insereProdutosCat(enc.getProdutos());
            if(p.encEntregue(enc)){
                l.insereEnc(enc);
                u.insereEnc(enc);
            }
        }
    }

    /**
     * Metodo que guarda o estado atual da aplicação em binário
     * @param file nome do ficheiro
     * @throws IOException
     */
    public void guardaEstado(String file) throws IOException {
        FileOutputStream fos = new FileOutputStream(file);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    /**
     * Metodo que lê o estado atual da aplicação em binário
     * @param file nome do ficheiro
     * @return sistema atualizado depois da leitura
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public SistemaGestao carregaEstado(String file) throws IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(file);
        ObjectInputStream ois = new ObjectInputStream(fis);
        SistemaGestao sg = (SistemaGestao) ois.readObject();
        ois.close();
        return sg;
    }
}