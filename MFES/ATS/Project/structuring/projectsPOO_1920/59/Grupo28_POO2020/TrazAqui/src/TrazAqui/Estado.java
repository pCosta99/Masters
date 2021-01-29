package TrazAqui;

import java.io.IOException;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Classe que aglomera todas as informações do sistema ( utilizadores, estafetas, lojas e quem é o utilizador que está a usar o programa).
 */
public class  Estado implements Serializable {
    private HashMap<String,Utilizador> utilizadores;
    private HashMap<String,Estafeta> trabalhadores;
    private HashMap<String,Loja> lojas;
    private Entrada login;

    /**
     * Construtor vazio de Estado.
     */
    public Estado() {
        this.utilizadores = new HashMap<>();
        this.lojas = new HashMap<>();
        this.trabalhadores = new HashMap<>();
        this.login = null;
    }

    /**
     * Construtor parametrizado do Estado.
     * @param u Map de todos os utilizadores.
     * @param t Map de todos os estafetas.
     * @param l Map de todas as lojas.
     * @param a Informação do utilizador que está a usar o programa.
     */
    public Estado(HashMap<String,Utilizador> u,HashMap<String,Estafeta> t,HashMap<String,Loja> l, Entrada a) {
        this.setLojas(l);
        this.setTrabalhadores(t);
        this.setUtilizadores(u);
        this.login = a.clone();
    }

    /**
     * Construtor por cópia do Estado.
     * @param e Estado que pretendemos copiar.
     */
    public Estado(Estado e) {
        this.setLojas(e.getLojas());
        this.setUtilizadores(e.getUtilizadores());
        this.login = e.getLogin();
        this.setTrabalhadores(e.getTrabalhadores());
    }

    /**
     * Método que clona um estado.
     * @return Estado clonado.
     */
    public Estado clone() {
        return new Estado(this);
    }

    /**
     * Método que compara 2 estados.
     * @param o Estado a comparar.
     * @return Booleano que indica se os elementos comparados são iguais ou não.
     */
    public boolean equals(Object o) {
        if (this==o) return true;
        if (o == null || !this.getClass().equals(o.getClass())) return false;

        Estado e = (Estado) o;

        return this.lojas.equals(e.getLojas()) &&
                this.trabalhadores.equals(e.getTrabalhadores()) &&
                this.utilizadores.equals(e.getUtilizadores());
    }

    /**
     * Converte um Estado para um string.
     * @return Estado convertido numa string.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Estado{");
        sb.append("utilizadores=").append(utilizadores);
        sb.append(", trabalhadores=").append(trabalhadores);
        sb.append(", lojas=").append(lojas);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Getter do utilizador que está a usar o sistema.
     * @return Utilizador que está a usar o sistema.
     */
    public Entrada getLogin() {
        return this.login == null ? null : this.login.clone();
    }

    /**
     * Setter do utilizador que está a usar o sistema.
     * @param login Utilizador.
     */
    public void setLogin(Entrada login) {
        this.login = login.clone();
    }

    /**
     * Getter dos estafetas do sistema.
     * @return Estafetas do sistema.
     */
    public HashMap<String, Estafeta> getTrabalhadores() {
        HashMap<String,Estafeta> res = new HashMap<>();

        for(Map.Entry<String,Estafeta> aux : this.trabalhadores.entrySet()) {
            res.put(aux.getKey(),aux.getValue());
        }
        return res;
    }

    /**
     * Setter dos estafetas do sistema.
     * @param trabalhadores Estafetas que pretendemos adicionar ao sistema.
     */
    public void setTrabalhadores(HashMap<String, Estafeta> trabalhadores) {
        this.trabalhadores = new HashMap<>();

        for (Map.Entry<String,Estafeta> aux : trabalhadores.entrySet()) {
            this.trabalhadores.put(aux.getKey(),aux.getValue());
        }
    }

    /**
     * Adiciona um estafeta ao sistema.
     * @param e Estafeta que vamos adicionar.
     * @throws ExistingCodeException Caso já exista o estafeta no sistema, temos uma exception.
     */
    public void addTrabalhador(Estafeta e) throws ExistingCodeException {
        if(this.trabalhadores.putIfAbsent(e.getCod(),e.clone()) != null)
            throw new ExistingCodeException("Código inválido!");
    }

    /**
     * Adiciona um utilizador ao sistema.
     * @param u Utilizador que vamos adicionar.
     * @throws ExistingCodeException Caso já exista o utilizador no sistema, temos uma exception.
     */
    public void addUtilizador(Utilizador u) throws ExistingCodeException {
        if(this.utilizadores.putIfAbsent(u.getCod(),u.clone())!=null)
            throw new ExistingCodeException("Código inválido!");
    }

    /**
     * Adiciona uma loja ao sistema.
     * @param l Loja que vamos adicionar ao sistema.
     * @throws ExistingCodeException Caso já exista a loja no sistema, temos uma exception.
     */
    public void addLoja(Loja l) throws ExistingCodeException {
        if(this.lojas.putIfAbsent(l.getCod(),l.clone()) != null)
            throw new ExistingCodeException("Código inválido!");
    }

    /**
     * Getter dos utilizadores dos sistemas.
     * @return Utilizadores do sistema.
     */
    public HashMap<String, Utilizador> getUtilizadores() {
        HashMap<String,Utilizador> res = new HashMap<>();
        for (Map.Entry<String,Utilizador> u : this.utilizadores.entrySet()) {
            res.put(u.getKey(),u.getValue().clone());
        }
        return res;
    }

    /**
     * Setter dos utilizadores do sistema.
     * @param utilizadores Utilizadores do sistema.
     */
    public void setUtilizadores(HashMap<String, Utilizador> utilizadores) {
        this.utilizadores = new HashMap<>();
        for (Map.Entry<String,Utilizador> u : utilizadores.entrySet()) {
            this.utilizadores.put(u.getKey(),u.getValue().clone());
        }
    }

    /**
     * Getter de lojas do sistema.
     * @return Lojas do sistema.
     */
    public HashMap<String, Loja> getLojas() {
        HashMap<String,Loja> res = new HashMap<>();
        for (Map.Entry<String,Loja> l : this.lojas.entrySet()) {
            res.put(l.getKey(),l.getValue().clone());
        }
        return res;
    }

    /**
     * Setter de lojas no sistema.
     * @param lojas Lojas que vamos adicionar ao sistema.
     */
    public void setLojas(HashMap<String, Loja> lojas) {
        this.lojas = new HashMap<>();
        for (Map.Entry<String,Loja> l : lojas.entrySet()) {
            this.lojas.put(l.getKey(),l.getValue().clone());
        }
    }

    /**
     * Getter de posição geográfica de um utilizador, estafeta ou loja.
     * @param user Código do utilizador, estafeta ou loja.
     * @return Posição geográfica.
     */
    public GPS getUserPos(String user) {
        return this.utilizadores.get(user).getLocalizacao();
    }

    /**
     * Remove uma encomenda de uma loja.
     * @param codEnc Código de uma encomenda.
     * @param cod Código estafeta.
     * @return Retorna null caso haja um erro, uma encomenda caso não haja erro.
     */
    public Encomenda removeEncomendaLoja(String codEnc,String cod) {
        boolean medicamentos = false;
        medicamentos = this.trabalhadores.get(cod).aceitoTransportesMedicamentos();
        for (Loja l : this.lojas.values()) {
            for (Encomenda e : l.getPedidos()) {
                if (e.getCod().equals(codEnc)) {
                    if (e.getMedicamentos() && medicamentos) {
                        this.lojas.get(l.getCod()).removePedido(codEnc);
                        return new Encomenda(e);
                    } else if (!e.getMedicamentos()) {
                        this.lojas.get(l.getCod()).removePedido(codEnc);
                        return new Encomenda(e);
                    }
                }
            }
        }
        return null;
    }
    /**
     * Remove uma encomenda de uma transportadora
     * @param codEnc Código de encomendas
     * @param cod Código da transportadora
     */
    public void removeEncomendaTransportadora(String codEnc, String cod) {
        this.trabalhadores.get(cod).removerEncomenda(codEnc);
    }

    /**
     * Adiciona um pedido de transporte
     * @param cod código do estafeta
     * @param e Encomenda que vamos adicionar aos pedidos
     */
    public void addPedidoDeTransporte(String cod,Encomenda e) {
        this.trabalhadores.get(cod).addPedidosEncomenda(e);
    }

    /**
     * Adiciona uma encomenda à lista de encomendas entregues.
     * @param cod Código do estafeta.
     * @param e Encomenda que vamos adicionar.
     */
    public void addEncomendaEntregue(String cod, Encomenda e) {
        this.trabalhadores.get(cod).addEncomendaEntregue(e);
    }

    /**
     * Calculador do preço de uma encomenda.
     * @param enc Encomenda que vamos calcular o peso.
     * @param trans Código do transportador.
     * @param loja Loja onde a encomenda foi pedida.
     * @return Preço final da encomenda.
     */
    public double calculaPreco(Encomenda enc,String trans,String loja) {
        Loja l = this.lojas.get(loja);
        Transportadora t = (Transportadora) this.trabalhadores.get(trans);
        double taxa = 0;

        if (l instanceof LojaFilaEspera) {
            LojaFilaEspera ljfe = (LojaFilaEspera) l;
            taxa = ljfe.getTempoEspera()*ljfe.getListaEspera()*0.30;
        }

        double dist = l.getLocalizacao().distancia(t.getLocalizacao());
        double preco = t.precoEncomenda(enc.getPeso(),dist);
        return (preco+preco*taxa);
    }

    /**
     * Calculador do faturado num intervalo de tempo.
     * @param cod Código do estafeta.
     * @param min Data inicial do intervalo de tempo.
     * @param max Data final do intervalo de tempo.
     * @return Total faturado.
     */
    public double totalFaturado(String cod, LocalDateTime min, LocalDateTime max) {
        double total=0;
        for (Encomenda e : this.trabalhadores.get(cod).getEncomendasEntregues()) {
            if (e.getData().isAfter(min) && e.getData().isBefore(max)) {
                    total += calculaPreco(e,cod,e.getLoja());
            }
        }
        return total;
    }

    /**
     * Função que nos dá o top10 utilizadores com base no número de pedidos.
     * @return Os 10 maiores utilizadores no sistema.
     */
    public List<Utilizador> getTop10Util() {
        Comparator<Integer> comp = Integer::compareTo;
        TreeMap<Integer,Set<Utilizador>> vezes = new TreeMap<>(comp);
        List<Utilizador> res = new ArrayList<>();
        int cont=0;
        for (Map.Entry<String,Utilizador> aux : this.utilizadores.entrySet()) {
            int numPedidos = aux.getValue().getEncomendasConcluidas().size();
            vezes.putIfAbsent(numPedidos,new TreeSet<>(Comparator.comparing(Utilizador::getNome)));
            vezes.get(numPedidos).add(aux.getValue());
        }
        for (Map.Entry<Integer,Set<Utilizador>> aux : vezes.entrySet()) {
            if (cont==10) break;
            for (Utilizador u : aux.getValue()) {
                res.add(u.clone());
                cont++;
                if(cont == 10) break;
            }
        }
        return res;
    }

    /**
     * Método dos top 10 transportadores com base no número de kilometros percorridos por cada transportador.
     * @return Top 10 transportadoras com base no número de kilometros percorridos.
     */
    public List<Transportadora> getTop10Trans() {
        Comparator<Double> comp = Double::compareTo;
        TreeMap<Double,List<Estafeta>> vezes = new TreeMap<>(comp);
        List<Transportadora> res = new ArrayList<>();
        int cont=0;
        for (Map.Entry<String,Estafeta> aux : this.trabalhadores.entrySet()) {
            if (aux.getValue() instanceof Transportadora) {
                Transportadora t = (Transportadora) aux.getValue();
                double numKms = t.getNumKms();
                vezes.putIfAbsent(numKms,new ArrayList<>());
                vezes.get(numKms).add(t);
            }
        }
        for (Map.Entry<Double,List<Estafeta>> aux : vezes.entrySet()) {
            aux.getValue().sort(Comparator.comparing(Estafeta::getNome));
            if (cont==10) break;
            for (Estafeta e : aux.getValue()) {
                res.add((Transportadora) e);
                cont++;
                if(cont==10) break;
            }
            cont++;
        }
        return res;
    }

    /**
     * Registo de uma nova conta no sistema.
     * @param email Email do novo utilizador.
     * @param pass Password do novo utilizador.
     * @param cod Código do novo utilizador.
     * @param nome Nome do novo utilizador.
     * @param loc Localização do novo utilizador.
     * @param f File onde vamos armazenar a informação do novo utilizador.
     * @param tipo Tipo do novo utilizador.
     * @param nif NIF do novo utilizador (só caso seja um transportador).
     * @param cert Boolean que diz se um transportador é certificado para transportar medicamentos.
     * @throws IOException Exception caso hajam erros de input.
     * @throws ExistingCodeException Exception caso já exista aquele utilizador.
     */
    public void registar(String email, String pass, String cod, String nome, GPS loc, FileIO f, String tipo, String nif,boolean cert) throws IOException, ExistingCodeException {
        Entrada a = new Utilizador();
        a = a.newEntrada(tipo);
        a.setCod(cod);
        a.setNome(nome);
        a.setLocalizacao(loc);
        if(a instanceof Transportadora) {
            ((Transportadora) a).setRaio(ThreadLocalRandom.current().nextDouble(60, 151));
            ((Transportadora) a).setPrecoKM(ThreadLocalRandom.current().nextDouble(1,6));
            ((Transportadora) a).setNIF(nif);
            ((Transportadora) a).setCertificada(cert);
        }
        f.registaConta(email,pass,a,this);
    }

    /**
     * Login de um utilizador.
     * @param email Email do utilizador que vai dar login.
     * @param pass Password do utilizador que vai dar login.
     * @param f File onde estão armazenadas as informações de login.
     * @throws IOException Exception caso hajam erros de input.
     * @throws InvalidInputException Exception caso o input que damos não seja o correto.
     */
    public void login(String email, String pass, FileIO f) throws IOException, InvalidInputException {
        f.validaLogin(email,pass, this);
    }

    /**
     * Método que termina a sessão.
     */
    public void logoff() {
        this.login = null;
    }

    /**
     * Adiciona uma nova conta.
     * @param a Conta que vamos adicionar.
     * @throws ExistingCodeException Exception caso já exista essa conta.
     */
    public void add(Entrada a) throws ExistingCodeException {
        if(a instanceof Utilizador) addUtilizador((Utilizador) a);
        else if(a instanceof Transportadora) addTrabalhador((Transportadora) a);
        else if(a instanceof Voluntario) addTrabalhador((Voluntario) a);
        else if(a instanceof LojaFilaEspera) addLoja((LojaFilaEspera) a);
        else if(a instanceof Loja) addLoja((Loja) a);
    }

    /**
     * Adiciona uma encomenda a um utilizador.
     * @param cod Código do utilizador.
     * @param e Encomenda que vamos adicionar.
     */
    public void addEncomendaUtilizador(String cod,Encomenda e) {
        this.utilizadores.get(cod).addEncomenda(e);
    }

    /**
     * Adiciona uma encomenda a uma loja.
     * @param cod Código da loja.
     * @param e Encomenda que vamos adicionar.
     * @throws LojaInexistenteException Exception caso essa loja não exista.
     */
    public void addEncomendaLoja(String cod,Encomenda e) throws LojaInexistenteException {
        if(this.getLojas().get(cod) != null) this.lojas.get(cod).addPedido(e);
        else throw new LojaInexistenteException("A loja " + cod + " nao existe");
    }

    /**
     * Getter de um utilizador do sistema.
     * @param cod Código do utilizador.
     * @return Utilizador.
     */
    public Utilizador getUtilizador(String cod) {
        return this.utilizadores.get(cod).clone();
    }

    /**
     * Getter de uma loja do sistema.
     * @param cod Código da loja.
     * @return Loja.
     */
    public Loja getLoja(String cod) {
        return this.lojas.get(cod).clone();
    }

    /**
     * Getter de uma estafeta que esteja no sistema.
     * @param cod Código de um estafeta.
     * @return Estafeta.
     */
    public Estafeta getEstafeta(String cod) {
        return this.trabalhadores.get(cod).clone();
    }

    /**
     * Setter do raio de um estafeta.
     * @param cod Código de um estafeta.
     * @param raio Raio que pretendemos dar a um estafeta.
     */
    public void setRaio(String cod, double raio) {
        this.trabalhadores.get(cod).setRaio(raio);
    }

    /**
     * Setter do preço por kilometro de um transportador.
     * @param cod Código do transportador.
     * @param preco Preço que pretendemos dar a um tranportador.
     */
    public void setPrecokms(String cod, double preco) {
        Transportadora a = (Transportadora) this.trabalhadores.get(cod);
        a.setPrecoKM(preco);
    }

    /**
     * Método que muda a disponibilidade de um transportador.
     * @param cod Código de um estafeta.
     * @return Booleano com a nova disponibilidade do estafeta.
     */
    public boolean mudaDisponibilidade(String cod) {
        return this.trabalhadores.get(cod).mudaDisponibilidade();
    }

    /**
     * Método que indica a disponibilidade de um estafeta.
     * @param cod Código do estafeta.
     * @return Disponibilidade do estafeta.
     */
    public boolean disponivel(String cod) {
        return this.trabalhadores.get(cod).isDisponivel();
    }

    /**
     * Lista das encomendas que um estafeta pode ir buscar, tendo em conta o raio de ação de um estafeta e a sua distância às lojas.
     * @param cod Código de um estafeta.
     * @return Lista de encomendas que um estafeta que pode ir buscar.
     */
    public List<Encomenda> encomendasDisponiveis(String cod) {
        Estafeta u = this.trabalhadores.get(cod);
        GPS v = this.trabalhadores.get(cod).getLocalizacao();
        List<Encomenda> res = new ArrayList<>();

        for (Loja l : this.lojas.values()) {
            if (l.getLocalizacao().distancia(v) < u.getRaio()) {
                for (Encomenda e : l.getPedidos()) {
                    res.add(e.clone());
                }
            }
        }
        return res;
    }

    /**
     * Método que
     * @param cod
     * @param transp
     * @return
     */
    public double precoDaEncomenda(String cod,String transp) {
        Loja lj = new Loja();
        Encomenda enc = new Encomenda();
        boolean stop = false;
        boolean temFila = false;
        double taxa = 0;

        for (Loja l : this.lojas.values()) {
            for (Encomenda e : l.getPedidos()) {
                if (e.getCod().equals(cod)) {
                    if (l instanceof LojaFilaEspera) {
                        temFila = true;
                        lj = new LojaFilaEspera((LojaFilaEspera) l);
                    } else lj = new Loja(l);
                    enc = new Encomenda(e);
                    stop = true;
                    break;
                }
            }
            if (stop) break;
        }
        if (!stop) {
            return -1;
        }
        if (temFila) {
            LojaFilaEspera ljfe = (LojaFilaEspera) lj;
            taxa = ljfe.getTempoEspera()*ljfe.getListaEspera()*1.30;
        }
        Transportadora t = (Transportadora) this.trabalhadores.get(transp);
        double dist = lj.getLocalizacao().distancia(t.getLocalizacao());
        double preco = t.precoEncomenda(enc.getPeso(),dist);
        return (preco+preco*taxa);
    }

    public double precoDaEncomendaMenu(Encomenda enc, String transp) {
        boolean temFila = false;
        double taxa = 0;
        Loja l;
        if ((l = this.getLoja(enc.getLoja())) instanceof LojaFilaEspera)
            temFila = true;
        if (temFila) {
            LojaFilaEspera a;
            a = (LojaFilaEspera) this.getLoja((enc.getLoja()));
            taxa = a.getTempoEspera() * a.getListaEspera() * 1.30;
        }
        Transportadora t = (Transportadora) this.trabalhadores.get(transp);
        double dist = l.getLocalizacao().distancia(t.getLocalizacao());
        double preco = t.precoEncomenda(enc.getPeso(), dist);
        return (preco + preco * taxa);
    }

    /**
     * Método que adiciona kilometros percorridos numa encomenda ao total de kilometros percorridos por uma transportadora.
     * @param cod Código da transportadora.
     * @param loja Loja aonde a transportadora foi buscar a encomenda.
     */
    public void aumentaKms(String cod,String loja) {
        Transportadora t = (Transportadora) this.trabalhadores.get(cod);
        t.aumentaKms(this.lojas.get(loja).getLocalizacao());
    }

    /**
     * Método que permite classificar um estafeta.
     * @param codEsta Código de um estafeta.
     * @param value Classificação que vamos dar.
     */
    public void classifica(String codEsta, int value) {
        this.trabalhadores.get(codEsta).classifica(value);
    }
}
