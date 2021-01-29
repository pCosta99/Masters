package Model;

import Model.Exceptions.*;

import java.io.*;
import java.time.*;
import java.util.*;

public class TrazAquiModel implements ITrazAquiModel, java.io.Serializable{

    private int numEncomendas;
    private Map<String,Encomenda> encomendasEntregues;
    private Map<String,Encomenda> encomendasemEspera;
    private Map<String, Utilizador> utilizadores;
    private Map<String, Loja> lojas;
    private Map<String, Voluntario> voluntarios;
    private Map<String, EmpresaTransporte> empresas;
    private Map<String, List<LinhaDeEncomenda>> produtosLoja;

    /**
     * 1 - Sol
     * 2 - Nublado
     * 3 - Chuva
     * 4 - Neve
     */
    private static int metereologia = new Random().nextInt(4) + 1;

    /**
     * 1 - Leve
     * 2 - Moderado
     * 3 - Elevado
     * 4 - Acidente (trânsito muito congestionado)
     */
    private static int transito = new Random().nextInt(4) + 1;


    /**
     * Construtor vazio
     */
    public TrazAquiModel(){
        this.numEncomendas = 0;
        this.encomendasEntregues = new HashMap<String, Encomenda>();
        this.encomendasemEspera = new HashMap<String, Encomenda>();
        this.utilizadores = new HashMap<String, Utilizador>();
        this.lojas = new HashMap<String, Loja>();
        this.voluntarios = new HashMap<String, Voluntario>();
        this.empresas = new HashMap<String, EmpresaTransporte>();
        this.produtosLoja = new HashMap<String, List<LinhaDeEncomenda>>();
    }

    /**
     * Método que devolve a metereologia atual
     * @return
     */
    public int getMetereologia(){
        return metereologia;
    }

    /**
     * Método que devolve o trânsito atual
     * @return
     */
    public int getTransito(){
        return transito;
    }

    /**
     * Regista um utilizador
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */

    public void registaUtilizador(String nome, String email, String password, String nif, double latitude, double longitude) throws EmailExistenteException {
        boolean existeMail = false;
        for(Map.Entry<String, Utilizador> u : utilizadores.entrySet()){
            if(u.getValue().getEmail().equals(email)) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(email);
        }
        else {
            Coordenadas cord = new Coordenadas(latitude, longitude);

            Integer cod = this.utilizadores.size() + 1;
            boolean existe = false;
            if (this.utilizadores.containsKey("u" + cod)) {
                existe = true;
                while (existe) {
                    existe = this.utilizadores.containsKey("u" + (++cod));
                }
            }

            String codigo = "u" + cod;

            Utilizador ut = new Utilizador(codigo, email, nif, nome, password, cord);

            utilizadores.put(codigo, ut.clone());
        }
    }

    /**
     * Regista um utilizador
     * @param cod
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    public void registaUtilizador(String cod, String email, String nif, String nome, String password, double latitude, double longitude) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, Utilizador> u : utilizadores.entrySet()){
            if(u.getValue().getEmail().equals(email)) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(email);
        }
        else {
            Coordenadas cord = new Coordenadas(latitude, longitude);

            Utilizador ut = new Utilizador(cod, email, nif, nome, password, cord);

            utilizadores.put(cod, ut.clone());
        }
    }

    /**
     * Regista um utilizador passado como parâmetro
     * @param ut
     */
    public void registaUtilizador(Utilizador ut) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, Utilizador> u : utilizadores.entrySet()){
            if(u.getValue().getEmail().equals(ut.getEmail())) existeMail = true;
        }

        if(existeMail)
            throw new EmailExistenteException(ut.getEmail());
        else
            utilizadores.put(ut.getCod(), ut.clone());
    }

    /**
     * Regista uma loja
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    public void registaLoja(String email, String nif, String nome, String password, double latitude, double longitude) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, Loja> u : lojas.entrySet()){
            if(u.getValue().getEmail().equals(email)) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(email);
        }
        else {


            Coordenadas cord = new Coordenadas(latitude, longitude);

            Integer cod = this.lojas.size() + 1;
            boolean existe = false;
            if (this.lojas.containsKey("l" + cod)) {
                existe = true;
                while (existe) {
                    existe = this.lojas.containsKey("l" + (++cod));
                }
            }

            String codigo = "l" + cod;

            Loja loja = new Loja(codigo, email, nif, nome, password, cord, -1, Duration.ZERO);

            lojas.put(codigo, loja.clone());

            this.produtosLoja.put(codigo, new ArrayList<>());
        }
    }

    /**
     * Regista uma loja
     * @param cod
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    public void registaLoja(String cod, String email, String nif, String nome, String password, double latitude, double longitude) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, Loja> u : lojas.entrySet()){
            if(u.getValue().getEmail().equals(email)) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(email);
        }
        else {

            Coordenadas cord = new Coordenadas(latitude, longitude);

            Loja loja = new Loja(cod, email, nif, nome, password, cord, 0, Duration.ZERO);

            lojas.put(cod, loja.clone());

            this.produtosLoja.put(cod, new ArrayList<>());
        }
    }

    /**
     * Regista uma loja passada como parâmetro
     * @param loja
     */
    public void registaLoja(Loja loja) throws EmailExistenteException {
        boolean existeMail = false;
        for(Map.Entry<String, Loja> u : lojas.entrySet()){
            if(u.getValue().getEmail().equals(loja.getEmail())) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(loja.getEmail());
        }
        else {
            lojas.put(loja.getCod(), loja.clone());
            this.produtosLoja.put(loja.getCod(), new ArrayList<>());
        }
    }

    /**
     * Regista um voluntario
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     * @param raio
     */
    public void registaVoluntario(String email, String nif, String nome, String password,
                                  double latitude, double longitude, double raio, double velocidade) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, Voluntario> u : voluntarios.entrySet()){
            if(u.getValue().getEmail().equals(email)) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(email);
        }
        else {
            Coordenadas cord = new Coordenadas(latitude, longitude);

            Integer cod = this.voluntarios.size() + 1;
            boolean existe = false;
            if (this.voluntarios.containsKey("v" + cod)) {
                existe = true;
                while (existe) {
                    existe = this.voluntarios.containsKey("v" + (++cod));
                }
            }

            String codigo = "v" + cod;

            Voluntario voluntario = new Voluntario(codigo, email, nif, nome, password, cord, raio, velocidade);

            voluntarios.put(codigo, voluntario.clone());
        }
    }

    /**
     * Regista um voluntário passado como parâmetro
     * @param voluntario
     */
    public void registaVoluntario(Voluntario voluntario) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, Voluntario> u : voluntarios.entrySet()){
            if(u.getValue().getEmail().equals(voluntario.getEmail())) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(voluntario.getEmail());
        }
        else
            voluntarios.put(voluntario.getCod(),voluntario.clone());
    }

    /**
     * Regista uma empresa de transporte
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     * @param raio
     * @param precoKm
     */
    public void registaEmpresaTransporte(String email, String nif, String nome, String password,
                                         double latitude, double longitude, double raio, double precoKm,
                                         boolean medica, double velocidade, double precoPorKg) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, EmpresaTransporte> u : empresas.entrySet()){
            if(u.getValue().getEmail().equals(email)) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(email);
        }
        else {
            Coordenadas cord = new Coordenadas(latitude, longitude);

            Integer cod = this.empresas.size() + 1;
            boolean existe = false;
            if (this.empresas.containsKey("t" + cod)) {
                existe = true;
                while (existe) {
                    existe = this.empresas.containsKey("t" + (++cod));
                }
            }

            String codigo = "t" + cod;

            EmpresaTransporte empresaTransporte = new EmpresaTransporte(codigo, email, nif, nome,
                    password, cord, raio, precoKm, 0, 0, medica, velocidade, precoPorKg);

            empresas.put(codigo, empresaTransporte.clone());
        }
    }

    /**
     * Regista uma empresa de transporte passada como parâmetro
     * @param empresaTransporte
     */
    public void registaEmpresaTransporte(EmpresaTransporte empresaTransporte) throws EmailExistenteException{
        boolean existeMail = false;
        for(Map.Entry<String, EmpresaTransporte> u : empresas.entrySet()){
            if(u.getValue().getEmail().equals(empresaTransporte.getEmail())) existeMail = true;
        }

        if(existeMail){
            throw new EmailExistenteException(empresaTransporte.getEmail());
        }
        else
            empresas.put(empresaTransporte.getCod(),empresaTransporte.clone());
    }

    /**
     * Regista uma encomenda passada como parâmetro
     * @param encomenda
     */
    public void registaEncomenda(Encomenda encomenda){

        double precoProds = 0;

        for (LinhaDeEncomenda enc : encomenda.getLinhaEnc()){
            precoProds += enc.getPreco() * enc.getPeso();
        }

        List<LinhaDeEncomenda> linhaDeEncomendas = new ArrayList<>();

        for (LinhaDeEncomenda le : encomenda.getLinhaEnc()){
            linhaDeEncomendas.add(le.clone());
        }

        this.produtosLoja.put(encomenda.getCodLoja(), linhaDeEncomendas);

        encomenda.setPreco(precoProds);

        encomendasemEspera.put(encomenda.getCodEnc(),encomenda.clone());
    }

    /**
     * Método responsável pelo login do Utilizador, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    public String loginUtilizador(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException {
        boolean existeMail = false;
        boolean passValida = false;
        for(Map.Entry<String, Utilizador> u : utilizadores.entrySet()){
            if(u.getValue().getEmail().equals(email)){
                existeMail = true;
                if(u.getValue().getPassword().equals(pass)) passValida = true;
            }
        }

        if(!existeMail){
            throw new EmailNaoValidoException(email);
        }
        else if(!passValida)
            throw new PasswordNaoValidoException(pass);

        else {
            String res = "";
            for (Utilizador u : this.utilizadores.values()) {
                if (u.getEmail().equals(email) && u.getPassword().equals(pass)) {
                    res = u.getCod();
                    break;
                }
            }
            return res;
        }
    }

    /**
     * Método responsável pelo login da Loja, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    public String loginLoja(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException {
        boolean existeMail = false;
        boolean passValida = false;
        for(Map.Entry<String, Loja> u : lojas.entrySet()){
            if(u.getValue().getEmail().equals(email)){
                existeMail = true;
                if(u.getValue().getPassword().equals(pass)) passValida = true;
            }
        }

        if(!existeMail){
            throw new EmailNaoValidoException(email);
        }
        else if(!passValida)
            throw new PasswordNaoValidoException(pass);
        else {

            String res = "";
            for (Loja l : this.lojas.values()) {
                if (l.getEmail().equals(email) && l.getPassword().equals(pass)) {
                    res = l.getCod();
                    break;
                }
            }
            return res;
        }
    }

    /**
     * Método responsável pelo login do Voluntário, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    public String loginVoluntario(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException {
        boolean existeMail = false;
        boolean passValida = false;
        for(Map.Entry<String, Voluntario> u : voluntarios.entrySet()){
            if(u.getValue().getEmail().equals(email)){
                existeMail = true;
                if(u.getValue().getPassword().equals(pass)) passValida = true;
            }
        }

        if(!existeMail){
            throw new EmailNaoValidoException(email);
        }
        else if(!passValida)
            throw new PasswordNaoValidoException(pass);

        else {
            String res = "";
            for (Voluntario v : this.voluntarios.values()) {
                if (v.getEmail().equals(email) && v.getPassword().equals(pass)) {
                    res = v.getCod();
                    break;
                }
            }
            return res;
        }
    }

    /**
     * Método responsável pelo login da empesa de transporte,
     * retorna o código do utilizador se as credenciais
     * forem válidas, caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    public String loginEmpresa(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException {
        boolean existeMail = false;
        boolean passValida = false;
        for(Map.Entry<String, EmpresaTransporte> u : empresas.entrySet()){
            if(u.getValue().getEmail().equals(email)){
                existeMail = true;
                if(u.getValue().getPassword().equals(pass)) passValida = true;
            }
        }

        if(!existeMail){
            throw new EmailNaoValidoException(email);
        }
        else if(!passValida)
            throw new PasswordNaoValidoException(pass);

        else {
            String res = "";
            for (EmpresaTransporte e : this.empresas.values()) {
                if (e.getEmail().equals(email) && e.getPassword().equals(pass)) {
                    res = e.getCod();
                    break;
                }
            }
            return res;
        }
    }

    /**
     * ADICIONAR AS EXCEÇÕES E ACABAR
     *
     * Método responsável por efetuar o pedido de uma
     * encomenda por parte de um utilizador
     * @param enc
     */
    public void pedidoEncomenda(String codUt, String codLoja, List<List<String>> enc, boolean medica) throws ProdutosNaoExisteException, LojaInexistenteException{

        if(!this.lojas.containsKey(codLoja)){
            throw new LojaInexistenteException(codLoja);
        }
        else {
            Encomenda encomenda = new Encomenda();
            String codEnc = "e" + this.numEncomendas++;
            encomenda.setCodEnc(codEnc);
            encomenda.setCodLoja(codLoja);
            encomenda.setCodUtilizador(codUt);
            encomenda.setMedica(medica);

            for (List<String> l : enc) {
                boolean existe = false;
                for (LinhaDeEncomenda p : this.produtosLoja.get(codLoja)) {
                    if (p.getCodigo().equals(l.get(0))) {
                        encomenda.addLinhaEnc(p.clone(), l.get(1));
                        existe = true;
                    }
                }
                if (!existe){
                    throw new ProdutosNaoExisteException(l.get(0));
                }
            }
            this.encomendasemEspera.put(codEnc, encomenda.clone());
        }
    }

    /**
     * Método responsável por sinalizar encomendas aceites pela Loja,
     * mudando o estado de pendente para aceiteLoja.
     * @param codEnc
     */
    public void aceitaEncomenda(String codEnc) throws EncomendaInexistenteException {
        if(!this.encomendasemEspera.containsKey(codEnc) || !this.encomendasemEspera.get(codEnc).getEstado().equals("Pendente")){
            throw new EncomendaInexistenteException(codEnc);
        }

        else {
            if (this.encomendasemEspera.containsKey(codEnc)) {

                this.encomendasemEspera.get(codEnc).setEstado("AceiteLoja");
            }
        }
    }

    /**
     * Método responsável por sinalizar encomendas aceites pela Loja,
     * mudando o estado de pendente para aceiteLoja.
     * @param codLoja
     * @param codEnc
     */
    public void aceitaEncomendaLoja(String codLoja ,String codEnc) throws LojaInexistenteException, EncomendaInexistenteException{
        if(!this.encomendasemEspera.containsKey(codEnc) || !this.encomendasemEspera.get(codEnc).getEstado().equals("Pendente")){
            throw new EncomendaInexistenteException(codEnc);
        }

        /*boolean existeLoja = false;
        for(Map.Entry<String, Loja> me : this.lojas.entrySet()){
            if(me.getKey().equals(codLoja)) existeLoja = true;
        }*/
        if(!this.lojas.containsKey(codLoja)){
            throw new LojaInexistenteException(codLoja);
        }

        else {

            if (this.encomendasemEspera.get(codEnc).getCodLoja().equals(codLoja)) {

                this.encomendasemEspera.get(codEnc).setEstado("AceiteLoja");
            }
        }
    }

    /**
     * Método que devolve as encomendas para entrega numa string,
     * para apresentar à entidade transporte
     * distanciaKm
     * @param cod
     * @return
     */
    public String encomendasParaEntregaEmpresa(String cod) throws EmpresaInexistenteException {
        if(!this.empresas.containsKey(cod))
            throw new EmpresaInexistenteException(cod);

        else {
            EmpresaTransporte emp = this.empresas.get(cod);

            boolean aceitaMedicas = emp.aceitoTransporteMedicamentos();

            List<Encomenda> encAceites = new ArrayList<>();

            for (Encomenda e : this.encomendasemEspera.values()) {
                if ((e.getEstado().equals("AceiteLoja") || (e.getEstado().equals("Aguardando") )) && (!e.getMedica() || e.getMedica() == aceitaMedicas)) {
                    encAceites.add(e.clone());
                }
            }

            List<Encomenda> encomendas = new ArrayList<>();

            for (Encomenda enc : encAceites) {
                if (emp.getLocalizacao().dentroRaio(this.lojas.get(enc.getCodLoja()).getLocalizacao(), emp.getRaio()) &&
                        emp.getLocalizacao().dentroRaio(this.utilizadores.get(enc.getCodUtilizador()).getLocalizacao(), emp.getRaio())) {
                    encomendas.add(enc.clone());
                }
            }


            StringBuilder sb = new StringBuilder();

            for (Encomenda en : encomendas) {
                boolean lojaComFila = false;
                //calcular a distancia
                double distancialoja = emp.getLocalizacao().distanciaKm(this.lojas.get(en.getCodLoja()).getLocalizacao());
                double distanciaCliente = emp.getLocalizacao().distanciaKm(this.utilizadores.get(en.getCodUtilizador()).getLocalizacao());

                if (this.lojas.get(en.getCodLoja()).getFiladeEspera() != -1) {
                    lojaComFila = true;
                }
                Duration tempoEsperaEncomenda = calculaTempoEnc(en.getCodEnc(), cod);

                sb.append("Codigo da Encomenda: " + en.getCodEnc() + "\n");
                sb.append("Peso: " + en.getPeso() + "\n");
                sb.append("Distânca à Loja: " + distancialoja + "\n");
                sb.append("Distânca ao Cliente: " + distanciaCliente + "\n");
                if(lojaComFila)
                    sb.append("Estimativa do tempo até entrega (com tempo da fila): " + tempoEsperaEncomenda.toString().substring(2)).append("\n");
                else
                    sb.append("Estimativa do tempo até entrega (sem tempo da fila): " + tempoEsperaEncomenda.toString().substring(2)).append("\n");
                sb.append("\n");
            }
            return sb.toString();
        }
    }

    /**
     * Método que devolve as encomendas para entrega numa String
     * para entregar ao voluntário
     * @param cod
     * @return
     */
    public String encomendasParaEntregaVoluntario(String cod){

        Voluntario vol = this.voluntarios.get(cod);

        List<Encomenda> encAceites = new ArrayList<>();

        for(Encomenda e : this.encomendasemEspera.values()) {
            if ((e.getEstado().equals("AceiteLoja") || (e.getEstado().equals("Aguardando") )) && !e.getMedica()) {
                encAceites.add(e.clone());
            }
        }

        List<Encomenda> encomendas = new ArrayList<>();

        for(Encomenda e : encAceites) {
            if (vol.getLocalizacao().dentroRaio(this.lojas.get(e.getCodLoja()).getLocalizacao(), vol.getRaio()) &&
                    vol.getLocalizacao().dentroRaio(this.utilizadores.get(e.getCodUtilizador()).getLocalizacao(), this.voluntarios.get(cod).getRaio())) {
                encomendas.add(e.clone());
            }
        }

        StringBuilder sb = new StringBuilder();

        for (Encomenda en : encomendas){
            boolean lojaComFila = false;

            //calcular a distancia
            double distancialoja = vol.getLocalizacao().distanciaKm(this.lojas.get(en.getCodLoja()).getLocalizacao());
            double distanciaCliente = vol.getLocalizacao().distanciaKm(this.utilizadores.get(en.getCodUtilizador()).getLocalizacao());

            if (this.lojas.get(en.getCodLoja()).getFiladeEspera() != -1) {
                lojaComFila = true;
            }

            Duration tempoEsperaEncomenda = calculaTempoEnc(en.getCodEnc(),cod);

            sb.append("Codigo da Encomenda: " + en.getCodEnc()+"\n");
            sb.append("Peso: " + en.getPeso() + "\n");
            sb.append("Distânca à Loja: " +distancialoja + "\n");
            sb.append("Distânca ao Cliente: " + distanciaCliente + "\n");
            if(lojaComFila)
                sb.append("Estimativa do tempo até entrega(com tempo da fila): " + tempoEsperaEncomenda.toString().substring(2)).append("\n");
            else{
                sb.append("Estimativa do tempo até entrega (sem tempo da fila): " + tempoEsperaEncomenda.toString().substring(2)).append("\n");
            }
            sb.append("\n");
        }
        return sb.toString();


    }

    /**
     * Método que devolve as encomendas a aguardar aprovação para o utilizador
     * @param codUt
     * @return
     */
    public List<List<String>> encomendasAguardandoUtilizador(String codUt){
        ArrayList<List<String>> res = new ArrayList<>();

        for(Map.Entry<String, Encomenda> me : this.encomendasemEspera.entrySet()){
            if(me.getValue().getEstado().equals("Aguardando") && me.getValue().getCodUtilizador().equals(codUt)){

                for (Map.Entry<String,Duration> map : me.getValue().getEmpresasAguardar().entrySet()) {
                    List<String> aux = new ArrayList<>();
                    aux.add(me.getValue().getCodEnc());
                    aux.add(map.getKey());
                    aux.add(this.empresas.get(map.getKey()).getNome());

                    double custo = this.empresas.get(map.getKey()).
                            custoEncomendas(this.lojas.get(me.getValue().getCodLoja()).getLocalizacao(),
                                    this.utilizadores.get(me.getValue().getCodUtilizador()).getLocalizacao(),
                                    me.getValue().getPeso());
                    aux.add(Double.toString(custo));

                    Duration tempoEspera = map.getValue();
                    aux.add(tempoEspera.toString().substring(2));

                    res.add(aux);
                }
            }
        }


        return res;
    }

    /**
     * Utilizador aceita encomenda
     * @param codEnc
     */
    public void aceitaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException{
        if(this.encomendasemEspera.containsKey(codEnc) && this.encomendasemEspera.get(codEnc).getEstado().equals("Aguardando")) {
            if (!this.encomendasemEspera.get(codEnc).getEmpresasAguardar().containsKey(codTrans)){
                throw new EmpresaInexistenteException(codTrans);
            }
            this.encomendasemEspera.get(codEnc).setEstado("AceiteUtilizador");
            this.encomendasemEspera.get(codEnc).setCodTransportadora(codTrans);
        }
        else
            throw new EncomendaInexistenteException(codEnc);
    }

    /**
     * Utilizador recusa encomenda
     * @param codEnc
     */
    public void recusaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException{
        if(this.encomendasemEspera.containsKey(codEnc) && this.encomendasemEspera.get(codEnc).getEstado().equals("Aguardando")) {
            if (!this.encomendasemEspera.get(codEnc).getEmpresasAguardar().containsKey(codTrans)) {
                throw new EmpresaInexistenteException(codTrans);
            }
            this.encomendasemEspera.get(codEnc).removeEmpresaAguarda(codTrans);
            if(this.encomendasemEspera.get(codEnc).numEmpresasAguardar() == 0)
                this.encomendasemEspera.get(codEnc).setEstado("AceiteLoja");
        }
        else
            throw new EncomendaInexistenteException(codEnc);
    }

    /**
     * Determina quantas encomendas um utilizador efetuou (que foram já entregues)
     */
    public int quantasEncomendasUtilizador(String codUtilizador){

        return this.utilizadores.get(codUtilizador).getEncomendas().size();
    }

    /**
     * Devolve as encomendas em espera para serem processadas numa determinada loja
     * @param codLoja
     * @return
     */
    public String encomendasEmEspera(String codLoja){
        StringBuilder sb = new StringBuilder();
        sb.append("------------Encomendas em espera na loja: -------------\n");
        boolean existem = false;

        for(Encomenda e : this.encomendasemEspera.values()){
            if(e.getCodLoja().equals(codLoja) && e.getEstado().equals("Pendente")){
                sb.append(e.toString());
                existem = true;
            }
        }
        if(!existem)
            sb.append("Não existem encomendas pedidas à espera para serem processadas nesta loja!\n");
        return sb.toString();
    }

    /**
     * Devolve o número de kms percorridos por uma empresa
     * @param codEmpresa
     * @return
     */
    public double quantosKmEmpresa(String codEmpresa){
        double res = 0;
        EmpresaTransporte emp = this.empresas.get(codEmpresa);

       for(String s : this.empresas.get(codEmpresa).getEncomendas()){
           Encomenda e = this.encomendasEntregues.get(s);
           Loja l = this.lojas.get(e.getCodLoja());
           double distanciaTransLoja = emp.getLocalizacao().distanciaKm(l.getLocalizacao());

           Utilizador ut = this.utilizadores.get(e.getCodUtilizador());
           double distanciaLojaCliente = l.getLocalizacao().distanciaKm(ut.getLocalizacao());

           res += distanciaLojaCliente + distanciaTransLoja;
       }

        return res;
    }


    /**
     * Devolve os 10 utilizadores que mais utilizam o sistema
     */
    public String utilizadoresMaisAtivos() {
        StringBuilder res = new StringBuilder();

        TreeSet<Utilizador> aux = new TreeSet<>(new Comparator<Utilizador>() {
            @Override
            public int compare(Utilizador u1, Utilizador u2) {
                if(quantasEncomendasUtilizador(u1.getCod()) == quantasEncomendasUtilizador(u2.getCod()))
                    return (int) u1.getCod().compareTo(u2.getCod());
                return  (int) quantasEncomendasUtilizador(u1.getCod()) - quantasEncomendasUtilizador(u2.getCod());
            }
        });


        for(Utilizador ut : this.utilizadores.values()){
            aux.add(ut.clone());
        }



        int i = 0;
        for(Utilizador u : aux) {
            res.append("Utilizador: ").append(u.getNome()).append(" realizou ").
                    append(quantasEncomendasUtilizador(u.getCod())).append(" compras.").append("\n");
            i++;
            if(i == 10) break;
        }


        return res.toString();
    }

    /**
     * Devolve as 10 empresas mais ativas (mais kilometros)
     * @return
     */
    public String empresasMaisAtivas(){
        StringBuilder res = new StringBuilder();

        TreeSet<EmpresaTransporte> aux = new TreeSet<>(new Comparator<EmpresaTransporte>() {
            @Override
            public int compare(EmpresaTransporte e1, EmpresaTransporte e2) {
                if(quantosKmEmpresa(e1.getCod()) == quantosKmEmpresa(e2.getCod()))
                    return (int) e1.getCod().compareTo(e2.getCod());
                return  (int) (quantosKmEmpresa(e1.getCod()) - quantosKmEmpresa(e2.getCod()));
            }
        });

        for(EmpresaTransporte emp : this.empresas.values()){
            aux.add(emp.clone());
        }

        int i = 0;
        for(EmpresaTransporte e : aux) {
            res.append("Empresa: ").append(e.getNome()).append(" realizou ").
                    append(quantosKmEmpresa(e.getCod())).append(" kms.").append("\n");
            i++;
            if(i == 10) break;
        }


        return res.toString();
    }

    /**
     * Determina a faturação de uma empresa num determinado período de tempo
     * @param codEmpresa
     * @param dataI
     * @param dataF
     * @return
     */
    public double faturacaoEmpresa(String codEmpresa, LocalDate dataI, LocalDate dataF) throws EmpresaInexistenteException{
        if(!this.empresas.containsKey(codEmpresa)) {
            throw new EmpresaInexistenteException(codEmpresa);
        }
        double r = 0;
        EmpresaTransporte emp = this.empresas.get(codEmpresa);
        double custoKm = emp.getPrecoKm();

        for(Encomenda e : this.encomendasEntregues.values()){
            if(e.getCodTransportadora().equals(codEmpresa) && e.getData().toLocalDate().isAfter(dataI) && e.getData().toLocalDate().isBefore(dataF)) {
                Loja l = this.lojas.get(e.getCodLoja());
                double distanciaTransLoja = emp.getLocalizacao().distanciaKm(l.getLocalizacao());

                Utilizador ut = this.utilizadores.get(e.getCodUtilizador());
                double distanciaLojaCliente = l.getLocalizacao().distanciaKm(ut.getLocalizacao());
                r += (distanciaLojaCliente + distanciaTransLoja) * custoKm;
            }
        }

        return r;
    }

    /**
     * Suporta a classificação da encomenda por parte do utilizador
     */
    public void classificarEntrega(String codEnc, int classificacao) throws EncomendaInexistenteException{
        if(this.encomendasEntregues.containsKey(codEnc))
            this.encomendasEntregues.get(codEnc).setClassificacao(classificacao);
        else
            throw new EncomendaInexistenteException(codEnc);
    }

    /**
     * Devolve o histórico das encomendas entregues a um determinado utilizador
     * @param codUt
     * @return
     */
    public List<String> historicoEncomendasUtilizador(String codUt){
        ArrayList<String> res = new ArrayList<>();

        for(Encomenda e : this.encomendasEntregues.values()){
            if(e.getCodUtilizador().equals(codUt)){
                StringBuilder sb = new StringBuilder();

                sb.append("Data : ").append(e.getData()).append("\n");
                sb.append("CodLoja: ").append(e.getCodLoja()).append("\n");
                sb.append("CodTransportadora: ").append(e.getCodTransportadora()).append("\n");
                sb.append("Preço: ").append(e.getPreco()).append("\n");
                sb.append("Medica: ").append(e.getMedica()).append("\n");
                sb.append("Classificação: ").append(e.getClassificacao()).append("\n");
                sb.append("Duração: ").append(e.getTempoEntrega().toString().substring(2)).append("\n");
                sb.append("Produtos: ").append(e.getLinhaEnc().toString()).append("\n");

                res.add(sb.toString());
            }
        }

        return res;
    }

    /**
     * Método que devolve os produtos do
     * sistema para poderem se encomendados
     * @return
     */
    public String verProdutos(){
        StringBuilder sb = new StringBuilder();

        for (Map.Entry<String, List<LinhaDeEncomenda>> p : this.produtosLoja.entrySet()){
            if (!p.getValue().isEmpty()) {
                sb.append("Loja ");
                sb.append(p.getKey());
                sb.append(": \n");
                for (LinhaDeEncomenda l : p.getValue()) {
                    sb.append("\t");
                    sb.append(l.getCodigo());
                    sb.append(", ");
                    sb.append(l.getDescricao());
                    sb.append(", ");
                    sb.append(l.getPreco());
                    sb.append(", ");
                    sb.append(l.getPeso());
                    sb.append("\n");
                }
            }
        }
        return sb.toString();
    }

    /**
     * Retorna a faturação de uma loja num determinado intervalo de tempo
     * @param codLoja
     * @param dataI
     * @param dataF
     * @return
     */
    public double faturacaoLoja(String codLoja, LocalDate dataI, LocalDate dataF) throws LojaInexistenteException{
        double res = 0;

        if(!this.lojas.containsKey(codLoja)){
            throw new LojaInexistenteException(codLoja);
        }

        for(Encomenda e : this.encomendasEntregues.values()){
            if(e.getCodLoja().equals(codLoja) && e.getData().toLocalDate().isAfter(dataI) && e.getData().toLocalDate().isBefore(dataF)){
                res += e.getPreco();
            }
        }

        return res;
    }


    /**
     * Devolve o histórico das encomendas processadas por uma determinada loja
     * @param codLoja
     * @return
     */
    public List<String> historicoEncomendasLoja(String codLoja){
        ArrayList<String> res = new ArrayList<>();

        for(Encomenda e : this.encomendasEntregues.values()){
            if(e.getCodLoja().equals(codLoja)){
                StringBuilder sb = new StringBuilder();

                sb.append("Data : ").append(e.getData()).append("\n");
                sb.append("Utilizador: ").append(e.getCodUtilizador()).append("\n");
                sb.append("CodTransportadora: ").append(e.getCodTransportadora()).append("\n");
                sb.append("Preço: ").append(e.getPreco()).append("\n");
                sb.append("Medico: ").append(e.getMedica()).append("\n");
                sb.append("Classificação: ").append(e.getClassificacao()).append("\n");
                sb.append("Duração: ").append(e.getTempoEntrega().toString().substring(2)).append("\n");
                sb.append("Produtos: ").append(e.getLinhaEnc().toString()).append("\n");

                res.add(sb.toString());
            }
        }

        return res;
    }

    /**
     * Adiciona um produto a uma determinada loja
     * @param descricao
     * @param peso
     * @param preco
     */
    public void adicionaProdutoLoja(String codLoja, String descricao, double peso, double preco){
        String cod;
        Random r = new Random();
        int codInt = r.nextInt((99 - 1) + 1 ) + 1;
        if(codInt > 9 )
            cod = "p" + String.valueOf(codInt);
        else
            cod = "p" + "0" + String.valueOf(codInt);
        while(this.produtosLoja.containsKey(cod)){
            codInt++;
            if(codInt > 9 )
                cod = "p" + String.valueOf(codInt);
            else
                cod = "p" + "0" + String.valueOf(codInt);
        }

        LinhaDeEncomenda prod = new LinhaDeEncomenda(cod, descricao, peso, preco);

        this.produtosLoja.get(codLoja).add(prod);
    }

    /**
     * Devolve o histórico das encomendas processadas por uma determinada empresa ou voluntario
     * @param codTrans
     * @return
     */
    public List<String> historicoEncomendasTransportadora(String codTrans){
        ArrayList<String> res = new ArrayList<>();

        for(Encomenda e : this.encomendasEntregues.values()){
            if(e.getCodTransportadora().equals(codTrans)){
                StringBuilder sb = new StringBuilder();

                sb.append("Data : ").append(e.getData()).append("\n");
                sb.append("Utilizador: ").append(e.getCodUtilizador()).append("\n");
                sb.append("CodLoja: ").append(e.getCodLoja()).append("\n");
                sb.append("Preço: ").append(e.getPreco()).append("\n");
                sb.append("Medico: ").append(e.getMedica()).append("\n");
                sb.append("Classificação: ").append(e.getClassificacao()).append("\n");
                sb.append("Duração: ").append(e.getTempoEntrega().toString().substring(2)).append("\n");
                sb.append("Produtos: ").append(e.getLinhaEnc().toString()).append("\n");

                res.add(sb.toString());
            }
        }

        return res;
    }

    /**
     * Método que regista o pedido de transporte de
     * uma encomenda por parte de uma empresa
     * @param codEmpresa
     * @param codEnc
     */
    public void pedidoTransporteEncomenda(String codEmpresa, String codEnc) throws EncomendaInexistenteException{
        boolean lojaComFila = false;

        if(this.encomendasemEspera.containsKey(codEnc) &&
                (this.encomendasemEspera.get(codEnc).getEstado().equals("AceiteLoja") ||
                        this.encomendasemEspera.get(codEnc).getEstado().equals("Aguardando"))) {
            Duration tempoEsperaEncomenda = calculaTempoEnc(codEnc,codEmpresa);

            this.encomendasemEspera.get(codEnc).adicionaEmpresaAguarda(codEmpresa, tempoEsperaEncomenda);
            this.encomendasemEspera.get(codEnc).setEstado("Aguardando");
        }
        else {
            throw new EncomendaInexistenteException(codEnc);
        }
    }

    /**
     * Método que devolve os códigos das encomendas aceites
     * pelos utilizadores e que aguardam o transporte por
     * parte da empresa transportadora
     * @param codTrans
     * @return
     */
    public String encomendasAceitesUtilizador(String codTrans){
        StringBuilder sb = new StringBuilder();

        for (Map.Entry<String,Encomenda> e : this.encomendasemEspera.entrySet()){
            if(e.getValue().getCodTransportadora().equals(codTrans) && e.getValue().getEstado().equals("AceiteUtilizador"))
                sb.append(e.getKey()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Devolve a lista dos códigos de encomenda que estão a ser transportados
     * por uma dada empresa
     * @param codEmpresa
     * @return
     */
    public List<String> encomendasEmTransporteEmpresa(String codEmpresa){
        List<String> res = new ArrayList<String>();

        if(!this.encomendasemEspera.values().isEmpty()) {
            for (Encomenda e : this.encomendasemEspera.values()) {
                if (e.getCodTransportadora()!=null && e.getCodTransportadora().equals(codEmpresa) && e.getEstado().equals("Transporte")) {
                    res.add(e.getCodEnc());
                }
            }
        }
        return res;
    }

    /**
     * Coloca uma encomenda como em transporte por uma empresa
     * @param codTransp
     */
    /*public void transportaEncomendaEmpresa(String codTransp, String codEncomenda) throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException {
        Encomenda e = new Encomenda();
        if(this.encomendasemEspera.get(codEncomenda).getMedica() && !this.empresas.get(codTransp).aceitoTransporteMedicamentos()){
            throw new TransportadoraNaoMedicaException(codEncomenda);
        }

        if(!this.encomendasemEspera.get(codEncomenda).getEstado().equals("AceiteUtilizador")){
            throw new EncomendaNaoAceiteUtilizadorException(codEncomenda);
        }

        if(this.encomendasemEspera.containsKey(codEncomenda)) {
            e = this.encomendasemEspera.remove(codEncomenda);
            e.setCodTransportadora(codTransp);
            List<String> vazia = new ArrayList<>();
            e.setEmpresasAguardar(vazia);
            e.setEstado("Entregue");
            this.encomendasEntregues.put(codEncomenda, e);
            this.utilizadores.get(e.getCodUtilizador()).addEncomenda(e.getCodEnc());
            this.empresas.get(codTransp).addEncomenda(e.getCodEnc());
            this.lojas.get(e.getCodLoja()).addEncomenda(e.getCodEnc());
        }
        else
            throw new EncomendaInexistenteException(codEncomenda);
    }*/
    public void transportaEncomendaEmpresa(String codTransp, String codEncomenda) throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException {
        Encomenda e = new Encomenda();
        if(!this.encomendasemEspera.containsKey(codEncomenda)) {
            throw new EncomendaInexistenteException(codEncomenda);
        }
        if(this.encomendasemEspera.get(codEncomenda).getMedica() && !this.empresas.get(codTransp).aceitoTransporteMedicamentos()){
            throw new TransportadoraNaoMedicaException(codEncomenda);
        }

        if(!this.encomendasemEspera.get(codEncomenda).getEstado().equals("AceiteUtilizador")){
            throw new EncomendaNaoAceiteUtilizadorException(codEncomenda);
        }

        Duration tempoEsperaEncomenda = calculaTempoEnc(codEncomenda,codTransp);

        e = encomendasemEspera.get(codEncomenda);
        e.setCodTransportadora(codTransp);
        Map<String, Duration> vazia = new HashMap<>();
        e.setEmpresasAguardar(vazia);
        e.setEstado("Transporte");
        e.setTempoEntrega(tempoEsperaEncomenda);
        this.empresas.get(codTransp).addEncomenda(e.getCodEnc());
        this.lojas.get(e.getCodLoja()).addEncomenda(e.getCodEnc());
    }

    /**
     * Coloca uma encomenda como em transporte por um voluntario
     * @param codTransp
     */
    /*public void transportaEncomendaVoluntario(String codTransp, String codEncomenda) throws EncomendaInexistenteException, TransportadoraNaoMedicaException {
        Encomenda e = new Encomenda();
        if(this.encomendasemEspera.get(codEncomenda).getMedica() && !this.voluntarios.get(codTransp).aceitoTransporteMedicamentos()){
            throw new TransportadoraNaoMedicaException(codEncomenda);
        }

        if(this.encomendasemEspera.containsKey(codEncomenda)) {
            e = this.encomendasemEspera.remove(codEncomenda);
            e.setCodTransportadora(codTransp);
            List<String> vazia = new ArrayList<>();
            e.setEmpresasAguardar(vazia);
            e.setEstado("Entregue");
            this.encomendasEntregues.put(codEncomenda, e);
            this.utilizadores.get(e.getCodUtilizador()).addEncomenda(e.getCodEnc());
            this.voluntarios.get(codTransp).addEncomenda(e.getCodEnc());
            this.lojas.get(e.getCodLoja()).addEncomenda(e.getCodEnc());
        }
        else
            throw new EncomendaInexistenteException(codEncomenda);
    }*/
     public void transportaEncomendaVoluntario(String codTransp, String codEncomenda) throws EncomendaInexistenteException, TransportadoraNaoMedicaException {
        Encomenda e = new Encomenda();
         if(!this.encomendasemEspera.containsKey(codEncomenda)) {
             throw new EncomendaInexistenteException(codEncomenda);
         }

        if(this.encomendasemEspera.get(codEncomenda).getMedica() && !this.voluntarios.get(codTransp).aceitoTransporteMedicamentos()){
            throw new TransportadoraNaoMedicaException(codEncomenda);
        }

        Duration tempoEsperaEncomenda = calculaTempoEnc(codEncomenda,codTransp);

        e = this.encomendasemEspera.get(codEncomenda);
        e.setCodTransportadora(codTransp);
        Map<String, Duration> vazia = new HashMap<>();
        e.setEmpresasAguardar(vazia);
        e.setEstado("Transporte");
        e.setTempoEntrega(tempoEsperaEncomenda);
        this.voluntarios.get(codTransp).addEncomenda(e.getCodEnc());
        this.lojas.get(e.getCodLoja()).addEncomenda(e.getCodEnc());

    }

    /**
     * Grava em ficheiro de objetos o estado atual da aplicação
     * @param nomeFicheiro
     */
    public boolean guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);

        oos.writeObject(this);
        oos.flush();
        oos.close();

        return true;
    }

    /**
     * Atualiza o tempo de espera médio de cada encomenda numa dada loja
     * @param codLoja
     * @param tempoEspera
     */
    public void atualizaTempoEspera (String codLoja, Duration tempoEspera){
        this.lojas.get(codLoja).setTempoEsperaUnico(tempoEspera);
    }

    /**
     * Regista que uma encomenda foi entregue com sucesso
     * @param codTrans
     * @param codEnc
     */
    public void encomendaEntregue (String codTrans, String codEnc, Duration tempo) throws EncomendaNaoTransportadaException{
        if(!this.encomendasemEspera.containsKey(codEnc) ||
                !this.encomendasemEspera.get(codEnc).getEstado().equals("Transporte") ||
                !this.encomendasemEspera.get(codEnc).getCodTransportadora().equals(codTrans)){
            throw new EncomendaNaoTransportadaException(codEnc);
        }

        Encomenda e = this.encomendasemEspera.remove(codEnc);
        e.setEstado("Entregue");
        e.setTempoEntrega(tempo);
        this.encomendasEntregues.put(codEnc, e.clone());
        this.utilizadores.get(e.getCodUtilizador()).addEncomenda(codEnc);
    }

    /**
     * Permite à loja atualizar o tamanho da sua fila de espera para levantar encomendas
     * @param codLoja
     * @param tamanho
     */
    public void atualizaFilaDeEspera(String codLoja, int tamanho) throws FilaDeEsperaException{
        if(tamanho >= -1){
            this.lojas.get(codLoja).setFiladeEspera(tamanho);
        }
        else {
            throw new FilaDeEsperaException(String.valueOf(tamanho));
        }
    }

    public int compare(Utilizador u1, Utilizador u2){
        return (int) this.quantasEncomendasUtilizador(u1.getCod()) - this.quantasEncomendasUtilizador(u2.getCod());
    }

    /**
     * Atualiza a velocidade média de uma empresa
     * @param codEmpresa
     * @param vel
     */
    public void atualizaVelMediaEmp(String codEmpresa, double vel){
        this.empresas.get(codEmpresa).setVelocidadeMedia(vel);
    }

    /**
     * Atualiza a velocidade média de um voluntário
     * @param codVol
     * @param vel
     */
    public void atualizaVelMediaVoluntario(String codVol, double vel){
        this.voluntarios.get(codVol).setVelocidadeMedia(vel);
    }

    /**
     * Devolve o histórico das encomendas processadas por uma determinada empresa ou voluntario
     * num determinado período de tempo
     * @param codTrans
     * @return
     */
    public List<String> historicoEncomendasTransportadoraData(String codTrans, LocalDate di, LocalDate df)
            throws EmpresaInexistenteException, VoluntarioInexistenteException{

        if( codTrans.charAt(0) == 't' && !this.empresas.containsKey(codTrans)){
            throw new EmpresaInexistenteException(codTrans);
        }
        if( codTrans.charAt(0) == 'v' && !this.voluntarios.containsKey(codTrans)){
            throw new VoluntarioInexistenteException(codTrans);
        }

        ArrayList<String> res = new ArrayList<>();

        for(Encomenda e : this.encomendasEntregues.values()){
            if(e.getCodTransportadora().equals(codTrans) && e.getData().toLocalDate().isBefore(df)
                    && e.getData().toLocalDate().isAfter(di)){

                StringBuilder sb = new StringBuilder();

                sb.append("Data : ").append(e.getData()).append("\n");
                sb.append("Utilizador: ").append(e.getCodUtilizador()).append("\n");
                sb.append("CodLoja: ").append(e.getCodLoja()).append("\n");
                sb.append("Preço: ").append(e.getPreco()).append("\n");
                sb.append("Medico: ").append(e.getMedica()).append("\n");
                sb.append("Classificação: ").append(e.getClassificacao()).append("\n");
                sb.append("Produtos: ").append(e.getLinhaEnc().toString()).append("\n");

                res.add(sb.toString());
            }
        }

        return res;
    }

    /**
     * Atualiza o preço por Km que uma empresa cobra
     * @param codEmp
     * @param preco
     */
    public void atualizaPrecoKm(String codEmp, double preco){
        this.empresas.get(codEmp).setPrecoKm(preco);
    }

    /**
     * Atualiza o preço por Kg que uma empresa cobra
     * @param codEmp
     * @param preco
     */
    public void atualizaPrecoKg(String codEmp, double preco){
        this.empresas.get(codEmp).setPrecoKg(preco);
    }







    /**
     * Solicita a diversos utilizadores para aprovarem uma entrega de uma empresa, que irá realizar
     * diversas entergas numa só viagem
     * @param codEmpresa
     * @param codEncomendas
     * @throws EncomendaInexistenteException
     */
    public void pedidoTransporteEncomendaGrupo(String codEmpresa,List<String> codEncomendas) throws EncomendaInexistenteException{

        for(String s : codEncomendas){
            if(this.encomendasemEspera.containsKey(s)) {

                Duration tempoEsperaEncomenda = calculaTempoEnc(s,codEmpresa);

                this.encomendasemEspera.get(s).adicionaEmpresaAguarda(codEmpresa, tempoEsperaEncomenda);
                this.encomendasemEspera.get(s).setEstado("Aguardando");



                this.encomendasemEspera.get(s).adicionaEmpresaAguarda(codEmpresa, tempoEsperaEncomenda);
                this.encomendasemEspera.get(s).setEstado("Aguardando");
            }
            else {
                throw new EncomendaInexistenteException(s);
            }
        }
    }

    /**
     * Empresa inicia o processo de transporte de um conjunto de encomendas para vários utilizadores
     * @param codTransp
     * @param codEncomendas
     * @throws EncomendaInexistenteException
     * @throws TransportadoraNaoMedicaException
     * @throws EncomendaNaoAceiteUtilizadorException
     */
    public void transportaParaUtilizadoresGrupo(String codTransp, List<String> codEncomendas)
            throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException {
        Encomenda e = new Encomenda();
        for(String s : codEncomendas){
            if(!this.encomendasemEspera.containsKey(s))
                throw new EncomendaInexistenteException(s);

            if(this.encomendasemEspera.get(s).getMedica() && !this.empresas.get(codTransp).aceitoTransporteMedicamentos()){
                throw new TransportadoraNaoMedicaException(s);
            }
            if(!this.encomendasemEspera.get(s).getEstado().equals("AceiteUtilizador")){
                throw new EncomendaNaoAceiteUtilizadorException(s);
            }

            e = this.encomendasemEspera.get(s);
            e.setCodTransportadora(codTransp);
            Map<String, Duration> vazia = new HashMap<>();
            e.setEmpresasAguardar(vazia);
            e.setEstado("Transporte");
            this.empresas.get(codTransp).addEncomenda(e.getCodEnc());
            this.lojas.get(e.getCodLoja()).addEncomenda(e.getCodEnc());
        }
    }

    /**
     * Cálcula o tempo de viagem dado o código de uma encomenda e o código de uma transportadora
     * @param codEnc
     * @param codTranspo
     * @return
     */
    private Duration calculaTempoEnc(String codEnc, String codTranspo){
        Encomenda en = this.encomendasemEspera.get(codEnc);
        Duration tempoViagem;
        Duration tempoEsperaLoja;
        Duration tempoEsperaEncomenda = Duration.ZERO;
        boolean lojaComFila = false;
        //calcular tempo espera fila
        tempoEsperaLoja = Duration.ZERO;
        double percentagemAtrasoViagem = calculaPercentagemAtrasoViagem();
        double percentagemAtrasoLoja = calculaPercentagemAtrasoLoja();


        if (this.lojas.get(en.getCodLoja()).getFiladeEspera() != -1) {
            tempoEsperaLoja = this.lojas.get(en.getCodLoja()).tempoEsperaLoja();
            tempoEsperaLoja = tempoEsperaLoja.plusDays(tempoEsperaLoja.toDaysPart() + ((int) (tempoEsperaLoja.toHoursPart() * percentagemAtrasoLoja)));
            tempoEsperaLoja = tempoEsperaLoja.plusHours(tempoEsperaLoja.toHoursPart() + ((int) (tempoEsperaLoja.toHoursPart() * percentagemAtrasoLoja)));
            tempoEsperaLoja = tempoEsperaLoja.plusMinutes(tempoEsperaLoja.toMinutesPart() + ((int) (tempoEsperaLoja.toHoursPart() * percentagemAtrasoLoja)));
            lojaComFila = true;
        }


        if(codTranspo.charAt(0) == 't'){
            EmpresaTransporte vol = this.empresas.get(codTranspo);

            //calcular a distancia
            double distancialoja = vol.getLocalizacao().distanciaKm(this.lojas.get(en.getCodLoja()).getLocalizacao());
            double distanciaCliente = vol.getLocalizacao().distanciaKm(this.utilizadores.get(en.getCodUtilizador()).getLocalizacao());
            double distancia = distanciaCliente + distancialoja;

            //calcular tempo de viagem
            double tempoDouble = distancia / vol.getVelocidadeMedia();
            int tempoHoras = (int) (tempoDouble + (tempoDouble * percentagemAtrasoViagem));
            int tempoMinutos = (int)(((((int)((tempoDouble-((int)tempoDouble))*100))*60)/100) + tempoDouble*percentagemAtrasoViagem);
            tempoViagem = Duration.ZERO;
            tempoViagem = tempoViagem.plusHours(tempoHoras);
            tempoViagem = tempoViagem.plusMinutes(tempoMinutos);
        }
        else{
            Voluntario vol = this.voluntarios.get(codTranspo);


            //calcular a distancia
            double distancialoja = vol.getLocalizacao().distanciaKm(this.lojas.get(en.getCodLoja()).getLocalizacao());
            double distanciaCliente = vol.getLocalizacao().distanciaKm(this.utilizadores.get(en.getCodUtilizador()).getLocalizacao());
            double distancia = distanciaCliente + distancialoja;

            //calcular tempo de viagem
            double tempoDouble = distancia / vol.getVelocidadeMedia();
            int tempoHoras = (int) (tempoDouble + (tempoDouble * percentagemAtrasoViagem));
            int tempoMinutos = (int)(((((int)((tempoDouble-((int)tempoDouble))*100))*60)/100) + tempoDouble*percentagemAtrasoViagem);
            tempoViagem = Duration.ZERO;
            tempoViagem = tempoViagem.plusHours(tempoHoras);
            tempoViagem = tempoViagem.plusMinutes(tempoMinutos);

        }
        //calcular tempo total:
        if(lojaComFila) {
            tempoEsperaEncomenda = tempoEsperaEncomenda.plusDays(tempoEsperaLoja.toDaysPart());
            tempoEsperaEncomenda = tempoEsperaEncomenda.plusHours(tempoEsperaLoja.toHoursPart());
            tempoEsperaEncomenda = tempoEsperaEncomenda.plusMinutes(tempoEsperaLoja.toMinutesPart());
        }
        tempoEsperaEncomenda = tempoEsperaEncomenda.plusDays(tempoViagem.toDaysPart());
        tempoEsperaEncomenda = tempoEsperaEncomenda.plusHours(tempoViagem.toHoursPart());
        tempoEsperaEncomenda = tempoEsperaEncomenda.plusMinutes(tempoViagem.toMinutesPart());

        return tempoEsperaEncomenda;
    }

    /**
     * Cálcula a percentagem (de 0 a 1) de atraso relativamente ao tempo da viagem de uma encomenda
     * @return
     */
    private double calculaPercentagemAtrasoViagem(){
        double r = (transito-1)*0.1;

        if(LocalDate.now().getDayOfWeek() == DayOfWeek.SUNDAY)
            r += 0.05;

        return r;
    }

    /**
     * Cálcula a percentagem (de 0 a 1) de atraso relativamente ao tempo de espera na filia por uma encomenda
     * @return
     */
    private double calculaPercentagemAtrasoLoja(){
        double r = (metereologia - 1) * 0.1;

        if(LocalDate.now().getDayOfWeek() == DayOfWeek.SUNDAY)
            r += 0.1;

        if(LocalDate.now().getMonthValue() == 12)
            r += 0.05;

        return r;
    }

    /**
     * Devolve o nome de um utilizador dado o seu código
     * @param codUt
     * @return
     */
    public String getNomeUtilizador(String codUt){
        return this.utilizadores.get(codUt).getNome();
    }
}
