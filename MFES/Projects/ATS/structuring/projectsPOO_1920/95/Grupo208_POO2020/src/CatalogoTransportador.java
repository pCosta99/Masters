import Enums.Estado;
import Enums.Tipo;
import Exception.*;

import java.io.Serializable;
import java.util.*;
import java.time.LocalDateTime;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

/**
 * Classe que lida com a informação de vários transportadores.
 *
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class CatalogoTransportador implements Serializable {
    // Instance Variables
    private Map<String, Transportador> transportadores;
    private Map<String, String> credenciais;      //Mail,   Codigo

    // Constructors

    /**
     * Construtor de um CatalogoTransportador.
     */
    public CatalogoTransportador() {
        this.transportadores = new HashMap<>();
        this.credenciais     = new HashMap<>();
    }

    /**
     * Construtor de um CatalogoTransportador.
     * @param transportadores Transportadores do catálogo a construir.
     * @param credenciais Credenciais dos transportadores do catálogo.
     */
    public CatalogoTransportador(Map<String, Transportador> transportadores, Map<String,String> credenciais) {
        this.setTransportadores(transportadores);
        this.setCredenciais(credenciais);
    }

    /**
     * Construtor por cópia de um CatálogoTransportador.
     * @param c CatalogoTransportador a copiar.
     */
    public CatalogoTransportador(CatalogoTransportador c) {
        this.transportadores = c.getTransportadores();
        this.credenciais     = c.getCredenciais();
    }

    // Get

    /**
     * Função que devolve os Transportadores de um CatalogoTransportador.
     * @return Map com os Transportadores do Catálogo.
     */
    private Map<String, Transportador> getTransportadores() {
        return this.transportadores.entrySet()
                                   .stream()
                                   .collect(Collectors.toMap(Map.Entry::getKey,
                                                             v -> v.getValue().clone()));
    }

    /**
     * Função que devolve as credenciais dos Transportadores do CatálogoTransportador.
     * @return Map com as credenciais dos transportadores do catálogo.
     */
    private Map<String, String> getCredenciais() {
        return this.credenciais.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                                          Map.Entry::getValue));
    }

    // Set

    /**
     * Função que modifica os transportadores do CatálogoTransportador.
     * @param t Novos transportadores.
     */
    private void setTransportadores(Map<String, Transportador> t) {
        this.transportadores = t.entrySet()
                                .stream()
                                .collect(Collectors.toMap(Map.Entry::getKey,
                                                          p -> p.getValue().clone()));
    }

    /**
     * Função que modifica as credenciais dos transportadores presentes no Catálogo.
     * @param credenciais Novas credenciais.
     */
    private void setCredenciais(Map<String, String> credenciais) {
        this.credenciais = credenciais.entrySet()
                           .stream()
                           .collect(Collectors.toMap(Map.Entry::getKey,
                                                     Map.Entry::getValue));
    }

    //

    /**
     * Função que cria um clone de um CatálogoTransportador.
     * @return CatalogoTransportador clonado.
     */
    public CatalogoTransportador clone() {
        return new CatalogoTransportador(this);
    }

    /**
     * Função que compara CatalogoTransportador.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        CatalogoTransportador ct = (CatalogoTransportador) o;
        return this.transportadores.equals(ct.getTransportadores()) &&
               this.credenciais.equals(ct.getCredenciais());
    }

    /**
     * Função que converte os parâmetros de um CatalogoTransportador em String.
     * @return String com os parâmetros de um CatalogoTransportador.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        this.transportadores.values().forEach(v -> sb.append(v).append("\n"));
        return sb.toString();
    }

    //

    /**
     * Função que adiciona um Transportador, assim como as suas credenciais, ao CatalogoTransportador.
     * @param t Transportador a adicionar.
     */
    public void add(Transportador t) {
        this.transportadores.put(t.getCodigo(), t.clone());
        this.credenciais.putIfAbsent(t.getMail(), t.getCodigo());
    }

    /**
     * Função que devolve um Transportador dado o seu código.
     * @param codigo Código do Transportador.
     * @return Transportador correspondente ao código dado.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    private Transportador getTransportador(String codigo) throws TransportadorNotFoundException{
        Transportador t = this.transportadores.get(codigo);
        if(t == null) throw new TransportadorNotFoundException();
        return t;
    }

    /**
     * Função que verifica se a Password correspondente a um Mail bate certo.
     * @param mail Mail a pesquisar.
     * @param password Password a verificar.
     * @return true se coincidir.
     * @throws MailNotRegisteredException Quando o Mail não estiver registado.
     */
    public boolean checkPassword(String mail, String password) throws MailNotRegisteredException {
        String v = this.credenciais.get(mail);
        if(v == null) throw new MailNotRegisteredException("O mail não está associado a nenhum voluntário!");
        return this.transportadores.get(v).checkPassword(password);
    }

    /**
     * Função que gera um código.
     * @return String com o código gerado.
     */
    public String codeGenerator(){
        String s = null;
        while(s == null || this.transportadores.containsKey(s)){
            StringBuilder sb = new StringBuilder();
            s = sb.append('t')
                  .append(ThreadLocalRandom.current()
                                           .nextInt(1,1000))
                  .toString();
        }
        return s;
    }

    /**
     * Função que regista um Voluntário.
     * @param nome Nome do Voluntário.
     * @param gps Localização do Voluntário.
     * @param mail Mail do Voluntário.
     * @param password Password do Voluntário.
     * @return Código do Voluntário.
     * @throws MailAlreadyRegisteredException Quando o Mail já estiver registado.
     */
    public String registaVoluntario(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException {
        if(this.credenciais.containsKey(mail)) throw new MailAlreadyRegisteredException();
        String codigo = this.codeGenerator();
        this.add(new Voluntario(codigo, nome, mail, password, gps));
        return codigo;
    }

    /**
     * Função que regista uma Empresa.
     * @param nome Nome da Empresa.
     * @param gps Localização da Empresa.
     * @param mail Mail da Empresa.
     * @param password Password da Empresa.
     * @return Código da Empresa.
     * @throws MailAlreadyRegisteredException Quando o Mail já estiver registado.
     */
    public String registaEmpresa(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException {
        if(this.credenciais.containsKey(mail)) throw new MailAlreadyRegisteredException();
        String codigo = this.codeGenerator();
        this.add(new Empresa(codigo, nome, mail, password, gps));
        return codigo;
    }

    /**
     * Função que devolve um Transportador a partir das suas credenciais.
     * @param mail Mail do Transportador.
     * @param password Password do Trasportador.
     * @return Transportador.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    private Transportador getFromMailAndPassword(String mail, String password) throws CredenciaisErradasException {
        boolean correto;
        try { correto = this.checkPassword(mail,password); }
        catch(MailNotRegisteredException e) { throw new CredenciaisErradasException(); }
        if(!correto) throw new CredenciaisErradasException();
        return this.transportadores.get(this.credenciais.get(mail));
    }

    /**
     * Função que devolve o código de um Voluntário a partir das suas credenciais.
     * @param mail Mail do Voluntário.
     * @param password Password do Voluntário.
     * @return Código do Voluntário.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getVoluntarioFromMailAndPassword(String mail, String password) throws CredenciaisErradasException{
        Transportador t = this.getFromMailAndPassword(mail,password);
        if(t instanceof Voluntario) return t.getCodigo();
        else throw new CredenciaisErradasException();
    }

    /**
     * Função que devolve o código de uma Empresa a partir das suas credenciais.
     * @param mail Mail da Empresa.
     * @param password Password da Empresa.
     * @return Código da Empresa.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getEmpresaFromMailAndPassword(String mail, String password) throws CredenciaisErradasException{
        Transportador t = this.getFromMailAndPassword(mail,password);
        if(t instanceof Empresa) return t.getCodigo();
        else throw new CredenciaisErradasException();
    }

    /**
     * Função que altera a disponibilidade de um Transportador.
     * @param disp Nova disponibilidade.
     * @param codigo Código do Transportador.
     * @return disponibilidade.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public boolean alterarDisponibilidade(boolean disp, String codigo) throws TransportadorNotFoundException{
        Transportador v = this.getTransportador(codigo);
        boolean anterior = v.getAvailable();
        v.setAvailable(disp);
        return anterior;
    }

    /**
     * Função que devolve a Localização de um Transportador.
     * @param codigo Código do Transportador.
     * @return Localização do Transportador.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public Localizacao getGPS(String codigo) throws TransportadorNotFoundException{
        return this.getTransportador(codigo).getLocalizacao();
    }

    /**
     * Função que devolve o raio de um Transportador.
     * @param codigo Código do Transportador.
     * @return Raio do Transportador.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public double getRaio(String codigo) throws TransportadorNotFoundException{
        return this.getTransportador(codigo).getRaio();
    }

    /**
     * Função que atribui uma entrega a um Transportador.
     * @param codigo Código do Transportador.
     * @param pe PossibilidadeEntrga do transportador.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public void entregar(String codigo, PossibilidadeEntrega pe) throws TransportadorNotFoundException{
        this.getTransportador(codigo).entregar(pe);
    }

    /**
     * Função que sucede uma entrega de um Transportador.
     * @param codigo Código do Transportador.
     * @param tempo Tempo da entrega.
     * @return InfoHistorico com a informação da encomenda entregue.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválido ou não foi encontrada.
     */
    public InfoHistorico entregue(String codigo, double tempo) throws TransportadorNotFoundException, EncomendaNotFoundException {
        return this.getTransportador(codigo).entregue(tempo);
    }

    /**
     * Função que diz se aceita encomendas médicas.
     * @param codigo Código do Transportador.
     * @return true se aceitar.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public boolean isCertificado(String codigo) throws TransportadorNotFoundException{
        return this.getTransportador(codigo).aceitoTransporteMedicamentos();
    }

    /**
     * Devolve o tipo de Transportador.
     * @param codigo Código do Transportador.
     * @return Empresa ou Voluntário.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public Tipo tipoTransportador(String codigo) throws TransportadorNotFoundException{
        return this.getTransportador(codigo) instanceof Voluntario ? Tipo.VOLUNTARIO : Tipo.EMPRESA;
    }

    /**
     * Função que devolve a disponibilidade de um Transportador.
     * @param codigo Código do Transportador.
     * @return true se estiver disponível.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public boolean getDisponibilidade(String codigo) throws TransportadorNotFoundException{
        return this.getTransportador(codigo).getAvailable();
    }

    /**
     * Função que devolve o top10 empresas por distância percorrida.
     * @return top10 empresas por distância percorrida.
     */
    public List<String> listagemTop10Empresas(){
        return this.transportadores.values()
                                   .stream()
                                   .filter(Empresa.class::isInstance)
                                   .map(e -> (Empresa) e.clone())
                                   .collect(Collectors.toMap(Empresa::getCodigo,
                                                             Empresa::getDistanciaTotal))
                                   .entrySet()
                                   .stream()
                                   //Comparar double reversed e em caso de iguais, String natural
                                   .sorted(Map.Entry.<String,Double>comparingByValue(Comparator.reverseOrder())
                                                    .thenComparing(Map.Entry.comparingByKey()))
                                   .limit(10)
                                   .map(e -> String.format("%s %.2f",e.getKey(),e.getValue()))
                                   .collect(Collectors.toList());
    }

    /**
     * Função que devolve o total faturado por um Transportador num intervalo de tempo.
     * @param codigo Código do transportador.
     * @param inf Limite inferior.
     * @param sup Limite superior.
     * @return total faturado por um Transportador num intervalo de tempo.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     * @throws OrdemCronologicaErradaException Quando a ordem cronológica estiver incorreta.
     */
    public double getFaturadoIntervalo(String codigo, LocalDateTime inf, LocalDateTime sup) throws EmpresaNotFoundException, OrdemCronologicaErradaException {
        Transportador t = this.transportadores.get(codigo);
        if (t == null)
            throw new EmpresaNotFoundException(codigo);
        if (t instanceof Empresa)
            return ((Empresa) t).getFaturadoIntervalo(inf,sup);
        throw new EmpresaNotFoundException(codigo);
    }

    /**
     * Função que altera o mail de um Voluntário.
     * @param cod Código do Voluntário.
     * @param newMail Novo mail.
     * @throws VoluntarioNotFoundException Quando o voluntário é inválido ou não foi encontrado.
     */
    public void alterarMailVoluntario(String cod, String newMail) throws VoluntarioNotFoundException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new VoluntarioNotFoundException(cod);
        if (t instanceof Voluntario){
            String mail = ((Voluntario) t).getMail();
            ((Voluntario) t).setMail(newMail);
            this.credenciais.remove(mail);
            this.credenciais.put(newMail,cod);
        }
        else throw new VoluntarioNotFoundException(cod);
    }

    /**
     * Função que altera a password de um Voluntário.
     * @param cod Código do Voluntário.
     * @param newPass Nova Password.
     * @throws VoluntarioNotFoundException Quando o voluntário é inválido ou não foi encontrado.
     */
    public void alterarPasswordVoluntario(String cod, String newPass) throws VoluntarioNotFoundException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new VoluntarioNotFoundException(cod);
        if (t instanceof Voluntario) ((Voluntario) t).setPassword(newPass);
        else throw new VoluntarioNotFoundException(cod);
    }

    /**
     * Função que altera o raio de um Voluntário.
     * @param cod Código do Voluntário.
     * @param raio Novo raio.
     * @throws VoluntarioNotFoundException Quando o voluntário é inválido ou não foi encontrado.
     * @throws NotPositiveNumberException Quando o raio for menor que 0.
     */
    public void alterarRaioVoluntario(String cod, double raio) throws VoluntarioNotFoundException, NotPositiveNumberException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new VoluntarioNotFoundException(cod);
        if (t instanceof Voluntario) t.setRaio(raio);
        else throw new VoluntarioNotFoundException(cod);
    }

    /**
     * Função que altera o mail de uma Empresa.
     * @param cod Código da Empresa.
     * @param newMail Novo mail.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     */
    public void alterarMailEmpresa(String cod, String newMail) throws EmpresaNotFoundException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa){
            String mail = ((Empresa) t).getMail();
            ((Empresa) t).setMail(newMail);
            this.credenciais.remove(mail);
            this.credenciais.put(newMail,cod);
        }
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que altera a password de uma Empresa.
     * @param cod Código da Empresa.
     * @param newPass Nova Password.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     */
    public void alterarPasswordEmpresa(String cod, String newPass) throws EmpresaNotFoundException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa) ((Empresa) t).setPassword(newPass);
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que altera o nif de uma Empresa.
     * @param cod Código da Empresa.
     * @param nif Novo Nif.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     * @throws NotPositiveNumberException Quando o nif é menor que 0.
     */
    public void alterarNifEmpresa(String cod, int nif) throws EmpresaNotFoundException, NotPositiveNumberException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa) ((Empresa) t).setNif(nif);
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que alter o raio de uma Empresa.
     * @param cod Código da Empresa.
     * @param raio Novo Raio.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     * @throws NotPositiveNumberException Qunado o raio é menor que 0.
     */
    public void alterarRaioEmpresa(String cod, double raio) throws EmpresaNotFoundException, NotPositiveNumberException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa) t.setRaio(raio);
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que faz um Transportador aceitar uma entrega.
     * @param transportador Código do Transportador.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválida ou não foi encontrada.
     */
    public void aceitar(String transportador) throws TransportadorNotFoundException, EncomendaNotFoundException {
        this.getTransportador(transportador).aceitar();
    }

    /**
     * Função que faz um Transportador recusar uma entrega.
     * @param transportador Código do Transportador.
     * @return PossibilidadeEntrega.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválida ou não foi encontrada.
     */
    public PossibilidadeEntrega recusar(String transportador) throws TransportadorNotFoundException, EncomendaNotFoundException {
        return this.getTransportador(transportador).recusar();
    }

    /**
     * Função que retorna o estado de entrega de um Transportador.
     * @param transportador Código do Transportador.
     * @return Estado da entrega de um transportador.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválida ou não foi encontrada.
     */
    public Estado getEstadoEntrega(String transportador) throws TransportadorNotFoundException, EncomendaNotFoundException {
        return this.getTransportador(transportador).getEstadoEntrega();
    }

    /**
     * Função que devolve o código da Encomenda da entrega de um Transportador.
     * @param transportador Código do Transportador.
     * @return Código da Encomenda.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválida ou não foi encontrada.
     */
    public String getCodigoEncomendaTransportador(String transportador) throws TransportadorNotFoundException, EncomendaNotFoundException {
        return this.getTransportador(transportador).getCodigoEncomenda();
    }

    /**
     * Função que altera o preço por km de uma Empresa.
     * @param cod Código da Empresa.
     * @param precoPorKm Novo preço por km.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     * @throws NotPositiveNumberException Quando o preço for menor que 0.
     */
    public void alterarPrecoPorKmEmpresa(String cod, double precoPorKm) throws EmpresaNotFoundException, NotPositiveNumberException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa) ((Empresa) t).setPrecoPorKm(precoPorKm);
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que altera o preço por peso de uma Empresa.
     * @param cod Código da Empresa.
     * @param precoPorPeso Novo preço por peso.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     * @throws NotPositiveNumberException Quando o preço for menor que 0.
     */
    public void alterarPrecoPorPesoEmpresa(String cod, double precoPorPeso) throws EmpresaNotFoundException, NotPositiveNumberException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa) ((Empresa) t).setPrecoPorPeso(precoPorPeso);
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que altera o preço por hora de uma Empresa.
     * @param cod Código da Empresa.
     * @param precoPorHora Novo preço por hora.
     * @throws EmpresaNotFoundException Quando a empresa é inválida ou não foi encontrada.
     * @throws NotPositiveNumberException Quando o preço for menor que 0.
     */
    public void alterarPrecoPorHoraEmpresa(String cod, double precoPorHora) throws EmpresaNotFoundException, NotPositiveNumberException {
        Transportador t = this.transportadores.get(cod);
        if (t == null) throw new EmpresaNotFoundException(cod);
        if (t instanceof Empresa) ((Empresa) t).setPrecoPorHora(precoPorHora);
        else throw new EmpresaNotFoundException(cod);
    }

    /**
     * Função que altera o estado de aceitação de encomendas médicas.
     * @param cod Código do Transportador.
     * @param state Novo estado de aceitação.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public void alterarCertificado(String cod, boolean state) throws TransportadorNotFoundException {
        this.getTransportador(cod).aceitaMedicamentos(state);
    }

    /**
     * Função que devolve o custo de um Transportador.
     * @param transportador Código do Transportador.
     * @param peso Peso da Encomenda.
     * @param distancia Distância da entrega.
     * @param tempo Tempo de entrega.
     * @return Custo do Transportdor.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public double getCusto(String transportador,double peso, double distancia, double tempo) throws TransportadorNotFoundException{
        return this.getTransportador(transportador).getCusto(peso, distancia, tempo);
    }

    /**
     * Função que classifica a entrega pela parte do Trnsportador.
     * @param transportador Código do Transportador.
     * @param encomenda Código da encomenda entregue.
     * @param classificacao Classificação atribuida.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     * @throws EncomendaNotFoundException Quando a encomenda é inválida ou não foi encontrada.
     * @throws ClassificacaoInvalidaException Quando a classificação não está entre 1 e 10, inclusive.
     */
    public void classificar(String transportador, String encomenda, int classificacao) throws TransportadorNotFoundException,
                                                                                              EncomendaNotFoundException,
                                                                                              ClassificacaoInvalidaException {
        this.getTransportador(transportador).classificar(encomenda,classificacao);
    }

    /**
     * Função que devolve a classificação média de um Transportador.
     * @param transportador Código do transportador.
     * @return Classificação média do Transportador.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public double classificacaoMedia(String transportador) throws TransportadorNotFoundException {
        return this.getTransportador(transportador).classificacaoMedia();
    }

    /**
     * Função que dado um código devolve os parâmetros do transportador correspondente.
     * @param codigo Código do transportador.
     * @return String com os parâmetros do transportador correspondente.
     * @throws TransportadorNotFoundException Quando o transportador é inválido ou não foi encontrado.
     */
    public String getDadosTransportador(String codigo) throws TransportadorNotFoundException {
        return this.getTransportador(codigo).toString();
    }

    /**
     * Devolve informação relativa às encomendas tranportadas pelo transportador.
     * @param transportador Código do tranportador.
     * @return Estrutura com informação sobre as encomendas transportadas.
     * @throws TransportadorNotFoundException Quando o Utilizador é inválido ou não for encontrado.
     */
    public List<String> encomendasTransportadas(String transportador) throws TransportadorNotFoundException{
        return this.getTransportador(transportador).encomendasTransportadas();
    }
}
