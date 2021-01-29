import Exception.*;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

/**
 * Classe que lida com a informação de várias lojas.
 * 
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class CatalogoLojas implements Serializable {
    // Instance Variables
    private Map<String, Loja> lojas;
    private Map<String, String> credenciais;      //Mail,   Codigo

    // Constructors

    /**
     * Construtor de um CatalogoLojas.
     */
    public CatalogoLojas() {
        this.lojas       = new HashMap<>();
        this.credenciais = new HashMap<>();
    }

    /**
     * Construtor de um CatalogoLojas.
     * @param lojas Lojas do catálogo a construir.
     * @param credenciais Credenciais das Lojas do Catálogo a construir.
     */
    public CatalogoLojas(Map<String, Loja> lojas, Map<String,String> credenciais) {
        this.setLojas(lojas);
        this.setCredenciais(credenciais);
    }

    /**
     * Construtor de um CatalogoLojas por cópia.
     * @param c CatalogoLojas a copiar.
     */
    public CatalogoLojas(CatalogoLojas c) {
        this.lojas       = c.getLojas();
        this.credenciais = c.getCredenciais();
    }

    // Get

    /**
     * Função que devolve as Lojas de um CatálogoLojas.
     * @return Map com as Lojas do Catálogo.
     */
    private Map<String, Loja> getLojas() {
        return this.lojas.entrySet()
                         .stream()
                         .collect(Collectors.toMap(Map.Entry::getKey,
                                                   v -> v.getValue().clone()));
    }

    /**
     * Função que devolve as credenciais das Lojas do Catálogo.
     * @return Map com as credenciais das Lojas.
     */
    private Map<String, String> getCredenciais() {
        return this.credenciais.entrySet()
                                .stream()
                                .collect(Collectors.toMap(Map.Entry::getKey,
                                                          Map.Entry::getValue));
    }

    // Set

    /**
     * Função que modifica as Lojas de um CatálogoLojas.
     * @param l Novas Lojas.
     */
    private void setLojas(Map<String, Loja> l) {
        this.lojas = l.entrySet()
                      .stream()
                      .collect(Collectors.toMap(Map.Entry::getKey,
                                                v -> v.getValue().clone()));
    }

    /**
     * Função que modifica as credenciais das Lojas do Catálogo.
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
     * Função que cria um clone de um CatálogoLojas.
     * @return CatálogoLojas clonado.
     */
    public CatalogoLojas clone() {
        return new CatalogoLojas(this);
    }

    /**
     * Função que compara CatalogoLojas.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        CatalogoLojas cl = (CatalogoLojas) o;
        return this.lojas.equals(cl.getLojas()) && 
               this.credenciais.equals(cl.getCredenciais());
    }

    /**
     * Função que converte os parâmetros de um CatalogoLojas em String.
     * @return String com os parâmetros de um CatalogoLojas.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        this.lojas.values().forEach(l -> sb.append(l).append("\n"));
        return sb.toString();
    }

    //

    /**
     * Função que adiciona uma Loja, assim como as suas credenciais, ao CatalogoLojas.
     * @param l Loja a adicionar.
     */
    public void add(Loja l) {
        this.lojas.put(l.getCodigo(), l.clone());
        this.credenciais.put(l.getMail(), l.getCodigo());
    }

    /**
     * Função que verifica se a Password correspondente a um Mail bate certo.
     * @param mail Mail a pesquisar.
     * @param password Password a verificar.
     * @return true se coincidir.
     * @throws MailNotRegisteredException Quando o Mail não estiver registado.
     */
    public boolean checkPassword(String mail, String password) throws MailNotRegisteredException {
        String l = this.credenciais.get(mail);
        if(l == null) throw new MailNotRegisteredException("O mail não está associado a nenhuma loja!");
        return this.lojas.get(l).checkPassword(password);
    }

    /**
     * Função que gera um código.
     * @return String com o código gerado.
     */
    public String codeGenerator(){
        String s = null;
        while(s == null || this.lojas.containsKey(s)){
            StringBuilder sb = new StringBuilder();
            s = sb.append('l')
                    .append(ThreadLocalRandom.current()
                            .nextInt(1,1000))
                    .toString();
        }
        return s;
    }

    /**
     * Função que regista uma Loja.
     * @param nome Nome da Loja.
     * @param gps Localização da Loja.
     * @param mail Mail da Loja.
     * @param password Password da Loja.
     * @return Código da Loja.
     * @throws MailAlreadyRegisteredException Quando o Mail já estiver registado.
     */
    public String regista(String nome, Localizacao gps, String mail, String password) throws MailAlreadyRegisteredException {
        if(this.credenciais.containsKey(mail)) throw new MailAlreadyRegisteredException();
        String codigo = this.codeGenerator();
        this.add(new Loja(codigo, nome, mail, password, gps));
        return codigo;
    }

    /**
     * Função que devolve o código de uma Loja a partir das suas credenciais.
     * @param mail Mail da Loja.
     * @param password Password da Loja.
     * @return Código da Loja.
     * @throws CredenciaisErradasException Quando ou o Mail ou a Password estiverem incorretos.
     */
    public String getFromMailAndPassword(String mail, String password) throws CredenciaisErradasException {
        boolean correto;
        try { correto = this.checkPassword(mail,password); }
        catch(MailNotRegisteredException e) { throw new CredenciaisErradasException(); }
        if(!correto) throw new CredenciaisErradasException();
        return this.lojas.get(this.credenciais.get(mail))
                         .getCodigo();
    }

    /**
     * Função que verifica se um código está presente no Catálogo.
     * @param codigo Código a pesquisar.
     * @return true se estiver presente.
     */
    public boolean contains(String codigo){
        return this.lojas.containsKey(codigo);
    }

    /**
     * Função que devolve uma Loja a partir de um código.
     * @param codigo Código dado.
     * @return Loja correspondente ao código.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     */
    private Loja getLoja(String codigo) throws LojaNotFoundException{
        Loja l = this.lojas.get(codigo);
        if(l == null) throw new LojaNotFoundException(codigo);
        return l;
    }

    /**
     * Função que devolve a Localização de uma Loja, dado o seu código.
     * @param codigo Código da Loja.
     * @return Localização da Loja.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     */
    public Localizacao getGPS(String codigo) throws LojaNotFoundException{
        return this.getLoja(codigo).getLocalizacao();
    }

    /**
     * Função que incrementa o número de clientes de uma Loja.
     * @param codigo Código da Loja.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     */
    public void incCliente(String codigo) throws LojaNotFoundException{
        this.getLoja(codigo).incCliente();
    }

    /**
     * Função que decrementa o número de clientes de uma Loja.
     * @param codigo Código da Loja
     * @return Tempo total.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada..
     */
    public double decCliente(String codigo) throws LojaNotFoundException{
        Loja l = this.getLoja(codigo);
        double time = l.getTempoTotal();
        l.decCliente();
        return time;
    }

    /**
     * Função que devolve todos os códigos e nomes das lojas presentes no catálogo.
     * @return Lista com os códigos e nomes das lojas presentes no catálogo.
     */
    public List<String> getAllCodigoLoja(){
        return this.lojas.values()
                         .stream()
                         .map(l -> ("" + l.getCodigo() + "\t- " + l.getNome()))
                         .collect(Collectors.toList());
    }

    /**
     * Função que altera o Mail de uma Loja.
     * @param cod Código da Loja.
     * @param newMail Novo Mail.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     */
    public void alterarMailLoja(String cod, String newMail) throws LojaNotFoundException{
        Loja l = this.getLoja(cod);
        String mail = l.getMail();
        l.setMail(newMail);
        this.credenciais.remove(mail);
        this.credenciais.put(newMail,cod);
    }

    /**
     * Função que altera a Password de uma Loja.
     * @param cod Código da Loja.
     * @param newPass Nova Password.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     */
    public void alterarPasswordLoja(String cod, String newPass) throws LojaNotFoundException{
        this.getLoja(cod).setPassword(newPass);
    }

    /**
     * Função que modifica o tempo médio de uma Loja.
     * @param cod Código da Loja.
     * @param tempoMedio Novo Tempo Médio.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     * @throws NotPositiveNumberException Quando o número não for positivo.
     */
    public void alterarTempoMedioLoja(String cod, double tempoMedio) throws LojaNotFoundException, NotPositiveNumberException{
        this.getLoja(cod).setTempoMedio(tempoMedio);
    }

    /**
     * Função que converte os parâmetros de uma Loja em String.
     * @param codigo Código da Loja.
     * @return String com os parâmetros da Loja dada.
     * @throws LojaNotFoundException Quando a loja é inválida ou não for encontrada.
     */
    public String getDadosLoja(String codigo) throws LojaNotFoundException {
        return this.getLoja(codigo).toString();
    }
}
