import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Voluntario extends User implements Serializable {

    // variáveis de instância
    private double classificacao;
    private int nclass;
    private boolean disponivel;
    private double raio_acao;
    private double velocidade;
    private boolean verificado;
    private List<Encomenda> entregas_feitas;

    private Voluntario(){
    }

    public Voluntario(String username,String nome, Coordenadas pos, double raio){
        super(username,nome,pos);
        this.raio_acao = raio;
        this.entregas_feitas = new ArrayList<>();
        this.nclass = 0;
        this.disponivel = true;
        this.classificacao = 0;
        this.velocidade = 0;
        this.verificado = false;

    }

    /**
     * Construtor parametrizado da classe Voluntario.
     * @param nome
     * @param user
     * @param pass
     * @param posicao
     * @param classi
     * @param disp
     * @param range
     * @param vel
     * @param veri
     * @param entregas_feitas1
     * @return
     */
    public Voluntario(String nome, String user, String pass, Coordenadas posicao, double classi,int cls, boolean disp, double range,double vel, boolean veri, List<Encomenda> entregas_feitas1){
        super(user,nome,pass,posicao);
        this.classificacao = classi;
        this.nclass = cls;
        this.disponivel = disp;
        this.raio_acao = range;
        this.velocidade = vel;
        this.verificado = veri;
        this.entregas_feitas = entregas_feitas1.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    /**
     * Construtor de cópia de Voluntario.
     * @param v
     * @return
     */
    public Voluntario(Voluntario v){
        super(v);
        this.classificacao = v.getClassificacao();
        this.disponivel = v.getDisponivel();
        this.raio_acao = v.getRaio_acao();
        this.velocidade = v.getVelocidade();
        this.verificado = v.getVerificado();
        setEntregas_feitas(v.getEntregas_feitas());
    }

    /**
     * Indica a classificacao do voluntario.
     * @param
     * @return classificacao
     */
    public double getClassificacao() {
        return classificacao;
    }

    /**
     * Permite definir a classificacao de um voluntario.
     * @param classificacao
     * @return
     */
    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public int getNclass() {
        return nclass;
    }

    public void setNclass(int nclass) {
        this.nclass = nclass;
    }

    /**
     * Indica se um voluntario esta disponivel.
     * @param
     * @return disponivel
     */
    public boolean getDisponivel(){
        return disponivel;
    }

    /**
     * Atualiza se um voluntario esta disponivel para entrega.
     * @param disponivel
     * @return
     */
    public void setDisponivel(boolean disponivel) {
        this.disponivel = disponivel;
    }

    /**
     * Indica o raio de acao do voluntario.
     * @param
     * @return raio_acao
     */
    public double getRaio_acao() {
        return raio_acao;
    }

    /**
     * Atualiza o raio de acao de um voluntario.
     * @param raio_acao
     * @return
     */
    public void setRaio_acao(double raio_acao) {
        this.raio_acao = raio_acao;
    }

    /**
     * Indica a velocidaide do voluntario.
     * @param
     * @return velocidade
     */
    public double getVelocidade() {
        return velocidade;
    }

    /**
     * Atualiza a velocidade do transportador.
     * @param velocidade
     * @return
     */
    public void setVelocidade(double velocidade) {
        this.velocidade = velocidade;
    }

    /**
     * Indica se o voluntario pode transportar material medico.
     * @param
     * @return verificado
     */
    public boolean getVerificado(){
        return verificado;
    }

    /**
     * Atualiza o estatuto do voluntario(se entrega voluntario medico ou nao).
     * @param verificado
     * @return
     */
    public void setVerificado(boolean verificado) {
        this.verificado = verificado;
    }

    /**
     * Permite obter a lista de entregas já finalizadas.
     * param
     * @return List<Encomenda> Entregas_feitas
     */
    public List<Encomenda> getEntregas_feitas() {
        List<Encomenda> novo = new ArrayList<Encomenda>();
        for (Encomenda x : this.entregas_feitas) {
            novo.add(x.clone());
        }
        return novo;
    }

    /**
     * Atualiza o array com as entregas feitas.
     * @param novo
     * @return
     */
    public void setEntregas_feitas(List<Encomenda> novo){
        this.entregas_feitas = novo.stream().map(Encomenda :: clone).collect(Collectors.toList());
    }

    public void addEnc(Encomenda enc){
        if(!this.entregas_feitas.contains(enc)) {
            this.entregas_feitas.add(enc.clone());
        }
    }


    public void updateClass(double rating){
        this.nclass += 1;
        this.classificacao += rating;
        setNclass(this.nclass);
        setClassificacao(this.classificacao / this.nclass);
    }

    /**
     * Método que faz uma cópia da classe Voluntario.
     * Para tal invoca o construtor de cópia.
     * @param
     * @return Voluntario clone da classe Voluntario
     */
    public Voluntario clone(){
        return new Voluntario(this);
    }

    /**
     * Método que devolve a representação em String da classe Voluntario.
     * @param
     * @return String
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ").append(super.getNome()).append("\n")
        .append("Posicao: \n").append(super.getPosicao())
        .append("\tClassificacao: ").append(this.getClassificacao()).append("\n")
        .append("\tDisponivel: ").append(this.getDisponivel()).append("\n")
        .append("\tRaio de acao: ").append(this.getRaio_acao()).append("\n")
        .append("\tVelocidade ").append(this.getVelocidade()).append("\n")
        .append("\tVerificado: ").append(this.getVerificado()).append("\n")
        .append("\tEntregas feitas: \n").append(this.entregas_feitas.toString()).append("\n");
        return sb.toString();
    }

    /**
     * Método que verifica se a classe Voluntario é igual à classe Voluntario.
     * @param o
     * @return boolean
     */
    public boolean equals(Object o){
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Voluntario v = (Voluntario) o;
        return (v.getUsername().equals(this.getUsername()));
    }

    public int hashCode(){
        long aux1, aux2,aux3;
        int hash = 5;
        hash = 31 * hash + super.hashCode();
        aux1 = 31 * hash + Double.doubleToLongBits(this.classificacao);
        hash = 31 * hash + (int)(aux1 ^ (aux1 >>> 32));
        hash = 31 * hash + Integer.hashCode(this.nclass);
        hash = 31 * hash + (this.disponivel ? 1 : 0);
        aux2 = 31 * hash + Double.doubleToLongBits(this.raio_acao);
        hash = 31 * hash + (int)(aux2 ^ (aux2 >>> 32));
        aux3 = 31 * hash + Double.doubleToLongBits(this.velocidade);
        hash = 31 * hash + (int)(aux3 ^ (aux3 >>> 32));
        hash = 31 * hash + (this.verificado ? 1 : 0);
        hash = 31*hash + this.entregas_feitas.stream().mapToInt(Encomenda::hashCode).sum();
        return hash;
    }
}
