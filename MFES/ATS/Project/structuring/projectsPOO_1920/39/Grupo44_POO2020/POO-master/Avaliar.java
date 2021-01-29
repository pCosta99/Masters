public abstract class Avaliar {
    /** Avaliação **/
    private double avaliacao;
    /** Numero de pessoas que avaliaram **/
    private int quantidade;

    /** Construtor nulo */
    public Avaliar(){
        this.avaliacao= 0.0;
        this.quantidade = 0;
    }

    /** Construtor parametrizado para a classe Avaliar */
    public Avaliar(double a, int vt){
        this.avaliacao = a;
        this.quantidade = vt;
    }

    /** Construtor de cópia */
    public Avaliar(Avaliar av){
        this.avaliacao= av.getAvaliacao();
        this.quantidade= av.getQuantidade();
    }

    /** Retorna a avaliação **/
    public double getAvaliacao(){
        return this.avaliacao;
    }

    /** Define a avaliação **/
    public void setAvaliacao(double a){
        this.avaliacao = a;
    }

    /** Retorna a quantidade de votos  **/
    public int  getQuantidade(){
        return this.quantidade;
    }

    /** Define a quantidade de votos **/
    public void setQuantidade(int q){
        this.quantidade = q;
    }

    /** Método que clona um Registo */
    public abstract Avaliar clone();

    /** Método que devolve um boolean true caso os Registos sejam iguais e false caso não sejam */
    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        Avaliar r = (Avaliar) o;
        String a = String.valueOf(r.getAvaliacao());
        String v = String.valueOf(r.getQuantidade());
        String af = String.valueOf(this.avaliacao);
        String vf = String.valueOf(this.quantidade);

        return af.equals(a) &&
                vf.equals(r.getAvaliacao());
    }
    /** Método que cria uma string com a informação do Registo */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        String af = String.valueOf(this.avaliacao);
        String vf = String.valueOf(this.quantidade);
        sb.append("Avaliação: ").append(af+"\n");
        sb.append("Quantidade de votos: ").append(vf+"\n");
        return sb.toString();
    }
}
