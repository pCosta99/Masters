package src.model;

import java.util.List;

/**
 * Classe que cria Transportadoras Médicas
 */

public class TransportadoraMedica extends Transportadora {

    //Variáveis de Instância

    private boolean estamosAAceitarMed;

    /**
     * Construtores da classe model.TransportadoraMedica.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.TransportadoraMedica.
     */

    public TransportadoraMedica(){
        super();
        this.estamosAAceitarMed = false;
    }

    /**
     * Construtor parametrizado de model.TransportadoraMedica.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public TransportadoraMedica(String password, String nome, String email, boolean estamosARecolher, double precoPorKm, double precoPorMin, List<Entrega> historicoEntregas, boolean estamosAAceitarMed, Classificacao classif, double raio, Ponto localizacao  ){
        super(password, nome,email,estamosARecolher,precoPorKm,precoPorMin,historicoEntregas,classif,raio,localizacao);
        this.estamosAAceitarMed = estamosAAceitarMed;
    }

    /**
     * Construtor de cópia de model.TransportadoraMedica.
     * Aceita como parâmetro outra model.TransportadoraMedica e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public TransportadoraMedica(TransportadoraMedica t){
        super(t);
        this.estamosAAceitarMed = t.aceitoTransporteMedicamentos();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o estado interno de aceitação de encomendas de medicamentos por parte da model.TransportadoraMedica
     * @return boolean consoante o estado interno de aceitação de medicamentos
     */

    public boolean aceitoTransporteMedicamentos(){
        return this.estamosAAceitarMed;
    }

    /**
     * Atualiza o estado interno de aceitação de encomendas de medicamentos por parte da model.TransportadoraMedica
     * @param state novo estado interno de aceitação de encomendas de medicamentos
     */

    public void aceitaMedicamentos(boolean state){
        this.estamosAAceitarMed = state;
    }

    /**
     * Método que determina se a transportadora pode transportar medicamentos
     * @return boolean consoante o estado se pode ou não transportar a encomenda independentemento do tipo
     */

    public boolean podeTransportar(Encomenda e) {
        if (e.is_medicamento()){
            return this.recolhe && this.estamosAAceitarMed;
        }

        return this.recolhe;
    }

    /**
     * Método que determina se uma model.TransportadoraMedica e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso a model.TransportadoraMedica e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        TransportadoraMedica t = (TransportadoraMedica) o;
        return this.estamosAAceitarMed == t.aceitoTransporteMedicamentos();
    }

    /**
     * Método que transforma uma model.TransportadoraMedica numa String
     * @return String com toda a informação presente na model.TransportadoraMedica
     */

    public String toString() {
        String s = super.toString();
        final StringBuilder sb = new StringBuilder("model.TransportadoraMedica{");
        sb.append("estamosAAceitarMed=").append(this.estamosAAceitarMed);
        sb.append('}');
        return s + sb.toString();
    }

    /**
     * Método que clona uma model.TransportadoraMedica, para tal é usado o construtor de cópia
     * @return Objeto model.TransportadoraMedica que é uma cópia da model.TransportadoraMedica
     */

    public TransportadoraMedica clone(){
        return new TransportadoraMedica(this);
    }


}
