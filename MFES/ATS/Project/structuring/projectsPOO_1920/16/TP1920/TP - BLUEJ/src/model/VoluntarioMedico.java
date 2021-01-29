package src.model;
import java.util.List;
/**
 * Classe que cria Voluntários Médicos
 */

public class VoluntarioMedico extends Voluntario {

    //Variáveis de Instância

    private boolean estouAAceitarMed;

    /**
     * Construtores da classe model.VoluntarioMedico.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.VoluntarioMedico.
     */

    public VoluntarioMedico(){
        super();
        this.estouAAceitarMed = false;
    }

    /**
     * Construtor parametrizado de model.VoluntarioMedico.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public VoluntarioMedico( String password, String nome, String email, double raio, Classificacao classificacao, boolean prontoARecolher, Ponto localizacao, List<Entrega> historicoEntregas, boolean estouAAceitarMed){
        super(password, nome,email, raio, classificacao, prontoARecolher, localizacao, historicoEntregas);
        this.estouAAceitarMed = estouAAceitarMed;
    }

    /**
     * Construtor de cópia de model.VoluntarioMedico.
     * Aceita como parâmetro outro model.VoluntarioMedico e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public VoluntarioMedico(VoluntarioMedico v){
        super(v);
        this.estouAAceitarMed = v.aceitoTransporteMedicamentos();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o estado interno de aceitação de encomendas de medicamentos por parte do model.VoluntarioMedico
     * @return boolean consoante o estado interno de aceitação de medicamentos
     */

    public boolean aceitoTransporteMedicamentos(){
        return this.estouAAceitarMed;
    }

    /**
     * Atualiza o estado interno de aceitação de encomendas de medicamentos por parte do model.VoluntarioMedico
     * @param state novo estado interno de aceitação de encomendas de medicamentos
     */

    public void aceitaMedicamentos(boolean state){
        this.estouAAceitarMed = state;
    }

    
    /**
     * Indica se uma encomenda pode ser transportada pelo model.VoluntarioMedico
     * @param e Encomenda a ser verificada se pode ou não ser transportada
     * @return boolean que indica se a encomenda verificada pode ou não ser transportada pelo model.VoluntarioMedico
    */

    public boolean podeTransportar(Encomenda e) {
        if (e.is_medicamento()){
            return this.recolhe && this.estouAAceitarMed;
        }

        return this.recolhe;
    }

    /**
     * Método que transforma um model.VoluntarioMedico numa String
     * @return String com toda a informação presente no model.VoluntarioMedico
     */

    public String toString() {
        String s = super.toString();
        final StringBuilder sb = new StringBuilder("model.VoluntarioMedico{");
        sb.append("estouAAceitarMed=").append(this.estouAAceitarMed);
        sb.append('}');
        return s + sb.toString();
    }

    /**
     * Método que determina se um model.VoluntarioMedico e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso o model.VoluntarioMedico e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        VoluntarioMedico v = (VoluntarioMedico) o;
        return this.estouAAceitarMed == v.aceitoTransporteMedicamentos();
    }

    /**
     * Método que clona um Voluntário Médico, para tal é usado o construtor de cópia
     * @return Objeto model.VoluntarioMedico que é uma cópia do model.VoluntarioMedico
     */

    public Voluntario clone() {
        return new VoluntarioMedico(this);
    }


}
