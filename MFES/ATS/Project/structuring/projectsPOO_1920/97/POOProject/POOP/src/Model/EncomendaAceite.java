package Model;
import java.io.Serializable;

/**
 * Classe que representa as encomendas aceites
 */

public class EncomendaAceite extends Encomendas implements Serializable{
        private static final long serialVersionUID = 2509013083856169856L;
        private String codEncomenda;

        public EncomendaAceite(){
            this.codEncomenda = "";
        }

        public EncomendaAceite(String cod){
            this.codEncomenda = cod;
        }

        public EncomendaAceite(EncomendaAceite ea){
            this.codEncomenda = ea.codEncomenda;
        }
        

       /**
        * Getter do código de encomenda aceite
        */
        public String getCodEncomenda(){
            return this.codEncomenda;
        }
    
        public EncomendaAceite clone(){return new EncomendaAceite(this);}

        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            EncomendaAceite that = (EncomendaAceite) o;
            return this.codEncomenda.equals(that.codEncomenda);
        }

        public String toString()
        { StringBuilder sb = new StringBuilder();
            sb.append("Aceite:").append(this.codEncomenda);
            return sb.toString();
        }

        @Override
        public int hashCode() {
        return 1;
        }
}
