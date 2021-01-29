package app.models;

import java.time.LocalDateTime;
import java.util.List;

public class EncomendaMedica extends Encomenda {

    // #region variables

    // #endregion

    // #region Construtores

    /**
    *
    */
    private static final long serialVersionUID = 8460419585663954841L;

    public EncomendaMedica() {
        super();

    }

    /**
     * 
     * @param codEnc
     * 
     * @param codUtilizador
     * 
     * @param codLoja
     * 
     * @param peso
     * 
     * @param dataCriacao
     * 
     * @param linhasEncomenda
     * 
     */

    public EncomendaMedica(String codEnc, String codUtilizador, String codLoja, double peso,
            LocalDateTime dataCriacao,

            List<LinhaEncomenda> linhasEncomenda) {
        super(codEnc, codUtilizador, codLoja, peso, dataCriacao, linhasEncomenda);

    }

    /**
     * 
     * @param e
     * 
     */

    public EncomendaMedica(EncomendaMedica e) {
        super(e);

    }

    // #endregion

    // #region Overrrides

    @Override
    public EncomendaMedica clone() {

        return new EncomendaMedica(this);

    }

    // #endregion

    // #region Methods
    // #endregion

}
