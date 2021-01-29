package model;

import java.util.List;

public class EncomendaMedica extends Encomenda {

    public EncomendaMedica(){
        super();
    }

    public EncomendaMedica(String codEncomenda, String codUtilizador, String codLoja, double peso, List<Linha_Encomenda> linhas) {
        super(codEncomenda,codUtilizador,codLoja,peso,linhas);
    }
}
